# insert_data_many(), and the steps that it shares with
# DBTable_v9$insert_data().
#
# This name sorts after "util_database.R" and "util_database_load.R", so
# load_data_infile and db_postgres exist before loads_through_json() runs.
#
# Until 2026.10.13 a caller that filled several tables called $insert_data()
# once for each table. On PostgreSQL each call started its own psql process,
# and so its own psql login. A cs9 partitioned table therefore made one psql
# login for each partition in one insert. On 2026-10-12 parallel workers that
# did this used up the 100 connections of a token proxy. From 2026.10.17 the
# PostgreSQL load starts no psql and makes no login.

#' Insert data into several tables in one transaction
#'
#' @description
#' Inserts one data.table into each table of a list. On PostgreSQL, one
#' transaction loads every table, so by default the call stores every row or
#' no row. The one exception is a duplicate key with
#' \code{confirm_insert_via_nrow = TRUE}: csdb then inserts table by table.
#'
#' @details
#' On PostgreSQL, one transaction on the connection of the first table loads
#' every table. csdb sends the rows as JSON text, at most 500,000 rows in one
#' statement, and starts no client. A table whose data.table is \code{NULL} or
#' has no rows gets no statement. After each statement, csdb compares the
#' stored rows with the rows sent. A difference raises an error before the
#' commit, so the server stores no row in any table.
#'
#' The transaction holds a lock on every table until it commits, so a
#' \code{TRUNCATE} or an index change on one of them waits. The transaction
#' waits at most 60 s for one lock. After a longer wait, a deadlock or a
#' serialization failure, csdb runs the whole load again, at most 3 attempts
#' in total. A load whose connection breaks, or whose COMMIT fails, does not
#' run again. The error then says to count the rows first.
#'
#' A duplicate key stops the load. With
#' \code{confirm_insert_via_nrow = TRUE}, csdb then calls
#' \code{$insert_data(confirm_insert_via_nrow = TRUE)} on each table, which
#' upserts a table that stops on the duplicate key.
#'
#' With another driver, or with tables that have different connection settings,
#' csdb calls \code{$insert_data()} on each table in turn.
#' @param tables A list of \code{DBTable_v9} objects.
#' @param newdata A list of data.tables, one for each table, in the order of
#'   \code{tables}.
#' @param confirm_insert_via_nrow See \code{DBTable_v9$insert_data()}.
#' @param verbose See \code{DBTable_v9$insert_data()}.
#' @param load_timeout The longest time in seconds that the load MAY run. On
#'   PostgreSQL, the server cancels the statement that runs past this time and
#'   stores no row, and the error has class \code{csdb_load_other}.
#' @return \code{NULL}, invisibly.
#' @examples
#' \donttest{
#' # SQLite has no JSON load, so csdb inserts into each table in turn here.
#' db_config <- list(driver = "SQLite", db = tempfile(fileext = ".sqlite"))
#' connection <- DBConnection_v9$new(driver = "SQLite", db = db_config$db)
#' tables <- lapply(c("cases_a", "cases_b"), function(name) {
#'   DBTable_v9$new(
#'     dbconfig = db_config,
#'     table_name = name,
#'     field_types = c("id" = "INTEGER", "n" = "INTEGER"),
#'     keys = "id",
#'     dbconnection = connection
#'   )
#' })
#' insert_data_many(
#'   tables,
#'   list(
#'     data.table::data.table(id = 1:2, n = c(5L, 7L)),
#'     data.table::data.table(id = 1L, n = 3L)
#'   )
#' )
#' tables[[1]]$nrow()
#' connection$disconnect()
#' }
#' @export
insert_data_many <- function(
  tables,
  newdata,
  confirm_insert_via_nrow = FALSE,
  verbose = TRUE,
  load_timeout = 3600
) {
  check_insert_many_arguments(tables, newdata)
  if (length(tables) == 0L) {
    return(invisible(NULL))
  }
  if (!insert_session_possible(tables)) {
    return(insert_each(
      tables,
      newdata,
      confirm_insert_via_nrow,
      verbose,
      load_timeout
    ))
  }

  # The first steps of $insert_data() for every table, before any load.
  to_load <- vapply(
    seq_along(tables),
    function(i) {
      return(insert_prepare(
        tables[[i]],
        table_private(tables[[i]]),
        newdata[[i]]
      ))
    },
    logical(1)
  )
  tables <- tables[to_load]
  newdata <- newdata[to_load]
  if (length(tables) == 0L) {
    return(invisible(NULL))
  }

  duplicate <- if (confirm_insert_via_nrow) {
    tryCatch(
      insert_session(tables, newdata, load_timeout),
      csdb_load_duplicate_key = function(e) e
    )
  } else {
    insert_session(tables, newdata, load_timeout)
  }

  if (!is.null(duplicate)) {
    message(
      "csdb: insert_data_many(confirm_insert_via_nrow = TRUE): ",
      duplicate$tool,
      " failed on attempt ",
      duplicate$attempts,
      " (duplicate key), so the server stored no row in any of the ",
      length(tables),
      " tables. It inserts into each table on its own instead, and upserts",
      " a table that stops on a duplicate key."
    )
    return(insert_each(tables, newdata, TRUE, verbose, load_timeout))
  }

  if (confirm_insert_via_nrow) {
    for (i in seq_along(tables)) {
      insert_confirm(tables[[i]], newdata[[i]], NULL, verbose, load_timeout)
    }
  }
  return(invisible(NULL))
}

# Stop unless `tables` is a list of DBTable_v9 objects and `newdata` is a list
# with one element for each table.
check_insert_many_arguments <- function(tables, newdata) {
  if (
    !is.list(tables) ||
      !all(vapply(tables, inherits, logical(1), what = "DBTable_v9"))
  ) {
    stop("tables MUST be a list of DBTable_v9 objects.", call. = FALSE)
  }
  if (!is.list(newdata) || is.data.frame(newdata)) {
    stop("newdata MUST be a list of data.tables.", call. = FALSE)
  }
  if (length(newdata) != length(tables)) {
    stop(
      "newdata MUST hold one data.table for each table. It holds ",
      length(newdata),
      " for ",
      length(tables),
      " tables.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The per-table path: $insert_data() on each table in turn.
insert_each <- function(
  tables,
  newdata,
  confirm_insert_via_nrow,
  verbose,
  load_timeout
) {
  for (i in seq_along(tables)) {
    tables[[i]]$insert_data(
      newdata[[i]],
      confirm_insert_via_nrow = confirm_insert_via_nrow,
      verbose = verbose,
      load_timeout = load_timeout
    )
  }
  return(invisible(NULL))
}

# One transaction on the connection of the first table loads every table.
# The tables share one connection configuration.
insert_session <- function(tables, newdata, load_timeout) {
  first <- tables[[1]]
  json_load_tables(
    connection = load_connection(
      first$dbconnection,
      first$table_name_short_for_mssql_fully_specified_for_postgres
    ),
    dbconfig = first$dbconnection$config,
    tables = lapply(tables, function(x) {
      return(x$table_name_short_for_mssql_fully_specified_for_postgres)
    }),
    dts = newdata,
    load_timeout = load_timeout,
    lock_timeout = load_lock_timeout_seconds
  )
  return(NULL)
}

# TRUE when load_data_infile() dispatches to the PostgreSQL method for this
# connection, which is the JSON load.
loads_through_json <- function(connection) {
  found <- tryCatch(
    S7::method(load_data_infile, object = connection),
    error = function(e) NULL
  )
  return(identical(found, S7::method(load_data_infile, db_postgres)))
}

# One transaction can load the tables when they share one connection
# configuration and that connection loads through JSON.
insert_session_possible <- function(tables) {
  configs <- lapply(tables, function(x) x$dbconnection$config)
  same <- all(vapply(configs, identical, logical(1), configs[[1]]))
  first <- tables[[1]]
  return(
    same &&
      loads_through_json(load_connection(
        first$dbconnection,
        first$table_name_short_for_mssql_fully_specified_for_postgres
      ))
  )
}

# The private environment of a DBTable_v9 object. insert_data_many() runs the
# private steps of $insert_data() on each table, and R6 gives a function
# outside the class no other way to reach them.
table_private <- function(table) {
  return(table$.__enclos_env__$private)
}

# The steps of $insert_data() before the load. They create the table at its
# first use and run the validator of the field contents. FALSE means that
# `newdata` has no row to load.
insert_prepare <- function(self, private, newdata) {
  private$lazy_creation_of_table()
  if (is.null(newdata)) {
    return(FALSE)
  }
  if (nrow(newdata) == 0) {
    return(FALSE)
  }

  validated <- self$validator_field_contents(newdata)
  if (!validated) {
    stop(glue::glue(
      "load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"
    ))
  }
  return(TRUE)
}

# A new load file in the load folder of the table, named after the first row.
insert_load_file <- function(private, newdata) {
  return(random_file(
    private$load_folder_fn(),
    extra_insert = if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(newdata[1, ])
    } else {
      ""
    }
  ))
}

# The steps of $insert_data(confirm_insert_via_nrow = TRUE) after the load.
# `duplicate` is the csdb_load_duplicate_key error of the load, or NULL. After
# a duplicate key, the rows are upserted once. Otherwise the rows are upserted
# when the table holds fewer rows than `newdata`.
insert_confirm <- function(
  self,
  newdata,
  duplicate,
  verbose,
  load_timeout
) {
  if (is.null(duplicate)) {
    nrow_after <- self$nrow(use_count = TRUE)
    upsert <- nrow_after < nrow(newdata)
    if (upsert) {
      message(
        "After insert have ",
        nrow_after,
        " rows. Tried to insert ",
        nrow(newdata),
        ". Now trying upsert."
      )
    }
  } else {
    upsert <- TRUE
    message(
      "csdb: insert_data(confirm_insert_via_nrow = TRUE) on table ",
      self$table_name,
      ": ",
      duplicate$tool,
      " failed on attempt ",
      duplicate$attempts,
      " (duplicate key). It upserts the ",
      nrow(newdata),
      " rows once instead."
    )
  }
  if (upsert) {
    self$upsert_data(
      newdata = newdata,
      drop_indexes = NULL,
      verbose = verbose,
      load_timeout = load_timeout
    )
    nrow_after <- self$nrow(use_count = TRUE)
    if (nrow_after < nrow(newdata)) {
      message(
        "After upsert have ",
        nrow_after,
        " rows. Tried to upsert ",
        nrow(newdata),
        "."
      )
      stop("Upsert failed", call. = FALSE)
    }
  }
  return(invisible(NULL))
}
