# insert_data_many(), and the steps that it shares with
# DBTable_v9$insert_data().
#
# This name sorts after "util_database.R" and "util_database_load.R", so
# load_data_infile and db_postgres exist before loads_through_psql() runs.
#
# Until 2026.10.13 a caller that filled several tables called $insert_data()
# once for each table. On PostgreSQL each call started its own psql process,
# and so its own login. A cs9 partitioned table therefore made one login for
# each partition in one insert. On 2026-10-12 parallel workers that did this
# used up the 100 connections of a token proxy.

#' Insert data into several tables in one transaction
#'
#' @description
#' Inserts one data.table into each table of a list. On PostgreSQL, one
#' \code{psql} session loads every table in one transaction, so the call stores
#' every row or no row. The one exception is a duplicate key with
#' \code{confirm_insert_via_nrow = TRUE}: csdb then inserts table by table.
#'
#' @details
#' On PostgreSQL, csdb writes one load file for each table and starts
#' \code{psql} once. That \code{psql} runs a script of one \code{\\copy} for
#' each table, with \code{--single-transaction} and \code{-v ON_ERROR_STOP=1}.
#' The load therefore needs one \code{psql} login. \code{$insert_data()} on
#' each table needs one \code{psql} login for each table. A caller that records
#' the insert afterwards, as the last-updated bookkeeping of cs9 does, needs
#' one more login for that write.
#'
#' After each \code{\\copy}, the script compares the row count that \code{psql}
#' reports with the rows that csdb sent to that table. A difference raises an
#' error before the commit, so the server stores no row in any table. After
#' \code{psql} exits, csdb also compares each \code{COPY <n>} line with the
#' rows that it sent.
#'
#' The whole call is one transaction. Each \code{\\copy} takes a ROW EXCLUSIVE
#' lock on its table, and the transaction holds every one of these locks until
#' it commits. Reads and inserts of other keys into the same tables do not
#' wait. An insert of the same key from another connection waits until the
#' commit, and then stops on the duplicate key. A \code{TRUNCATE}, a
#' \code{DROP TABLE}, a \code{CREATE INDEX} or a \code{DROP INDEX} on one of
#' the tables waits until the commit.
#'
#' The script starts with \code{SET lock_timeout = '60s'}, so the session
#' waits at most 60 s for one lock. A longer wait stops the session with
#' SQLSTATE 55P03, which is the lock timeout below.
#'
#' The session uses the retry rules of \code{$insert_data()}:
#' \itemize{
#'   \item A failure while \code{psql} connects stores nothing, and csdb runs
#'     the whole session again. So do a deadlock, a serialization failure and
#'     a lock timeout, because the server rolls back the whole transaction.
#'   \item A connection that breaks after the load starts stops with an error
#'     of class \code{csdb_load_ambiguous}. csdb does not run the session
#'     again.
#'   \item Every other failure stops at once. The server stores no row in any
#'     table.
#' }
#'
#' A duplicate key also stops the whole session, and the server stores no row.
#' With \code{confirm_insert_via_nrow = FALSE}, the function then stops with an
#' error of class \code{csdb_load_duplicate_key}. With
#' \code{confirm_insert_via_nrow = TRUE}, it calls
#' \code{$insert_data(confirm_insert_via_nrow = TRUE)} on each table in turn.
#' That call upserts the rows of a table that stops on a duplicate key. Each
#' table then has its own transaction, so a later failure leaves the rows in
#' the tables before it.
#'
#' After a session that succeeds with \code{confirm_insert_via_nrow = TRUE},
#' csdb counts the rows of each table, as \code{$insert_data()} does. It
#' upserts the rows of a table that holds fewer rows than its data.table.
#'
#' Before the session, csdb runs the first steps of \code{$insert_data()} on
#' every table, in order. It creates a table that does not exist. It skips a
#' table whose data.table is \code{NULL} or has no rows, so the script has no
#' \code{\\copy} for that table. It runs the validator of the field contents on
#' the other tables. A validator that fails stops the call before any load.
#'
#' The session needs every table to have the same connection configuration,
#' and a driver whose loads use \code{psql}. Otherwise
#' \code{insert_data_many()} calls \code{$insert_data()} on each table in turn.
#' A failure then leaves the new rows in the tables that came before it.
#' @param tables A list of \code{DBTable_v9} objects.
#' @param newdata A list of data.tables, one for each table, in the order of
#'   \code{tables}.
#' @param confirm_insert_via_nrow If TRUE, a duplicate key makes csdb insert
#'   into each table on its own, and upsert a table that stops on the
#'   duplicate key. After a session that succeeds, csdb upserts a table that
#'   holds fewer rows than its data.table.
#' @param verbose Boolean. csdb passes it to \code{$insert_data()} and
#'   \code{$upsert_data()} of each table.
#' @param load_timeout The longest time in seconds that the \code{psql} session
#'   MAY run. csdb then kills \code{psql} and stops with an error of class
#'   \code{csdb_load_ambiguous}, and it does not run the session again. The
#'   per-table path passes it to \code{$insert_data()}.
#' @param ... Named arguments for a subclass of \code{DBTable_v9} whose
#'   \code{$insert_data()} and \code{$upsert_data()} take more arguments. csdb
#'   passes them to every call of these two methods that it makes. It makes
#'   these calls on the per-table path, after a duplicate key, and for the
#'   upsert of \code{confirm_insert_via_nrow}. The \code{psql} session calls neither
#'   method, so an override of \code{$insert_data()} in a subclass does not run
#'   for the rows that the session loads. csdb stops before any load when one
#'   of the two methods of a table does not take an argument.
#' @return \code{NULL}, invisibly.
#' @examples
#' \donttest{
#' # SQLite has no psql, so csdb inserts into each table in turn here.
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
  load_timeout = 3600,
  ...
) {
  check_insert_many_arguments(tables, newdata)
  check_method_arguments(tables, names(list(...)), length(list(...)))
  if (length(tables) == 0L) {
    return(invisible(NULL))
  }
  if (!insert_session_possible(tables)) {
    return(insert_each(
      tables,
      newdata,
      confirm_insert_via_nrow = confirm_insert_via_nrow,
      verbose = verbose,
      load_timeout = load_timeout,
      ...
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
    return(insert_each(
      tables,
      newdata,
      confirm_insert_via_nrow = TRUE,
      verbose = verbose,
      load_timeout = load_timeout,
      ...
    ))
  }

  if (confirm_insert_via_nrow) {
    for (i in seq_along(tables)) {
      insert_confirm(
        tables[[i]],
        newdata[[i]],
        duplicate = NULL,
        verbose = verbose,
        load_timeout = load_timeout,
        ...
      )
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

# Stop before any load unless $insert_data() and $upsert_data() of every table
# take each argument in `...`. csdb passes `...` to every call of these two
# methods that it makes, on both paths. On the session path csdb calls them
# only after a duplicate key and for the upsert of confirm_insert_via_nrow, so
# a misspelt argument would otherwise pass unnoticed there.
check_method_arguments <- function(tables, extra, n) {
  if (n == 0L) {
    return(invisible(NULL))
  }
  if (is.null(extra) || any(!nzchar(extra))) {
    stop(
      "insert_data_many(): every argument in ... MUST have a name.",
      call. = FALSE
    )
  }
  for (tab in tables) {
    for (method in c("insert_data", "upsert_data")) {
      stop_on_unknown_arguments(tab, method, extra)
    }
  }
  return(invisible(NULL))
}

# Stop when $<method>() of `tab` takes neither `...` nor every name in
# `extra`.
stop_on_unknown_arguments <- function(tab, method, extra) {
  takes <- names(formals(tab[[method]]))
  unknown <- setdiff(extra, takes)
  if ("..." %in% takes || length(unknown) == 0) {
    return(invisible(NULL))
  }
  stop(
    "insert_data_many(): $",
    method,
    "() of table ",
    tab$table_name,
    " takes no argument ",
    paste0(unknown, collapse = ", "),
    ". csdb passes the arguments in ... to $insert_data() and ",
    "$upsert_data() of each table, so both methods MUST take them.",
    call. = FALSE
  )
}

# The per-table path: $insert_data() on each table in turn. `...` goes to each
# $insert_data().
insert_each <- function(
  tables,
  newdata,
  confirm_insert_via_nrow,
  verbose,
  load_timeout,
  ...
) {
  for (i in seq_along(tables)) {
    tables[[i]]$insert_data(
      newdata[[i]],
      confirm_insert_via_nrow = confirm_insert_via_nrow,
      verbose = verbose,
      load_timeout = load_timeout,
      ...
    )
  }
  return(invisible(NULL))
}

# One psql session that loads every table, through the connection of the
# first table. The tables share one connection configuration.
insert_session <- function(tables, newdata, load_timeout) {
  first <- tables[[1]]
  psql_load_session(
    connection = first$dbconnection$autoconnection,
    dbconfig = first$dbconnection$config,
    tables = lapply(tables, function(x) {
      return(x$table_name_short_for_mssql_fully_specified_for_postgres)
    }),
    dts = newdata,
    files = lapply(seq_along(tables), function(i) {
      return(insert_load_file(table_private(tables[[i]]), newdata[[i]]))
    }),
    load_timeout = load_timeout
  )
  return(NULL)
}

# TRUE when load_data_infile() dispatches to the psql method for this
# connection. That is the PostgreSQL branch of today's per-table load, so the
# session covers exactly the connections that load through psql.
loads_through_psql <- function(connection) {
  found <- tryCatch(
    S7::method(load_data_infile, object = connection),
    error = function(e) NULL
  )
  return(identical(found, S7::method(load_data_infile, db_postgres)))
}

# One psql session can load the tables when they share one connection
# configuration and that connection loads through psql.
insert_session_possible <- function(tables) {
  configs <- lapply(tables, function(x) x$dbconnection$config)
  same <- all(vapply(configs, identical, logical(1), configs[[1]]))
  return(same && loads_through_psql(tables[[1]]$dbconnection$autoconnection))
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
# when the table holds fewer rows than `newdata`. `...` goes to upsert_data().
insert_confirm <- function(
  self,
  newdata,
  duplicate,
  verbose,
  load_timeout,
  ...
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
      load_timeout = load_timeout,
      ...
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

# Load several PostgreSQL tables with one psql process, in one transaction.
#
# csdb writes one load file for each table, with the writer of the
# single-table load, and one script with one \copy for each table, in order.
# --single-transaction wraps the script in BEGIN and COMMIT, and
# ON_ERROR_STOP=1 makes psql stop at the first error. The server then rolls
# back every \copy before it. After each \copy, pg_row_count_check() compares
# the stored rows with the rows sent, inside the transaction. In the first
# version of this function only run_checked_load() compared them, after psql
# had committed, so a short count stopped the call with the rows stored.
# run_checked_load() still checks one "COPY <n>" for each table, and it
# classifies and retries the whole session as it does one table.
#
# The first line of the script sets lock_timeout to load_lock_timeout_seconds.
# A longer wait for a lock fails with SQLSTATE 55P03, which
# classify_psql_failure() classes as transient.
#
# The function deletes every load file and the script when it returns, also
# after a write that fails partway.
psql_load_session <- function(
  connection,
  dbconfig,
  tables,
  dts,
  files,
  load_timeout = 3600
) {
  files <- unlist(files)
  on.exit(unlink(files), add = TRUE)
  table_text <- vapply(
    tables,
    function(x) {
      return(as.character(DBI::dbQuoteIdentifier(connection, x)))
    },
    character(1)
  )
  rows_sent <- vapply(dts, nrow, integer(1))
  copies <- vapply(
    seq_along(tables),
    function(i) {
      return(pg_write_copy_command(
        connection,
        tables[[i]],
        dts[[i]],
        files[[i]]
      ))
    },
    character(1)
  )

  # A binary connection writes "\n" on every OS, so the script has the same
  # bytes on Windows as on Linux.
  script <- tempfile(fileext = ".sql")
  on.exit(unlink(script), add = TRUE)
  con <- file(script, open = "wb")
  lines <- c(
    sprintf("SET lock_timeout = '%ds';", as.integer(load_lock_timeout_seconds)),
    unlist(lapply(seq_along(copies), function(i) {
      return(c(
        copies[[i]],
        pg_row_count_check(table_text[[i]], rows_sent[[i]], i)
      ))
    }))
  )
  tryCatch(writeLines(lines, con, sep = "\n"), finally = close(con))

  return(run_psql_load(
    dbconfig = dbconfig,
    command = c("-f", script),
    table = table_text,
    rows_sent = rows_sent,
    load_timeout = load_timeout,
    options = c("-v", "ON_ERROR_STOP=1", "--single-transaction")
  ))
}

# The psql lines that check one table inside the transaction, after its \copy.
#
# psql sets ROW_COUNT to the number of rows that the \copy stored. psql 14.24
# against PostgreSQL 14.24 printed "ROW_COUNT=2" after a \copy of 3 rows of
# which a trigger skipped 1 (2026-10-12). \gset stores the comparison in
# csdb_rows_ok, and \if runs the DO block only when the counts differ. Its
# RAISE EXCEPTION stops the script under ON_ERROR_STOP=1 before COMMIT, so
# the server rolls back every table. The error has SQLSTATE P0001, which
# classify_psql_failure() classes as "other": csdb does not retry it.
#
# psql does not replace :csdb_rows inside the DO block, so \echo prints the
# stored count first. A quote or a % in the table name is doubled, because
# the name sits in a SQL string that RAISE reads as a format.
pg_row_count_check <- function(table_text, rows_sent, i) {
  name <- gsub(
    "%",
    "%%",
    gsub("'", "''", table_text, fixed = TRUE),
    fixed = TRUE
  )
  return(c(
    sprintf(
      "SELECT :ROW_COUNT AS csdb_rows, :ROW_COUNT = %d AS csdb_rows_ok \\gset",
      as.integer(rows_sent)
    ),
    "\\if :csdb_rows_ok",
    "\\else",
    sprintf(
      "\\echo csdb: table number %d of the session stored :csdb_rows rows, and csdb sent %d rows.",
      as.integer(i),
      as.integer(rows_sent)
    ),
    sprintf(
      "DO $csdb$ BEGIN RAISE EXCEPTION 'csdb: table %s did not store the %d rows that csdb sent. The server stored no row in any table.'; END $csdb$;",
      name,
      as.integer(rows_sent)
    ),
    "\\endif"
  ))
}
