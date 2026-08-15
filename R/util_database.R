# Database Utilities for csdb package
# This file contains S7 database methods and database-specific utility functions

# Database utility functions

#' Replace non-finite values with NA, by reference
#'
#' Every write path in this package writes `Inf`, `-Inf` and `NaN` as text,
#' which destroys the upload. The CSV backends write the literal string.
#' `DBI::dbAppendTable()` stores `Inf` and reads it back as `Inf`. This
#' function sets them to `NA` first, which makes all three backends agree.
#'
#' `write_data_infile()` and the SQLite `load_data_infile` method both call
#' this function. The two paths do not share the `POSIXt` to character
#' conversion, and that is deliberate. The SQLite path needs a `POSIXct` to
#' stay a `POSIXct`. A connection opened with `extended_types = TRUE`
#' round-trips a `POSIXct` through a `DATETIME` column correctly.
#'
#' @param dt data.table to scrub.
#' @return `dt`, invisibly. The function changes `dt` by reference.
#' @keywords internal
#' @noRd
scrub_non_finite <- function(dt) {
  for (i in names(dt)) {
    dt[is.infinite(get(i)), (i) := NA]
    dt[is.nan(get(i)), (i) := NA]
  }
  invisible(dt)
}

#' Write data.table to file for database bulk insert
#'
#' Internal function that writes a data.table to a CSV file. The format suits a
#' database bulk insert. The function handles the special cases: infinite
#' values, `NaN` values, and `POSIXt` objects.
#'
#' @param dt data.table to write.
#' @param file Output file path.
#' @param colnames Logical, whether to include the column names.
#' @param eol End of line character.
#' @param quote Quoting behavior.
#' @param na String to use for NA values.
#' @param sep Column separator.
#' @return NULL. The function is called for its side effects.
#' @keywords internal
#' @noRd
write_data_infile <- function(
  dt,
  file = paste0(tempfile(), ".csv"),
  colnames = T,
  eol = "\n",
  quote = "auto",
  na = "\\N",
  sep = ","
) {
  # infinites and NANs get written as text
  # which destroys the upload
  # we need to set them to NA
  scrub_non_finite(dt)
  for (i in names(dt)) {
    if (inherits(dt[[i]], "POSIXt")) dt[, (i) := as.character(get(i))]
  }
  fwrite(
    dt,
    file = file,
    logical01 = T,
    na = na,
    col.names = colnames,
    eol = eol,
    quote = quote,
    sep = sep
  )
}

#' List indexes for a database table
#'
#' Internal function that lists all indexes for one table. It supports
#' Microsoft SQL Server only.
#'
#' @param connection Database connection object.
#' @param table Name of the table to list indexes for.
#' @return data.frame with index information.
#' @keywords internal
#' @noRd
list_indexes <- function(connection, table) {
  retval <- DBI::dbGetQuery(
    connection,
    glue::glue(
      "select * from sys.indexes where object_id = (select object_id from sys.objects where name = '{table}')"
    )
  )
  return(retval)
}

# S7 classes for database connections - register S4 classes for S7 dispatch
# This allows S7 to dispatch on actual S4 connection objects
# We need to register the S4 classes with S7 first, then create methods for them

# Helper function to safely register S4 classes for S7 dispatch
register_s4_classes <- function() {
  # Register S4 classes from odbc package if available
  tryCatch(
    {
      # Ensure odbc package is available before trying to register classes
      if (requireNamespace("odbc", quietly = TRUE)) {
        # Check if PostgreSQL S4 class exists in odbc package and register it
        if (
          methods::isClass("PostgreSQL") &&
            methods::getClass("PostgreSQL")@package == "odbc"
        ) {
          S7::S4_register(methods::getClass("PostgreSQL"))
        }

        # Check if Microsoft SQL Server S4 class exists and register it
        if (
          methods::isClass("Microsoft SQL Server") &&
            methods::getClass("Microsoft SQL Server")@package == "odbc"
        ) {
          S7::S4_register(methods::getClass("Microsoft SQL Server"))
        }
      }
    },
    error = function(e) {
      # Silently continue if odbc package not available
      NULL
    }
  )

  # Register the SQLite S4 class from RSQLite
  tryCatch(
    {
      if (requireNamespace("RSQLite", quietly = TRUE)) {
        if (
          methods::isClass("SQLiteConnection") &&
            methods::getClass("SQLiteConnection")@package == "RSQLite"
        ) {
          S7::S4_register(methods::getClass("SQLiteConnection"))
        }
      }
    },
    error = function(e) NULL
  )

  # Register DBI classes if available
  tryCatch(
    {
      if (requireNamespace("DBI", quietly = TRUE)) {
        if (methods::isClass("DBIConnection")) {
          S7::S4_register(methods::getClass("DBIConnection"))
        }
      }
    },
    error = function(e) NULL
  )
}

# Helper function to get or create database class objects for S7 dispatch
get_db_classes <- function() {
  # Try to get actual S4 classes first, fall back to S3 wrappers

  # PostgreSQL class
  db_postgres <- tryCatch(
    {
      if (
        requireNamespace("odbc", quietly = TRUE) &&
          methods::isClass("PostgreSQL") &&
          methods::getClass("PostgreSQL")@package == "odbc"
      ) {
        methods::getClass("PostgreSQL")
      } else {
        S7::new_S3_class("PostgreSQL")
      }
    },
    error = function(e) S7::new_S3_class("PostgreSQL")
  )

  # Microsoft SQL Server class
  db_mssql <- tryCatch(
    {
      if (
        requireNamespace("odbc", quietly = TRUE) &&
          methods::isClass("Microsoft SQL Server") &&
          methods::getClass("Microsoft SQL Server")@package == "odbc"
      ) {
        methods::getClass("Microsoft SQL Server")
      } else {
        S7::new_S3_class("Microsoft SQL Server")
      }
    },
    error = function(e) S7::new_S3_class("Microsoft SQL Server")
  )

  # SQLite class.
  # This one gets NO S3 fallback, and the difference matters. The real S4
  # DBIConnection default is always present, so a method registered against
  # S7::new_S3_class("SQLiteConnection") never wins dispatch: the call lands
  # on the db_default method instead and emits MySQL-flavoured SQL with no
  # error at all. Registering the real class afterwards does not retarget
  # methods that were already defined against the fallback, so
  # refresh_database_methods() cannot repair it either. RSQLite is in
  # Imports, so failing loudly here is both safe and the only honest option.
  db_sqlite <- tryCatch(
    {
      if (
        requireNamespace("RSQLite", quietly = TRUE) &&
          methods::isClass("SQLiteConnection")
      ) {
        methods::getClass("SQLiteConnection")
      } else {
        stop(
          "the RSQLite package does not provide the S4 class 'SQLiteConnection'"
        )
      }
    },
    error = function(e) {
      stop(
        "csdb cannot register its SQLite methods: ",
        conditionMessage(e),
        ". RSQLite is listed in csdb's Imports; install it with ",
        "install.packages(\"RSQLite\")."
      )
    }
  )

  # Default/DBI class
  db_default <- tryCatch(
    {
      if (
        requireNamespace("DBI", quietly = TRUE) &&
          methods::isClass("DBIConnection")
      ) {
        methods::getClass("DBIConnection")
      } else {
        S7::new_S3_class("DBIConnection")
      }
    },
    error = function(e) S7::new_S3_class("DBIConnection")
  )

  return(list(
    postgres = db_postgres,
    mssql = db_mssql,
    sqlite = db_sqlite,
    default = db_default
  ))
}

# Public function to re-register database methods
# This can be called if database connections are not working properly
#' Re-register database S7 methods
#'
#' Re-registers S7 methods for database operations. Call this function if you
#' encounter method dispatch errors with database connections.
#'
#' @return NULL. The function is called for its side effects.
#' @keywords internal
#' @noRd
refresh_database_methods <- function() {
  # Re-register S4 classes
  register_s4_classes()

  # Re-get database class objects
  db_classes <- get_db_classes()
  db_postgres <<- db_classes$postgres
  db_mssql <<- db_classes$mssql
  db_sqlite <<- db_classes$sqlite
  db_default <<- db_classes$default

  # Re-register S7 methods
  S7::methods_register()

  message(
    "Database methods refreshed. S4 classes re-registered and S7 methods updated."
  )
  invisible(NULL)
}

# Debug function to show method dispatch information
#' Debug database method dispatch
#'
#' Shows information about registered classes and methods. Use it to debug
#' method dispatch.
#'
#' @param connection Optional database connection object to check.
#' @return List with debugging information.
#' @keywords internal
#' @noRd
debug_database_methods <- function(connection = NULL) {
  info <- list()

  # Check available packages
  info$packages <- list(
    odbc_available = requireNamespace("odbc", quietly = TRUE),
    DBI_available = requireNamespace("DBI", quietly = TRUE),
    RSQLite_available = requireNamespace("RSQLite", quietly = TRUE),
    S7_available = requireNamespace("S7", quietly = TRUE)
  )

  # Check registered S4 classes
  info$s4_classes <- list()
  if (info$packages$odbc_available) {
    info$s4_classes$PostgreSQL <- tryCatch(
      {
        if (methods::isClass("PostgreSQL")) {
          list(
            exists = TRUE,
            package = methods::getClass("PostgreSQL")@package
          )
        } else {
          list(exists = FALSE)
        }
      },
      error = function(e) list(error = e$message)
    )

    info$s4_classes$MicrosoftSQLServer <- tryCatch(
      {
        if (methods::isClass("Microsoft SQL Server")) {
          list(
            exists = TRUE,
            package = methods::getClass("Microsoft SQL Server")@package
          )
        } else {
          list(exists = FALSE)
        }
      },
      error = function(e) list(error = e$message)
    )
  }

  if (info$packages$RSQLite_available) {
    info$s4_classes$SQLiteConnection <- tryCatch(
      {
        if (methods::isClass("SQLiteConnection")) {
          list(
            exists = TRUE,
            package = methods::getClass("SQLiteConnection")@package
          )
        } else {
          list(exists = FALSE)
        }
      },
      error = function(e) list(error = e$message)
    )
  }

  # Check connection object if provided
  if (!is.null(connection)) {
    info$connection <- list(
      class = class(connection),
      is_s4 = methods::is(connection, "S4"),
      package = attr(class(connection), "package")
    )
  }

  # Check current database class objects
  info$db_classes <- list(
    postgres_type = class(db_postgres),
    mssql_type = class(db_mssql),
    sqlite_type = class(db_sqlite),
    default_type = class(db_default)
  )

  return(info)
}

# Register S4 classes and get database class objects
register_s4_classes()
db_classes <- get_db_classes()
db_postgres <- db_classes$postgres
db_mssql <- db_classes$mssql
db_sqlite <- db_classes$sqlite
db_default <- db_classes$default

# S7 generic definitions (internal use only)
load_data_infile <- S7::new_generic("load_data_infile", "connection")
upsert_load_data_infile <- S7::new_generic(
  "upsert_load_data_infile",
  "connection"
)
create_table <- S7::new_generic("create_table", "connection")
add_constraint <- S7::new_generic("add_constraint", "connection")
drop_constraint <- S7::new_generic("drop_constraint", "connection")
get_indexes <- S7::new_generic("get_indexes", "connection")
get_index_columns <- S7::new_generic("get_index_columns", "connection")
drop_index <- S7::new_generic("drop_index", "connection")
add_index <- S7::new_generic("add_index", "connection")
drop_all_rows <- S7::new_generic("drop_all_rows", "connection")
drop_rows_where <- S7::new_generic("drop_rows_where", "connection")
keep_rows_where <- S7::new_generic("keep_rows_where", "connection")
drop_table <- S7::new_generic("drop_table", "connection")

# S7 method definitions
# load_data_infile methods
S7::method(load_data_infile, db_default) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt = NULL,
  file = "/xtmp/x123.csv",
  force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  t0 <- Sys.time()

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = F]
  }
  write_data_infile(dt = dt, file = file)
  on.exit(unlink(file), add = T)

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- T
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  sql <- paste0(
    "LOAD DATA INFILE ",
    DBI::dbQuoteString(connection, path),
    "\n",
    "INTO TABLE ",
    DBI::dbQuoteIdentifier(connection, table),
    "\n",
    "CHARACTER SET utf8",
    "\n",
    "FIELDS TERMINATED BY ",
    DBI::dbQuoteString(connection, sep),
    "\n",
    "OPTIONALLY ENCLOSED BY ",
    DBI::dbQuoteString(connection, quote),
    "\n",
    "LINES TERMINATED BY ",
    DBI::dbQuoteString(connection, eol),
    "\n",
    "IGNORE ",
    skip + as.integer(header),
    " LINES \n",
    "(",
    paste0(correct_order, collapse = ","),
    ")"
  )
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)

  invisible()
}

S7::method(load_data_infile, db_mssql) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  a <- Sys.time()

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = F]
  }
  write_data_infile(
    dt = dt,
    file = file,
    colnames = F,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )
  on.exit(unlink(file), add = T)

  format_file <- tempfile(tmpdir = tempdir(check = TRUE))
  on.exit(unlink(format_file), add = T)

  args <- c(
    table,
    "format",
    "nul",
    "-q",
    "-c",
    "-f",
    format_file,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }

  if (Sys.which("bcp") == "") {
    stop("bcp command not found. Please install SQL Server command line tools.")
  }

  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  if (FALSE) {
    hint_arg <- NULL
  } else {
    hint_arg <- NULL
  }

  if (!is.null(key(dt))) {
    hint_arg <- c(
      hint_arg,
      paste0("ORDER(", paste0(key(dt), " ASC", collapse = ", "), ")")
    )
  }
  if (length(hint_arg) > 0) {
    hint_arg <- paste0(hint_arg, collapse = ", ")
    hint_arg <- paste0("-h '", hint_arg, "'")
  }

  args <- c(
    table,
    "in",
    file,
    "-a 16384",
    hint_arg,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password,
    "-f",
    format_file,
    "-m",
    0
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }

  if (Sys.which("bcp") == "") {
    stop("bcp command not found. Please install SQL Server command line tools.")
  }

  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

S7::method(load_data_infile, db_postgres) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  a <- Sys.time()

  table_text <- DBI::dbQuoteIdentifier(connection, table)

  correct_order <- DBI::dbListFields(connection, table)

  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = F]
  }

  write_data_infile(
    dt = dt,
    file = file,
    colnames = F,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )

  on.exit(unlink(file), add = T)

  sql <- sprintf(
    "\"\\copy %s (%s) from '%s' (FORMAT CSV, DELIMITER '\t')\"",
    table_text,
    paste(correct_order, collapse = ","),
    file
  )

  uri <- sprintf(
    "postgresql://%s:%s@%s:%s/%s",
    dbconfig$user,
    dbconfig$password,
    dbconfig$server,
    dbconfig$port,
    dbconfig$db
  )

  args <- c(
    "-U",
    dbconfig$user,
    "-c",
    sql,
    uri
  )

  if (Sys.which("psql") == "") {
    stop(
      "psql command not found. Please install PostgreSQL command line tools."
    )
  }

  system2(
    "psql",
    args = args,
    stdout = FALSE
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

# Load a data.table into a SQLite table.
#
# There is no staging file and no external client binary here: SQLite is a
# file, and DBI::dbAppendTable() writes 100,000 rows in about 0.02 seconds.
# `file` and `force_tablock` are accepted so the call sites in DBTable_v9 need
# no SQLite arm, and are then ignored.
#
# The copy() is load-bearing. The other three backends reach
# write_data_infile(), which modifies the caller's data.table by reference and
# has always done so. Doing the same here would leave the caller holding a
# table whose Inf values had turned into NA, which is a surprising thing for
# an insert to do to its argument.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(load_data_infile, db_sqlite) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt = NULL,
  file = tempfile(),
  force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  dt <- data.table::copy(dt)

  # Inf survives dbAppendTable and reads back as Inf, where the CSV backends
  # write NA. Without this the SQLite backend silently disagrees with them.
  scrub_non_finite(dt)

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = FALSE]
  }

  DBI::dbAppendTable(connection, table, dt)

  invisible()
}

# Continue with upsert_load_data_infile methods
S7::method(upsert_load_data_infile, db_default) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = "/tmp/x123.csv",
  fields,
  keys = NULL,
  drop_indexes = NULL
) {
  temp_name <- random_uuid()
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(connection, sql)

  if (!is.null(drop_indexes)) {
    # `drop_indexes` holds LOGICAL names. Every caller fills it from
    # names(self$indexes): the two DBTable_v9 defaults, and the two
    # DBTableExtended_v9 defaults in cs9. The temporary table is created LIKE
    # {table}, so it inherits {table}'s index names, and those are the
    # PHYSICAL names. Map them here, from the identity of {table} and not of
    # the temporary table. Without that map this drops nothing, and the upsert
    # keeps every index it meant to remove.
    for (i in drop_indexes) {
      physical <- index_physical_name(table = table, index = i)
      try(
        DBI::dbExecute(
          connection,
          glue::glue("ALTER TABLE `{temp_name}` DROP INDEX `{physical}`")
        ),
        TRUE
      )
    }
  }

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file
  )

  t0 <- Sys.time()

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue(
    "
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    "
  )
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)

  invisible()
}

S7::method(upsert_load_data_infile, db_mssql) <- function(
  connection,
  dbconfig,
  table,
  dt,
  file = tempfile(),
  fields,
  keys,
  drop_indexes = NULL
) {
  temp_name <- paste0("tmp", random_uuid())
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE 1 = 0;")
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE
  )

  a <- Sys.time()
  add_index(
    connection = connection,
    table = temp_name,
    keys = keys
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", keys),
    s = paste0("s.", keys)
  )
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  sql_update_set <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", fields),
    s = paste0("s.", fields)
  )
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue(
    "
  MERGE {table} t
  USING {temp_name} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED BY TARGET
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  "
  )

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

S7::method(upsert_load_data_infile, db_postgres) <- function(
  connection,
  dbconfig,
  table,
  dt,
  file = tempfile(),
  fields,
  keys,
  drop_indexes = NULL
) {
  temp_name <- DBI::Id(
    schema = table@name[["schema"]],
    paste0("tmp", random_uuid())
  )
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)
  table_text <- DBI::dbQuoteIdentifier(connection, table)

  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue(
    "SELECT * INTO {temp_name_text} FROM {table_text} WHERE 1 = 0;"
  )
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE
  )

  a <- Sys.time()
  # Two arguments here were wrong, and the try() inside add_index() hid both.
  # `"ind" + random_uuid()` is `non-numeric argument to binary operator`,
  # because `+` does not concatenate strings in R.
  #
  # This passes `temp_name`, the DBI::Id, and not the pre-quoted
  # `temp_name_text` beside it. add_index() quotes the table itself from
  # 2026.8.16, so a pre-quoted string would arrive and be quoted a second
  # time. Verified against PostgreSQL 16.14 on norsyss_data1.
  add_index(
    connection = connection,
    table = temp_name,
    keys = keys,
    index = paste0("ind", random_uuid())
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", keys),
    s = paste0("s.", keys)
  )
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  update_fields <- setdiff(fields, keys)
  sql_update_set <- glue::glue(
    "{t} = {s}",
    t = update_fields,
    s = paste0("s.", update_fields)
  )
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue(
    "
  MERGE INTO {table_text} t
  USING {temp_name_text} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  "
  )

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

# Upsert a data.table into a SQLite table.
#
# SQLite has no MERGE and no ON DUPLICATE KEY UPDATE. It has
# INSERT ... ON CONFLICT, which needs three things the other backends do not:
#
#   * a PRIMARY KEY or UNIQUE constraint on the conflict target. The SQLite
#     create_table method inlines one, which is why creation and upsert are
#     the same piece of work.
#   * `WHERE true` between the SELECT and the ON CONFLICT clause. Without it
#     SQLite's parser cannot tell the ON CONFLICT clause from a join
#     constraint on the SELECT, and rejects the statement.
#   * DO NOTHING rather than DO UPDATE SET when every field is a key, because
#     there is then nothing left to assign.
#
# The three preconditions are checked before any SQL is emitted. Empty `keys`
# would produce `ON CONFLICT ()`, a syntax error that says nothing about the
# cause. `fields` that are not exactly the table's live columns cannot work at
# all: CREATE TABLE ... AS SELECT discards defaults and constraints, so a
# staging table filled from a partial field list would insert NULL into every
# omitted column.
#
# `drop_indexes` is ignored, exactly as the PostgreSQL method already ignores
# it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(upsert_load_data_infile, db_sqlite) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  fields,
  keys = NULL,
  drop_indexes = NULL
) {
  if (length(keys) == 0) {
    stop(
      "upsert on SQLite needs at least one key column: ",
      "keys is empty, and ON CONFLICT () is a syntax error."
    )
  }
  if (!all(keys %in% fields)) {
    stop(
      "upsert on SQLite needs every key to be one of the fields. ",
      "Missing from fields: ",
      paste0(setdiff(keys, fields), collapse = ", "),
      "."
    )
  }
  live_fields <- DBI::dbListFields(connection, table)
  if (!setequal(fields, live_fields)) {
    stop(
      "upsert on SQLite needs fields to be exactly the columns of the table. ",
      "In fields but not the table: ",
      paste0(setdiff(fields, live_fields), collapse = ", "),
      ". In the table but not fields: ",
      paste0(setdiff(live_fields, fields), collapse = ", "),
      "."
    )
  }

  table_text <- DBI::dbQuoteIdentifier(connection, table)
  temp_name <- paste0("tmp", random_uuid())
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)

  on.exit(
    try(
      DBI::dbExecute(
        connection,
        paste0("DROP TABLE IF EXISTS ", temp_name_text)
      ),
      silent = TRUE
    ),
    add = TRUE,
    after = FALSE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE TEMPORARY TABLE ",
      temp_name_text,
      " AS SELECT * FROM ",
      table_text,
      " WHERE 0"
    )
  )

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file
  )

  fields_text <- paste0(
    DBI::dbQuoteIdentifier(connection, fields),
    collapse = ", "
  )
  keys_text <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  update_fields <- setdiff(fields, keys)
  if (length(update_fields) > 0) {
    update_fields_text <- DBI::dbQuoteIdentifier(connection, update_fields)
    resolution <- paste0(
      "DO UPDATE SET ",
      paste0(
        update_fields_text,
        " = excluded.",
        update_fields_text,
        collapse = ", "
      )
    )
  } else {
    resolution <- "DO NOTHING"
  }

  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO ",
      table_text,
      " (",
      fields_text,
      ") SELECT ",
      fields_text,
      " FROM ",
      temp_name_text,
      " WHERE true ON CONFLICT (",
      keys_text,
      ") ",
      resolution
    )
  )

  invisible()
}

# create_table methods
S7::method(create_table, db_default) <- function(
  connection,
  table,
  fields,
  keys = NULL,
  role_create_table = NULL,
  ...
) {
  fields_new <- fields
  fields_new[
    fields == "TEXT"
  ] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"

  sql <- DBI::sqlCreateTable(
    connection,
    table,
    fields_new,
    row.names = F,
    temporary = F
  )
  DBI::dbExecute(connection, sql)
}

S7::method(create_table, db_mssql) <- function(
  connection,
  table,
  fields,
  keys = NULL,
  role_create_table = NULL,
  ...
) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "NVARCHAR (1000)"
  fields_new[fields == "DOUBLE"] <- "FLOAT"
  fields_new[fields == "BOOLEAN"] <- "BIT"

  if (!is.null(keys)) {
    fields_new[names(fields_new) %in% keys] <- paste0(
      fields_new[names(fields_new) %in% keys],
      " NOT NULL"
    )
  }

  sql <- DBI::sqlCreateTable(
    connection,
    table,
    fields_new,
    row.names = F,
    temporary = F
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")
  DBI::dbExecute(connection, sql)
}

S7::method(create_table, db_postgres) <- function(
  connection,
  table,
  fields,
  keys = NULL,
  role_create_table = NULL,
  ...
) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "VARCHAR"
  fields_new[fields == "DOUBLE"] <- "REAL"
  fields_new[fields == "BOOLEAN"] <- "BIT"
  fields_new[fields == "DATETIME"] <- "TIMESTAMP"

  if (!is.null(keys)) {
    fields_new[names(fields_new) %in% keys] <- paste0(
      fields_new[names(fields_new) %in% keys],
      " NOT NULL"
    )
  }

  sql <- DBI::sqlCreateTable(
    connection,
    table,
    fields_new,
    row.names = F,
    temporary = F
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")

  if (!is.na(role_create_table)) {
    if (role_create_table != "x") {
      sql <- paste0(
        "SET ROLE ",
        DBI::dbQuoteIdentifier(connection, role_create_table),
        "; ",
        sql,
        "; RESET ROLE"
      )
    }
  }

  DBI::dbExecute(connection, sql)
}

#' The csdb field types that SQLite accepts, and what each becomes
#'
#' The map is closed on purpose. SQLite accepts any declared type name. An
#' unrecognised name passed straight through would create a table with an
#' unintended affinity and no warning. `TEXT(100)`, `VARCHAR(100)` and a
#' misspelling would all succeed. `DATE` and `DATETIME` are declared types
#' rather than storage classes. A connection opened with
#' `extended_types = TRUE` reads them back as `Date` and `POSIXct`.
#'
#' @keywords internal
#' @noRd
sqlite_field_types <- c(
  "TEXT" = "TEXT",
  "INTEGER" = "INTEGER",
  "DOUBLE" = "REAL",
  "BOOLEAN" = "INTEGER",
  "DATE" = "DATE",
  "DATETIME" = "DATETIME"
)

S7::method(create_table, db_sqlite) <- function(
  connection,
  table,
  fields,
  keys = NULL,
  role_create_table = NULL,
  ...
) {
  unsupported <- !fields %in% names(sqlite_field_types)
  if (any(unsupported)) {
    stop(
      "SQLite does not support the field type(s): ",
      paste0(
        names(fields)[unsupported],
        " (",
        fields[unsupported],
        ")",
        collapse = ", "
      ),
      ". The supported types are: ",
      paste0(names(sqlite_field_types), collapse = ", "),
      "."
    )
  }

  fields_new <- unname(sqlite_field_types[fields])
  names(fields_new) <- names(fields)

  # SQLite cannot add a primary key to a table after the fact, so the key
  # columns are marked NOT NULL and the key itself is inlined below.
  if (!is.null(keys)) {
    fields_new[names(fields_new) %in% keys] <- paste0(
      fields_new[names(fields_new) %in% keys],
      " NOT NULL"
    )
  }

  definitions <- paste0(
    DBI::dbQuoteIdentifier(connection, names(fields_new)),
    " ",
    fields_new
  )
  if (length(keys) > 0) {
    definitions <- c(
      definitions,
      paste0(
        "PRIMARY KEY (",
        paste0(DBI::dbQuoteIdentifier(connection, keys), collapse = ", "),
        ")"
      )
    )
  }

  # role_create_table is ignored: SQLite has no roles.
  sql <- paste0(
    "CREATE TABLE ",
    DBI::dbQuoteIdentifier(connection, table),
    " (\n  ",
    paste0(definitions, collapse = ",\n  "),
    "\n)"
  )
  DBI::dbExecute(connection, sql)
}

# add_constraint methods
S7::method(add_constraint, db_default) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue(
    "
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});"
  )
  a <- DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(add_constraint, db_postgres) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue(
    "ALTER table {table}
    ADD CONSTRAINT {constraint}
    PRIMARY KEY ({primary_keys});"
  )

  a <- DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

# Add a primary key constraint to a SQLite table.
#
# This method does nothing, and that is the whole of it. SQLite has no
# ALTER TABLE ... ADD CONSTRAINT ... PRIMARY KEY: the statement the other
# backends use is a syntax error there. The SQLite create_table method
# therefore inlines PRIMARY KEY (...) in the CREATE TABLE statement, so by the
# time this is called the key already exists and there is nothing left to add.
#
# connection  A SQLite connection.
# table       The table the key belongs to. Not used.
# keys        The key columns. Not used.
# returns     NULL, invisibly.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class, and a
# roxygen block here makes roxygenise() report "Unknown S7 class type".
S7::method(add_constraint, db_sqlite) <- function(connection, table, keys) {
  invisible(NULL)
}

# drop_constraint methods
S7::method(drop_constraint, db_default) <- function(connection, table) {
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue(
    "
          ALTER table {table}
          DROP CONSTRAINT {constraint};"
  )
  try(a <- DBI::dbExecute(connection, sql), TRUE)
}

# index naming
#
# The UUID namespace that index_physical_name() hashes under. The value is
# arbitrary and fixed. Changing it renames every index csdb manages.
INDEX_NAME_NAMESPACE <- "3f2a6c1e-7d54-4b98-9a0f-6c5d2e8b41a7"

# The longest identifier PostgreSQL keeps. NAMEDATALEN is 64 bytes, and one
# byte holds the terminator, so 63 characters survive.
INDEX_NAME_MAX_CHARS <- 63L

# The front of every physical index name.
INDEX_NAME_PREFIX <- "ix_"

# How many hexadecimal characters of the digest the name carries.
INDEX_NAME_DIGEST_CHARS <- 16L

# The ordered name components of a table, for index naming.
#
# The components, and NOT a joined string. Joining them loses the boundary
# between them, and the boundary is what tells two tables apart.
# `Id(schema = "a", table = "b.c")` and `Id(schema = "a.b", table = "c")` both
# join to `a.b.c`. A joined identity therefore gives them one index name.
#
# The identity MUST be the same at every call site. A name built at creation
# otherwise does not match the name used at drop, and the index becomes
# unreachable.
#
# csdb passes the DBI::Id at every site that needs an identity, and never the
# text form. Those sites are DBTable_v9$add_indexes(), $drop_indexes(),
# $confirm_indexes(), and the drop inside the default upsert method below.
# They all read
# table_name_short_for_mssql_fully_specified_for_postgres, so they all give
# one component vector, for every table name.
#
# Text is accepted for a caller that holds only the text form. Text splits on
# every dot, so `"anon.tab"` and `Id(schema = "anon", table = "tab")` give one
# vector. That agreement is what a text caller relies on, and a test pins it.
#
# The split keeps every empty component, because R drops a trailing empty
# string and `"a.b."` would otherwise read as `"a.b"`.
#
# Text with a dot names exactly the identity of its dot-separated pieces.
# That is a definition and not a guess. It costs one thing: `"a.b.c"` names
# three components. It is therefore NOT the text form of
# `Id(schema = "a", table = "b.c")`, which names two. No text can tell those
# two apart, because `"anon.tab"` is equally the text form of
# `Id(table = "anon.tab")`. csdb answers this by never using text as an
# identity, and not by a rule inside this function.
#
# table    Text, or a DBI::Id.
# returns  A character vector of the ordered name components.
index_table_identity <- function(table) {
  if (methods::is(table, "Id")) {
    return(as.character(table@name))
  }
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    stop("table must be one character string, or a DBI::Id.")
  }
  dots <- gregexpr(".", table, fixed = TRUE)[[1]]
  n <- if (attr(dots, "match.length")[1] == -1L) 0L else length(dots)
  parts <- strsplit(table, ".", fixed = TRUE)[[1]]
  length(parts) <- n + 1L
  parts[is.na(parts)] <- ""
  parts
}

# The dotted text form of a table identity.
#
# Two drop_index methods paste `table` straight into SQL with glue::glue().
# glue coerces with as.character(), and that raises
# `no method for coercing this S4 class to a vector` on a DBI::Id. csdb now
# hands those methods the Id, so they convert it here first.
#
# The result is byte-identical to
# table_name_short_for_mssql_fully_specified_for_postgres_text for every
# identity, so neither method changes what it emits for a table it already
# handled.
#
# table    Text, or a DBI::Id.
# returns  One character string.
index_table_text <- function(table) {
  paste(index_table_identity(table), collapse = ".")
}

# The name a declared index carries in the database.
#
# csdb used the caller's logical name as the database name, verbatim.
# `indexes = list(ind1 = "a")` created an index called `ind1`. A PostgreSQL
# index name is unique per SCHEMA, so every table in one schema that declared
# `ind1` asked for one name. `CREATE INDEX IF NOT EXISTS` answers a taken name
# with a notice, not an error. The first table won the name, and every later
# table silently got nothing.
#
# Measured on the norsyss_data1 database on 2026-08-15. The table
# `anon_norsyss_data` had 87 partitions in schema `anon`. All 87 declared
# `ind1` and `ind2`. One partition held `ind2`, and none held `ind1`.
#
# The name built here carries the table identity, so one table cannot take
# another table's name. It has three parts:
#
#   ix_       A fixed prefix. It keeps a letter at the front. PostgreSQL
#             rejects an unquoted identifier that starts with a digit, and the
#             db_postgres add_index method does not quote.
#   <slug>    The identity and the logical name, lowercased, with every other
#             character replaced by an underscore. This part is readable only.
#             It is cut from the LEFT when it is too long. That keeps the
#             logical name, because the logical name sits at the end.
#   <digest>  16 hexadecimal characters of a version 5 UUID over the identity
#             and the logical name. This part carries the distinctness.
#
# The result is at most 63 characters, the PostgreSQL identifier limit.
# PostgreSQL truncates a longer name and reports nothing. A silently truncated
# name plus `IF NOT EXISTS` is the same silent no-op again. csdb therefore
# applies the limit here, rather than leave it to the server.
#
# The name is lowercase. PostgreSQL folds an unquoted identifier to lowercase
# and SQLite does not. A lowercase name therefore reads the same in the source
# and in both catalogues. Measured on norsyss_data1: 92 lowercase `pk_`
# constraint names and 0 uppercase, while the source writes `PK_`.
#
# The key the digest covers is unambiguous. It length-prefixes the component
# count, then every table name component, then the logical name, so no two
# different inputs build one key. That is stronger than the `PK_{table}` rule
# in add_constraint(), which deletes `.`, `[` and `]`. Under that rule, schema
# `a` with table `bc` and schema `ab` with table `c` both give `PK_abc`.
#
# The name is collision-resistant and not injective. 16 hexadecimal characters
# hold 64 bits, and the version nibble of a version 5 UUID is fixed, so 60 bits
# vary. Two different keys give one name when those 60 bits agree. The key
# construction removes every structural collision; the digest width bounds what
# is left.
#
# table    The table identity. Text, or a DBI::Id.
# index    One logical index name, from names(self$indexes).
# returns  One lowercase character string of at most 63 characters.
index_physical_name <- function(table, index) {
  if (!is.character(index) || length(index) != 1L || is.na(index)) {
    stop("index must be one character string.")
  }
  parts <- index_table_identity(table)
  identity <- paste(parts, collapse = ".")

  # Every field is length-prefixed, and the first field is the component
  # count. A reader can therefore recover the exact input, so no two different
  # inputs build one key. Without the per-component prefix the pair
  # (Id("a", "b.c"), "ind1") and the pair (Id("a.b", "c"), "ind1") share a key.
  fields <- c(as.character(length(parts)), parts, index)
  key <- paste0(paste0(nchar(fields), ":", fields), collapse = "")
  digest <- gsub(
    "-",
    "",
    uuid::UUIDfromName(INDEX_NAME_NAMESPACE, key),
    fixed = TRUE
  )
  digest <- substr(digest, 1L, INDEX_NAME_DIGEST_CHARS)

  slug_max <- INDEX_NAME_MAX_CHARS -
    nchar(INDEX_NAME_PREFIX) -
    1L -
    INDEX_NAME_DIGEST_CHARS
  slug <- tolower(paste0(identity, "_", index))
  # Every character outside the class becomes an underscore, so the slug is
  # ASCII and one character is one byte. The 63 above is a byte limit.
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("^_+|_+$", "", slug)
  if (nchar(slug) > slug_max) {
    slug <- substring(slug, nchar(slug) - slug_max + 1L)
    slug <- sub("^_+", "", slug)
  }

  if (nzchar(slug)) {
    paste0(INDEX_NAME_PREFIX, slug, "_", digest)
  } else {
    paste0(INDEX_NAME_PREFIX, digest)
  }
}

# get_indexes methods
S7::method(get_indexes, db_mssql) <- function(connection, table) {
  index_name <- NULL
  table_name <- NULL

  table_rows <- connection |>
    DBI::dbGetQuery(
      "select o.name as table_name, i.name as index_name from sys.objects o join sys.sysindexes i on o.object_id = i.id where o.is_ms_shipped = 0 and i.rowcnt > 0 order by o.name"
    ) |>
    dplyr::filter(
      !is.na(index_name) & !stringr::str_detect(index_name, "^PK")
    ) |>
    setDT()
  retval <- table_rows[table_name %in% table]$index_name
  return(retval)
}

S7::method(get_indexes, db_postgres) <- function(connection, table) {
  index_name <- NULL
  table_name <- NULL

  sql <- "
    select tablename, indexname
    from pg_indexes
  "

  table_rows <- connection |>
    DBI::dbGetQuery(sql) |>
    dplyr::filter(!is.na(indexname) & !stringr::str_detect(indexname, "^pk")) |>
    setDT()
  retval <- table_rows[tablename %in% table]$indexname
  return(retval)
}

# List the indexes a SQLite table carries, in creation order.
#
# Three details are load-bearing:
#
#  1. `tbl_name` is a string VALUE in sqlite_master, not an identifier, so it
#     is bound as a parameter. DBI::dbQuoteIdentifier() would emit `` `tab` ``
#     and match nothing.
#  2. `name NOT LIKE 'sqlite\_%' ESCAPE '\'` removes the index a PRIMARY KEY
#     creates on its own, `sqlite_autoindex_<table>_1`. Without the filter
#     this returns the autoindex beside every index csdb manages. A caller
#     that counts or compares the whole vector then reads one index too many.
#
#     The ESCAPE clause is mandatory, not decoration. `_` is a
#     single-character wildcard in SQL LIKE, so the unescaped
#     `NOT LIKE 'sqlite_%'` also hides every user index whose name begins
#     "sqlite" followed by any character at all. An index named `sqliteIdx`
#     would then never be found, and every caller would read it as missing.
#     Written `'sqlite\\_%' ESCAPE '\\'` in R, so a literal backslash reaches
#     SQLite.
#  3. `ORDER BY rowid` is creation order, which is the order add_indexes()
#     iterates names(self$indexes) in. A caller that compares the whole vector
#     with identical() needs that order.
#
# The return value is `$name`, a plain character vector. A one-column
# data.frame, or a vector carrying names or any other attribute, fails an
# identical() test that a caller has every reason to make.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(get_indexes, db_sqlite) <- function(connection, table) {
  sql <- paste0(
    "SELECT name FROM sqlite_master ",
    "WHERE type = 'index' AND tbl_name = ? ",
    "AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\' ",
    "ORDER BY rowid"
  )
  retval <- DBI::dbGetQuery(connection, sql, params = list(table))$name
  return(retval)
}

# get_index_columns methods
#
# Every method answers one question. Which columns does `index` cover on
# `table`, in index order?
#
# Column verification is defined for SQLite and for PostgreSQL, and for no
# other backend. The db_default method returns NULL, which means "this backend
# has no catalogue reader". On such a backend add_indexes() creates the index
# and does NOT verify it, and confirm_indexes() falls back to existence by
# name. SQL Server and MySQL both dispatch to db_default and both get that
# weaker guarantee.
#
# There are three answers, and they are distinct:
#
#   NULL          The backend has no catalogue reader. Nothing was measured.
#   character(0)  No index of that name exists on that table.
#   a character   The column names, in index order.
#
# The distinction is what lets add_indexes() raise on a measured absence and
# stay quiet on an unmeasured one. `CREATE INDEX IF NOT EXISTS` reporting
# success proves nothing about which columns were indexed, and nothing about
# which table holds the name.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(get_index_columns, db_default) <- function(
  connection,
  table,
  index
) {
  NULL
}

# Read the columns of one SQLite index.
#
# Two queries run here, and both are needed.
#
# A SQLite index name is unique per database, not per table, so
# `pragma_index_info` answers for an index on ANY table. The first query reads
# `tbl_name` from sqlite_master and rejects an index that belongs to another
# table. Without it, a name that another table already took would read as a
# correct index here. That is the exact failure this release removes.
#
# `pragma_index_info` is the table-valued form of the pragma. It accepts a
# bound parameter, where `PRAGMA index_info(...)` does not. `seqno` is the
# position of the column inside the index, so ORDER BY seqno gives the
# declared order.
S7::method(get_index_columns, db_sqlite) <- function(connection, table, index) {
  parts <- index_table_identity(table)
  table_bare <- parts[length(parts)]
  owner <- DBI::dbGetQuery(
    connection,
    "SELECT tbl_name FROM sqlite_master WHERE type = 'index' AND name = ?",
    params = list(index)
  )$tbl_name
  if (length(owner) == 0L || !identical(owner[1], table_bare)) {
    return(character(0))
  }
  DBI::dbGetQuery(
    connection,
    "SELECT name FROM pragma_index_info(?) ORDER BY seqno",
    params = list(index)
  )$name
}

# Read the columns of one PostgreSQL index.
#
# The index relation carries its own columns in pg_attribute. They are
# numbered from 1 in index order, so ORDER BY attnum gives the declared order.
# `attname` is the base column name for a plain column reference.
#
# `table` arrives as the fully specified text, `schema.table`. pg_class holds
# the bare name and pg_namespace holds the schema, so the text is split on the
# last dot. An empty schema drops the pg_namespace test rather than match on
# the empty string, which would match nothing.
#
# csdb's test suite runs on SQLite alone, so the db_sqlite method above is the
# one under test. This method was verified by hand against the norsyss_data1
# server on 2026-08-15. Two tables in one schema, both declaring the index name
# ind1, each returned isoyearweek for its own index and nothing for the other.
# No automated test covers it.
S7::method(get_index_columns, db_postgres) <- function(
  connection,
  table,
  index
) {
  parts <- index_table_identity(table)
  table_bare <- parts[length(parts)]
  schema <- if (length(parts) > 1L) parts[length(parts) - 1L] else ""

  sql <- paste0(
    "select a.attname as column_name ",
    "from pg_class ic ",
    "join pg_namespace n on n.oid = ic.relnamespace ",
    "join pg_index ix on ix.indexrelid = ic.oid ",
    "join pg_class tc on tc.oid = ix.indrelid ",
    "join pg_attribute a on a.attrelid = ic.oid and a.attnum > 0 ",
    "where ic.relkind = 'i' and ic.relname = ? and tc.relname = ?"
  )
  params <- list(index, table_bare)
  if (nzchar(schema)) {
    sql <- paste0(sql, " and n.nspname = ?")
    params <- c(params, list(schema))
  }
  sql <- paste0(sql, " order by a.attnum")

  DBI::dbGetQuery(connection, sql, params = params)$column_name
}

# drop_index methods
S7::method(drop_index, db_default) <- function(connection, table, index) {
  # DBTable_v9 hands this method a DBI::Id, and glue::glue() cannot coerce
  # one. index_table_text() gives the same string the caller used to pass.
  table <- index_table_text(table)
  try(
    DBI::dbExecute(
      connection,
      glue::glue("ALTER TABLE `{table}` DROP INDEX `{index}`")
    ),
    TRUE
  )
}

S7::method(drop_index, db_mssql) <- function(connection, table, index) {
  table <- index_table_text(table)
  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX {table}.{index}")
    ),
    TRUE
  )
}

# Drop a PostgreSQL index.
#
# The statement names the SCHEMA. `DROP INDEX IF EXISTS ind1` resolves the
# name through search_path, and csdb creates every index on a fully specified
# table. The index lands in that table's schema, so an unqualified drop finds
# it only when the schema is on the path. The try() below then hides the miss,
# and the index stays.
#
# The method took `table` and ignored it until 2026.8.16. It uses it now: the
# schema is the PENULTIMATE component of the table identity, and not every
# component except the last. The two differ on `catalog.schema.table`, where
# the penultimate component is `schema` alone. That is what PostgreSQL wants
# here, because an index lives in a schema and a DROP INDEX names
# `schema.index`. A catalog-qualified index name is not valid there.
#
# The schema and the index are quoted separately, and never pasted in raw.
# `DROP INDEX IF EXISTS an.on.ix_1` names schema `an` and index `on.ix_1`,
# which is a different index, and the try() below hides the miss.
S7::method(drop_index, db_postgres) <- function(connection, table, index) {
  parts <- index_table_identity(table)
  schema <- if (length(parts) > 1L) parts[length(parts) - 1L] else ""
  index_quoted <- as.character(DBI::dbQuoteIdentifier(connection, index))
  target <- if (nzchar(schema)) {
    paste0(
      as.character(DBI::dbQuoteIdentifier(connection, schema)),
      ".",
      index_quoted
    )
  } else {
    index_quoted
  }

  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX IF EXISTS {target}")
    ),
    TRUE
  )
}

# Drop a SQLite index.
#
# In SQLite an index belongs to the schema, not to the table, so the statement
# names the index alone: `DROP INDEX <index> ON <table>` is a syntax error.
# `IF EXISTS` makes dropping an absent index a no-op, which is why there is no
# try() here where the other three backends have one.
#
# `table` is accepted and ignored, because drop_index() is one generic and the
# other three methods need it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(drop_index, db_sqlite) <- function(connection, table, index) {
  DBI::dbExecute(
    connection,
    paste0(
      "DROP INDEX IF EXISTS ",
      DBI::dbQuoteIdentifier(connection, index)
    )
  )
}

# add_index methods
#
# A method here either creates the index or raises. It never returns the
# failure as a value.
#
# The db_default and db_postgres methods wrapped their DBI::dbExecute() call
# in try(..., T) until 2026.8.16. Every failure then came back as a
# `try-error` object that no caller reads. The PostgreSQL upsert method asked
# for an index named `"ind" + random_uuid()`. That expression raises in R, so
# the method built every temporary table with no index. The caller could not
# tell any of that apart from success.
#
# One failure stayed silent after that change, and it was the largest one. A
# PostgreSQL index name is unique per SCHEMA. `CREATE INDEX IF NOT EXISTS`
# answers a taken name with a notice, not an error. Measured on the
# norsyss_data1 database on 2026-08-15: `anon_norsyss_data` had 87 partitions.
# All 87 asked for the same two index names. One partition held `ind2`, and
# none held `ind1`. Removing try() does not reach that. index_physical_name()
# does. The name now carries the table identity, so two tables in one schema
# no longer ask for one name.
#
# db_mssql still wraps its call. Its SQL is invalid and its only caller passes
# no index name, so removing the wrapper there needs a SQL Server to verify
# against. That work is deferred, and the wrapper marks it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(add_index, db_default) <- function(connection, table, index, keys) {
  # DBTable_v9 hands this method a DBI::Id, and glue::glue() cannot coerce
  # one. index_table_text() gives the same string the caller used to pass.
  table <- index_table_text(table)
  keys <- glue::glue_collapse(keys, sep = ", ")

  sql <- glue::glue(
    "
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;"
  )
  DBI::dbExecute(connection, sql)
}

S7::method(add_index, db_mssql) <- function(connection, table, index, keys) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX {index} IF NOT EXISTS ON {table} ({keys});")
    ),
    T
  )
}

# Create a PostgreSQL index.
#
# `table` MUST arrive as a DBI::Id, or as one unqualified name. The method
# quotes it here rather than take it pre-quoted. A caller that hands over
# `"anon.tab"` as text gets one identifier called `anon.tab`, which is a
# different table.
#
# Every identifier is quoted: the index, the table and each key column. csdb
# built this statement by pasting all three in raw until 2026.8.16. A name
# holding a dot, a space or an upper case letter then produced SQL that
# PostgreSQL read as something else, or rejected. `anon.my tab` is a syntax
# error, and `anon.MyTab` silently folds to `anon.mytab`.
#
# Quoting the index name changes nothing that csdb generates.
# index_physical_name() emits `[a-z][a-z0-9_]*`, and the upsert method below
# emits `ind` plus random_uuid(), which is also lower case and alphanumeric.
# PostgreSQL folds an unquoted name of that shape to itself.
S7::method(add_index, db_postgres) <- function(connection, table, index, keys) {
  keys_quoted <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE INDEX IF NOT EXISTS ",
      DBI::dbQuoteIdentifier(connection, index),
      " ON ",
      DBI::dbQuoteIdentifier(connection, table),
      " (",
      keys_quoted,
      ")"
    )
  )
}

# Create a SQLite index.
#
# The table name MUST be unqualified. SQLite lets the INDEX name carry a
# schema, but never the table: `CREATE INDEX ind ON main.tab (a)` is
# `near ".": syntax error`. Under this package's SQLite arm the value arriving
# is already the bare table name, so the method simply does not re-qualify it.
#
# `IF NOT EXISTS` makes re-adding an existing index a no-op, and, crucially,
# leaves `PRAGMA schema_version` untouched when it does nothing.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(add_index, db_sqlite) <- function(connection, table, index, keys) {
  keys_quoted <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE INDEX IF NOT EXISTS ",
      DBI::dbQuoteIdentifier(connection, index),
      " ON ",
      DBI::dbQuoteIdentifier(connection, table),
      " (",
      keys_quoted,
      ")"
    )
  )
}

# drop_all_rows methods
#
# This was a plain function until SQLite arrived. The body below is the whole
# of that function, unchanged, so SQL Server and PostgreSQL still receive the
# byte-identical TRUNCATE TABLE statement they always did.
S7::method(drop_all_rows, db_default) <- function(connection, table) {
  a <- DBI::dbExecute(
    connection,
    glue::glue({
      "TRUNCATE TABLE {table};"
    })
  )
}

# SQLite has no TRUNCATE: `TRUNCATE TABLE tab` is `near "TRUNCATE": syntax
# error`. A bare DELETE is the documented equivalent, and it leaves the
# primary key and every index intact, which matters because the SQLite
# add_constraint method cannot put a primary key back.
S7::method(drop_all_rows, db_sqlite) <- function(connection, table) {
  DBI::dbExecute(
    connection,
    paste0("DELETE FROM ", DBI::dbQuoteIdentifier(connection, table))
  )
}

# drop_rows_where methods
S7::method(drop_rows_where, db_mssql) <- function(
  connection,
  table,
  condition
) {
  t0 <- Sys.time()

  numrows <- DBI::dbGetQuery(
    connection,
    glue::glue(
      "SELECT COUNT(*) FROM {table} WHERE {condition};"
    )
  ) |>
    as.numeric()

  num_deleting <- 100000
  num_deleting_character <- formatC(
    num_deleting,
    format = "f",
    drop0trailing = T
  )
  num_delete_calls <- ceiling(numrows / num_deleting)

  indexes <- csutil::easy_split(1:num_delete_calls, number_of_groups = 10)
  notify_indexes <- unlist(lapply(indexes, max))

  i <- 0
  while (numrows > 0) {
    b <- DBI::dbExecute(
      connection,
      glue::glue(
        "DELETE TOP ({num_deleting_character}) FROM {table} WHERE {condition}; ",
        "CHECKPOINT; "
      )
    )

    numrows <- DBI::dbGetQuery(
      connection,
      glue::glue(
        "SELECT COUNT(*) FROM {table} WHERE {condition};"
      )
    ) |>
      as.numeric()
    i <- i + 1
  }

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(drop_rows_where, db_postgres) <- function(
  connection,
  table,
  condition
) {
  t0 <- Sys.time()

  sql <- glue::glue("delete from {table} where {condition};")

  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(drop_rows_where, db_sqlite) <- function(
  connection,
  table,
  condition
) {
  DBI::dbExecute(
    connection,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(connection, table),
      " WHERE ",
      condition
    )
  )
}

# keep_rows_where methods
S7::method(keep_rows_where, db_mssql) <- function(
  connection,
  table,
  condition,
  role_create_table = NULL
) {
  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  DBI::dbExecute(connection, sql)

  DBI::dbRemoveTable(connection, name = table)

  sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table}'")
  DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(keep_rows_where, db_postgres) <- function(
  connection,
  table,
  condition,
  role_create_table = NULL
) {
  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  if (!is.na(role_create_table)) {
    if (role_create_table != "x") {
      sql <- paste0(
        "SET ROLE ",
        DBI::dbQuoteIdentifier(connection, role_create_table),
        "; ",
        sql,
        "; RESET ROLE"
      )
    }
  }
  DBI::dbExecute(connection, sql)

  sql <- glue::glue("DROP TABLE {table}")
  if (!is.na(role_create_table)) {
    if (role_create_table != "x") {
      sql <- paste0(
        "SET ROLE ",
        DBI::dbQuoteIdentifier(connection, role_create_table),
        "; ",
        sql,
        "; RESET ROLE"
      )
    }
  }
  DBI::dbExecute(connection, sql)

  sql <- glue::glue("ALTER TABLE {temp_name} RENAME TO {table}")
  if (!is.na(role_create_table)) {
    if (role_create_table != "x") {
      sql <- paste0(
        "SET ROLE ",
        DBI::dbQuoteIdentifier(connection, role_create_table),
        "; ",
        sql,
        "; RESET ROLE"
      )
    }
  }
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

# Keep only the rows a SQLite table's condition holds for.
#
# The predicate is `(<condition>) IS NOT TRUE`, and the parentheses and the
# `IS NOT TRUE` are both mandatory. `NOT (<condition>)` is NOT the inverse of
# `WHERE <condition>` in SQL: DELETE removes only rows whose predicate
# evaluates to TRUE, and the negation of NULL is NULL, so every row on which
# the condition is NULL would survive a plain negation even though
# `SELECT ... WHERE <condition>` would not have kept it. `IS NOT TRUE` folds
# NULL into FALSE and gives the exact complement.
#
# This is a DELETE rather than the drop-and-rename the other two backends use,
# because that would discard the primary key, and the SQLite add_constraint
# method cannot add one back.
#
# `role_create_table` is accepted and ignored: SQLite has no roles.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(keep_rows_where, db_sqlite) <- function(
  connection,
  table,
  condition,
  role_create_table = NULL
) {
  DBI::dbExecute(
    connection,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(connection, table),
      " WHERE (",
      condition,
      ") IS NOT TRUE"
    )
  )
}

# drop_table methods
S7::method(drop_table, db_mssql) <- function(
  connection,
  table,
  role_create_table = NULL
) {
  return(try(DBI::dbRemoveTable(connection, name = table), TRUE))
}

S7::method(drop_table, db_postgres) <- function(
  connection,
  table,
  role_create_table = NULL
) {
  sql <- glue::glue("DROP TABLE {table}")
  if (!is.na(role_create_table)) {
    if (role_create_table != "x") {
      sql <- paste0(
        "SET ROLE ",
        DBI::dbQuoteIdentifier(connection, role_create_table),
        "; ",
        sql,
        "; RESET ROLE"
      )
    }
  }

  return(try(DBI::dbExecute(connection, sql), TRUE))
}
