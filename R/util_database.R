# The S7 database layer for csdb: the utility functions, the S4 class
# registration, the db_* class objects and the S7 generics.
#
# The method assignments live in the sibling files, one file per group of
# generics: util_database_load.R, util_database_table.R, util_database_index.R
# and util_database_rows.R. R sources this directory in C collation order.
# Each of those names sorts after this one, so every generic and every class
# exists before the method assignments run.

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
