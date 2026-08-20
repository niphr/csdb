# The S7 method assignments for create_table, add_constraint and
# drop_constraint.
#
# The generics and the db_* class objects are in "util_database.R". R
# sources this directory in C collation order. This name sorts after that
# one, so every generic and class exists before the assignments below run.

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
