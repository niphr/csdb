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
    row.names = FALSE,
    temporary = FALSE
  )
  return(DBI::dbExecute(connection, sql))
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
    row.names = FALSE,
    temporary = FALSE
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")
  return(DBI::dbExecute(connection, sql))
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
    row.names = FALSE,
    temporary = FALSE
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

  return(DBI::dbExecute(connection, sql))
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
      ".",
      call. = FALSE
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
  return(DBI::dbExecute(connection, sql))
}

# The SQL text of a table identity, with every component quoted.
#
# index_table_identity() splits the identity into its components. Each
# component is quoted on its own, and the quoted components are joined with a
# dot. Schema `s q` and table `t"x` therefore give `"s q"."t""x"` on
# PostgreSQL. Pasted in raw, the space and the quote break the statement or
# change what it does.
#
# The dotted text form cannot carry a dot inside a component. Only a DBI::Id
# can.
#
# connection  The connection whose quoting rule applies.
# table       The table identity: text, or a DBI::Id.
# returns     One character string.
quote_table_identity <- function(connection, table) {
  return(paste0(
    as.character(DBI::dbQuoteIdentifier(
      connection,
      index_table_identity(table)
    )),
    collapse = "."
  ))
}

# add_constraint methods
#
# The db_default method quotes the table and the constraint name. SQL Server
# has its own method below, which keeps the statement it always received.
S7::method(add_constraint, db_default) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  table_quoted <- quote_table_identity(connection, table)
  constraint <- as.character(
    DBI::dbQuoteIdentifier(connection, pk_physical_name(table))
  )
  sql <- glue::glue(
    "
          ALTER table {table_quoted}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});"
  )
  a <- DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
}

# The SQL Server statement stays unquoted. DBTable_v9 documents the SQL Server
# identity as `[db].[dbo].[table_name]`, with each component already in square
# brackets. Quoting such a component again names a different object.
S7::method(add_constraint, db_mssql) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- pk_physical_name(table)
  sql <- glue::glue(
    "
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});"
  )
  a <- DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  return(invisible(dif))
}

S7::method(add_constraint, db_postgres) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  table_quoted <- quote_table_identity(connection, table)
  constraint <- as.character(
    DBI::dbQuoteIdentifier(connection, pk_physical_name(table))
  )
  sql <- glue::glue(
    "ALTER table {table_quoted}
    ADD CONSTRAINT {constraint}
    PRIMARY KEY ({primary_keys});"
  )

  a <- DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
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
  return(invisible(NULL))
}

# drop_constraint methods
#
# PostgreSQL has no drop_constraint method of its own, so it reaches the
# db_default method. SQL Server keeps its unquoted statement, for the reason
# given at the db_mssql add_constraint method.
S7::method(drop_constraint, db_default) <- function(connection, table) {
  table_quoted <- quote_table_identity(connection, table)
  constraint <- as.character(
    DBI::dbQuoteIdentifier(connection, pk_physical_name(table))
  )
  sql <- glue::glue(
    "
          ALTER table {table_quoted}
          DROP CONSTRAINT {constraint};"
  )
  return(try(a <- DBI::dbExecute(connection, sql), TRUE))
}

S7::method(drop_constraint, db_mssql) <- function(connection, table) {
  constraint <- pk_physical_name(table)
  sql <- glue::glue(
    "
          ALTER table {table}
          DROP CONSTRAINT {constraint};"
  )
  return(try(a <- DBI::dbExecute(connection, sql), TRUE))
}
