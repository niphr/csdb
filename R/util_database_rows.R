# The S7 method assignments for drop_all_rows, drop_rows_where,
# keep_rows_where and drop_table.
#
# The generics and the db_* class objects are in "util_database.R". R
# sources this directory in C collation order. This name sorts after that
# one, so every generic and class exists before the assignments below run.

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
