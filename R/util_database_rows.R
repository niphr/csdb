# The S7 method assignments for drop_all_rows, drop_rows_where,
# keep_rows_where and drop_table.
#
# The generics and the db_* class objects are in "util_database.R". R
# sources this directory in C collation order. This name sorts after that
# one, so every generic and class exists before the assignments below run.

# Wrap one PostgreSQL statement in SET ROLE and RESET ROLE.
#
# The statement runs unchanged when role_create_table is NULL, NA or "x".
# DBConnection_v9 stores "x" for "no role". The public keep_rows_where() calls
# the generic with three arguments, so the method receives NULL. The test
# `!is.na(role_create_table)` was then `if (logical(0))`, and that is an error.
#
# connection         The connection whose quoting rule applies.
# sql                One SQL statement.
# role_create_table  NULL, NA, "x", or the role to take.
# returns            One character string.
postgres_with_create_role <- function(connection, sql, role_create_table) {
  if (
    is.null(role_create_table) ||
      is.na(role_create_table) ||
      role_create_table == "x"
  ) {
    return(sql)
  }
  return(paste0(
    "SET ROLE ",
    DBI::dbQuoteIdentifier(connection, role_create_table),
    "; ",
    sql,
    "; RESET ROLE"
  ))
}

# drop_all_rows methods
#
# This was a plain function until SQLite arrived. The db_default method now
# quotes the table, and PostgreSQL reaches it. SQL Server has its own method,
# which sends the byte-identical TRUNCATE TABLE statement it always did. The
# db_mssql add_constraint method in util_database_table.R gives the reason.
S7::method(drop_all_rows, db_default) <- function(connection, table) {
  return(a <- DBI::dbExecute(
    connection,
    paste0("TRUNCATE TABLE ", quote_table_identity(connection, table), ";")
  ))
}

S7::method(drop_all_rows, db_mssql) <- function(connection, table) {
  a <- DBI::dbExecute(
    connection,
    glue::glue({
      "TRUNCATE TABLE {table};"
    })
  )
  return(invisible(a))
}

# SQLite has no TRUNCATE: `TRUNCATE TABLE tab` is `near "TRUNCATE": syntax
# error`. A bare DELETE is the documented equivalent, and it leaves the
# primary key and every index intact, which matters because the SQLite
# add_constraint method cannot put a primary key back.
S7::method(drop_all_rows, db_sqlite) <- function(connection, table) {
  return(DBI::dbExecute(
    connection,
    paste0("DELETE FROM ", DBI::dbQuoteIdentifier(connection, table))
  ))
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
    drop0trailing = TRUE
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
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
}

S7::method(drop_rows_where, db_postgres) <- function(
  connection,
  table,
  condition
) {
  t0 <- Sys.time()

  sql <- paste0(
    "delete from ",
    quote_table_identity(connection, table),
    " where ",
    condition,
    ";"
  )

  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
}

S7::method(drop_rows_where, db_sqlite) <- function(
  connection,
  table,
  condition
) {
  return(DBI::dbExecute(
    connection,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(connection, table),
      " WHERE ",
      condition
    )
  ))
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
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
}

# Keep only the rows of a PostgreSQL table that the condition holds for.
#
# The method copies the kept rows to a new table, drops the source and renames
# the copy. Three details are load-bearing:
#
#  1. The copy is in the schema of the source. An unqualified copy lands in
#     the first schema on search_path, and the rename keeps it there.
#  2. The rename target is the bare table name. PostgreSQL rejects a
#     schema-qualified name after RENAME TO.
#  3. The three statements run in one transaction. PostgreSQL DDL is
#     transactional, so a failure after the DROP rolls the DROP back, and the
#     source table stays intact.
S7::method(keep_rows_where, db_postgres) <- function(
  connection,
  table,
  condition,
  role_create_table = NULL
) {
  t0 <- Sys.time()
  parts <- index_table_identity(table)
  temp_parts <- c(parts[-length(parts)], paste0("tmp", random_uuid()))

  table_quoted <- quote_table_identity(connection, table)
  temp_quoted <- paste0(
    as.character(DBI::dbQuoteIdentifier(connection, temp_parts)),
    collapse = "."
  )
  name_quoted <- as.character(
    DBI::dbQuoteIdentifier(connection, parts[length(parts)])
  )

  statements <- c(
    paste0(
      "SELECT * INTO ",
      temp_quoted,
      " FROM ",
      table_quoted,
      " WHERE ",
      condition
    ),
    paste0("DROP TABLE ", table_quoted),
    paste0("ALTER TABLE ", temp_quoted, " RENAME TO ", name_quoted)
  )

  DBI::dbWithTransaction(connection, {
    for (sql in statements) {
      DBI::dbExecute(
        connection,
        postgres_with_create_role(connection, sql, role_create_table)
      )
    }
  })

  t1 <- Sys.time()
  return(dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1))
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
  return(DBI::dbExecute(
    connection,
    paste0(
      "DELETE FROM ",
      DBI::dbQuoteIdentifier(connection, table),
      " WHERE (",
      condition,
      ") IS NOT TRUE"
    )
  ))
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
  sql <- postgres_with_create_role(
    connection,
    paste0("DROP TABLE ", quote_table_identity(connection, table)),
    role_create_table
  )

  return(try(DBI::dbExecute(connection, sql), TRUE))
}
