# The SQL layer of the profile engine.
#
# Every statistic in a profile comes from one of the aggregate queries in this
# file. Each query wraps the caller's SQL as `SELECT ... FROM (<sql>) q`. The
# queries use only COUNT, SUM(CASE WHEN ...), MIN, MAX, GROUP BY and COUNT(*)
# over SELECT DISTINCT, so the same text runs on SQLite, PostgreSQL and SQL
# Server. SQLite has no multi-column COUNT(DISTINCT a, b), which is why the
# distinct counts use a subquery.

# Quote one or more column names for this connection.
profile_quote <- function(con, x) {
  return(as.character(DBI::dbQuoteIdentifier(con, x)))
}

# The FROM clause that every profile query shares.
profile_from <- function(sql) {
  return(paste0(" FROM (", sql, ") q"))
}

# A WHERE clause that drops the rows where any of `quoted` is NULL.
profile_where_present <- function(quoted) {
  return(paste0(
    " WHERE ",
    paste0(quoted, " IS NOT NULL", collapse = " AND ")
  ))
}

# A number as a SQL literal, with enough digits to give back the same double.
profile_sql_number <- function(x) {
  return(sprintf("%.17g", x))
}

# A count from a query result as an integer. SUM() over no rows gives NULL,
# which is a count of 0.
profile_as_count <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  if (any(x > .Machine$integer.max)) {
    stop("A count is too large to hold as an integer.", call. = FALSE)
  }
  return(as.integer(round(x)))
}

# The R class of each column of a zero-row fetch, as a csdb field type.
sql_field_types <- function(con, sql) {
  empty <- DBI::dbGetQuery(
    con,
    paste0("SELECT *", profile_from(sql), " WHERE 1 = 0")
  )
  types <- vapply(empty, profile_type_of, character(1))
  names(types) <- names(empty)
  return(types)
}

profile_type_of <- function(x) {
  if (inherits(x, "POSIXct")) {
    return("DATETIME")
  }
  if (inherits(x, "Date")) {
    return("DATE")
  }
  type <- c(
    character = "TEXT",
    integer = "INTEGER",
    integer64 = "INTEGER",
    numeric = "DOUBLE",
    logical = "BOOLEAN"
  )[class(x)[1]]
  if (is.na(type)) {
    stop(
      "Cannot infer a field type from the R class '",
      class(x)[1],
      "'. Give field_types.",
      call. = FALSE
    )
  }
  return(unname(type))
}

# The row count, and the count of missing values in each of `columns`.
sql_missing_counts <- function(con, sql, columns) {
  quoted <- profile_quote(con, columns)
  sums <- paste0(
    ", SUM(CASE WHEN ",
    quoted,
    " IS NULL THEN 1 ELSE 0 END) AS m_",
    seq_along(columns),
    collapse = ""
  )
  if (length(columns) == 0) {
    sums <- ""
  }
  r <- DBI::dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n_rows", sums, profile_from(sql))
  )
  missing <- profile_as_count(unlist(r[1, -1], use.names = FALSE))
  names(missing) <- columns
  return(list(n_rows = profile_as_count(r$n_rows), missing = missing))
}

# The number of distinct combinations of `columns`. With `present = TRUE`,
# rows where any of `columns` is NULL are dropped first.
sql_count_distinct <- function(con, sql, columns, present = TRUE) {
  quoted <- profile_quote(con, columns)
  where <- ""
  if (present) {
    where <- profile_where_present(quoted)
  }
  inner <- paste0(
    "SELECT DISTINCT ",
    paste0(quoted, collapse = ", "),
    profile_from(sql),
    where
  )
  r <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM (", inner, ") z"))
  return(profile_as_count(r$n))
}

# The row count of each combination of `columns`, NULLs dropped. The result
# holds the columns by position, then the count `n`. The columns are not
# aliased, so SQLite keeps their declared type and RSQLite gives back a Date
# or a POSIXct.
sql_value_counts <- function(con, sql, columns) {
  quoted <- paste0(profile_quote(con, columns), collapse = ", ")
  r <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      quoted,
      ", COUNT(*) AS n",
      profile_from(sql),
      profile_where_present(profile_quote(con, columns)),
      " GROUP BY ",
      quoted
    )
  )
  return(r)
}

# The minimum and the maximum of one numeric column.
sql_range <- function(con, sql, column) {
  quoted <- profile_quote(con, column)
  r <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT MIN(",
      quoted,
      ") AS lo, MAX(",
      quoted,
      ") AS hi",
      profile_from(sql)
    )
  )
  return(c(lo = as.numeric(r$lo), hi = as.numeric(r$hi)))
}

# The row count of each half-open bin [breaks[i], breaks[i + 1]).
sql_bin_counts <- function(con, sql, column, breaks) {
  quoted <- profile_quote(con, column)
  m <- length(breaks) - 1L
  lower <- profile_sql_number(breaks[seq_len(m)])
  upper <- profile_sql_number(breaks[seq_len(m) + 1L])
  sums <- paste0(
    "SUM(CASE WHEN ",
    quoted,
    " >= ",
    lower,
    " AND ",
    quoted,
    " < ",
    upper,
    " THEN 1 ELSE 0 END) AS b_",
    seq_len(m),
    collapse = ", "
  )
  r <- DBI::dbGetQuery(con, paste0("SELECT ", sums, profile_from(sql)))
  return(profile_as_count(unlist(r[1, ], use.names = FALSE)))
}

# The number of values of `column` that hold each row count. The values
# themselves never leave the database.
sql_group_sizes <- function(con, sql, column) {
  quoted <- profile_quote(con, column)
  inner <- paste0(
    "SELECT COUNT(*) AS size",
    profile_from(sql),
    profile_where_present(quoted),
    " GROUP BY ",
    quoted
  )
  r <- DBI::dbGetQuery(
    con,
    paste0("SELECT size, COUNT(*) AS n FROM (", inner, ") z GROUP BY size")
  )
  return(r)
}

# snapshot ----

# The SQL Server isolation levels, by the number that
# sys.dm_exec_sessions.transaction_isolation_level holds.
profile_mssql_levels <- c(
  "READ UNCOMMITTED",
  "READ COMMITTED",
  "REPEATABLE READ",
  "SERIALIZABLE",
  "SNAPSHOT"
)

# The database system of `con` for the snapshot, or NA when the function
# knows no snapshot for it.
profile_snapshot_driver <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    return("SQLite")
  }
  driver <- class(con)[1]
  if (inherits(con, "PqConnection")) {
    driver <- "PostgreSQL"
  }
  if (driver %in% c("PostgreSQL", "Microsoft SQL Server")) {
    return(driver)
  }
  return(NA_character_)
}

# The statements around DBI::dbBegin() and DBI::dbRollback() that give one
# snapshot. `before` runs before dbBegin(), `inside` after it and before the
# first read, and `after` after dbRollback(). Each is a transaction-control
# statement. On SQL Server the isolation level is a session setting, so
# `after` puts back `mssql_level`.
profile_snapshot_statements <- function(
  driver,
  mssql_snapshot = FALSE,
  mssql_level = "READ COMMITTED"
) {
  out <- list(before = character(0), inside = character(0), after = character(0))
  if (driver == "PostgreSQL") {
    out$inside <- "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
  }
  if (driver == "Microsoft SQL Server") {
    level <- if (isTRUE(mssql_snapshot)) "SNAPSHOT" else "REPEATABLE READ"
    out$before <- paste0("SET TRANSACTION ISOLATION LEVEL ", level)
    out$after <- paste0("SET TRANSACTION ISOLATION LEVEL ", mssql_level)
  }
  return(out)
}

# Whether the database allows SNAPSHOT isolation, and the isolation level of
# the session. Two SELECTs. When either fails, no SNAPSHOT and READ
# COMMITTED, the SQL Server default.
profile_mssql_state <- function(con) {
  state <- list(snapshot = FALSE, level = "READ COMMITTED")
  try(
    {
      allowed <- DBI::dbGetQuery(
        con,
        "SELECT snapshot_isolation_state AS s FROM sys.databases WHERE name = DB_NAME()"
      )$s
      state$snapshot <- isTRUE(as.integer(allowed) == 1L)
    },
    silent = TRUE
  )
  try(
    {
      level <- DBI::dbGetQuery(
        con,
        "SELECT transaction_isolation_level AS l FROM sys.dm_exec_sessions WHERE session_id = @@SPID"
      )$l
      level <- profile_mssql_levels[as.integer(level)]
      if (length(level) == 1 && !is.na(level)) {
        state$level <- level
      }
    },
    silent = TRUE
  )
  return(state)
}

# Opens one read snapshot on `con`. Returns the statements that close it, or
# NULL when the connection gives no snapshot. A message then says why.
profile_snapshot_begin <- function(con) {
  driver <- profile_snapshot_driver(con)
  if (is.na(driver)) {
    profile_snapshot_message(paste0("csdb knows no snapshot for ", class(con)[1]))
    return(NULL)
  }
  state <- list(snapshot = FALSE, level = "READ COMMITTED")
  if (driver == "Microsoft SQL Server") {
    state <- profile_mssql_state(con)
  }
  plan <- profile_snapshot_statements(driver, state$snapshot, state$level)
  opened <- tryCatch(
    {
      for (s in plan$before) {
        DBI::dbExecute(con, s)
      }
      DBI::dbBegin(con)
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
  if (!isTRUE(opened)) {
    profile_snapshot_run(con, plan$after)
    profile_snapshot_message(opened)
    return(NULL)
  }
  return(profile_snapshot_inside(con, plan))
}

# The `inside` statements of an open transaction. When one fails, the
# transaction is rolled back and the profile runs without a snapshot.
profile_snapshot_inside <- function(con, plan) {
  failed <- tryCatch(
    {
      for (s in plan$inside) {
        DBI::dbExecute(con, s)
      }
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(failed)) {
    profile_snapshot_end(con, plan)
    profile_snapshot_message(failed)
    return(NULL)
  }
  return(plan)
}

# Closes the snapshot. The transaction only reads, so a rollback ends it.
profile_snapshot_end <- function(con, plan) {
  if (is.null(plan)) {
    return(invisible(NULL))
  }
  try(DBI::dbRollback(con), silent = TRUE)
  profile_snapshot_run(con, plan$after)
  return(invisible(NULL))
}

profile_snapshot_run <- function(con, statements) {
  for (s in statements) {
    try(DBI::dbExecute(con, s), silent = TRUE)
  }
  return(invisible(NULL))
}

profile_snapshot_message <- function(reason) {
  message(
    "profile_query() runs without a snapshot (",
    reason,
    "). The check for a torn profile still runs."
  )
  return(invisible(NULL))
}
