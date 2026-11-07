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
