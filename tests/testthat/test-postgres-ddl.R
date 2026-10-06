# The PostgreSQL methods below quote the table identity in every statement
# they send. keep_rows_where() either completes or leaves the source table
# intact.
#
# The methods need a PostgreSQL server, so every block calls the S7 method
# directly and captures the SQL it sends. DBI::dbExecute(), DBI::dbGetQuery()
# and DBI::dbWithTransaction() are mocked. DBI::ANSI() is the connection: it
# quotes identifiers with `"` and doubles an inner `"`, as PostgreSQL does.
#
# The identity is schema `s q` with table `t"x`. Pasted in raw, the space
# splits the name, and the quote ends it.

pg_table <- "s q.t\"x"
pg_table_quoted <- "\"s q\".\"t\"\"x\""
pg_con <- DBI::ANSI()

# Run `code` with the three DBI functions mocked.
#
# Every statement is recorded with a flag that says whether it ran inside
# DBI::dbWithTransaction(). The mock keeps the statements of a transaction
# apart until its code finishes. It moves them to `committed` only then, and
# it discards them when the code raises. A statement outside a transaction
# goes to `committed` at once.
#
# fail_on  NULL, or a pattern. A statement that matches it raises an error.
# returns  A list: `sent` (every statement), `in_tx` (one flag per statement),
#          `committed` (the statements that took effect), `error` (the
#          condition message, or NULL).
capture_pg_sql <- function(code, fail_on = NULL) {
  log <- new.env()
  log$sent <- character(0)
  log$in_tx <- logical(0)
  log$committed <- character(0)
  log$pending <- character(0)
  log$depth <- 0L

  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      s <- as.character(statement)
      log$sent <- c(log$sent, s)
      log$in_tx <- c(log$in_tx, log$depth > 0L)
      if (!is.null(fail_on) && grepl(fail_on, s)) {
        stop("injected failure: ", s)
      }
      if (log$depth > 0L) {
        log$pending <- c(log$pending, s)
      } else {
        log$committed <- c(log$committed, s)
      }
      0L
    },
    dbGetQuery = function(conn, statement, ...) {
      log$sent <- c(log$sent, as.character(statement))
      log$in_tx <- c(log$in_tx, log$depth > 0L)
      data.frame(n = 0L)
    },
    dbWithTransaction = function(conn, code, ...) {
      log$depth <- log$depth + 1L
      log$pending <- character(0)
      on.exit(log$depth <- log$depth - 1L)
      tryCatch(
        {
          force(code)
          log$committed <- c(log$committed, log$pending)
          log$pending <- character(0)
        },
        error = function(e) {
          log$pending <- character(0)
          stop(e)
        }
      )
      invisible(TRUE)
    },
    .package = "DBI"
  )

  err <- tryCatch(
    {
      force(code)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  return(list(
    sent = log$sent,
    in_tx = log$in_tx,
    committed = log$committed,
    error = err
  ))
}

# One statement names the quoted identity, and the raw name appears nowhere
# outside it.
expect_quoted_identity <- function(statement) {
  expect_match(statement, pg_table_quoted, fixed = TRUE)
  rest <- gsub(pg_table_quoted, "", statement, fixed = TRUE)
  expect_false(grepl("t\"x", rest, fixed = TRUE))
  expect_false(grepl("s q", rest, fixed = TRUE))
}

test_that("quote_table_identity() quotes each component on its own", {
  expect_identical(quote_table_identity(pg_con, pg_table), pg_table_quoted)
  # Only a DBI::Id can carry a dot inside a component.
  expect_identical(
    quote_table_identity(pg_con, DBI::Id(schema = "a.b", table = "c")),
    "\"a.b\".\"c\""
  )
  expect_identical(quote_table_identity(pg_con, "tab"), "\"tab\"")
})

test_that("the db_default add_constraint quotes the table and the constraint", {
  res <- capture_pg_sql(
    S7::method(add_constraint, db_default)(pg_con, pg_table, "n")
  )
  expect_length(res$sent, 1L)
  expect_quoted_identity(res$sent)
  expect_match(
    res$sent,
    paste0("ADD CONSTRAINT \"", pk_physical_name(pg_table), "\""),
    fixed = TRUE
  )
})

test_that("the PostgreSQL add_constraint quotes the table and the constraint", {
  res <- capture_pg_sql(
    S7::method(add_constraint, db_postgres)(pg_con, pg_table, "n")
  )
  expect_length(res$sent, 1L)
  expect_quoted_identity(res$sent)
  expect_match(
    res$sent,
    paste0("ADD CONSTRAINT \"", pk_physical_name(pg_table), "\""),
    fixed = TRUE
  )
})

test_that("the db_default drop_constraint quotes the table and the constraint", {
  # PostgreSQL has no drop_constraint method, so it reaches this one.
  expect_identical(
    S7::method(drop_constraint, db_postgres),
    S7::method(drop_constraint, db_default)
  )
  res <- capture_pg_sql(
    S7::method(drop_constraint, db_default)(pg_con, pg_table)
  )
  expect_length(res$sent, 1L)
  expect_quoted_identity(res$sent)
  expect_match(
    res$sent,
    paste0("DROP CONSTRAINT \"", pk_physical_name(pg_table), "\""),
    fixed = TRUE
  )
})

test_that("the db_default drop_all_rows quotes the table", {
  # PostgreSQL has no drop_all_rows method, so it reaches this one.
  expect_identical(
    S7::method(drop_all_rows, db_postgres),
    S7::method(drop_all_rows, db_default)
  )
  res <- capture_pg_sql(
    S7::method(drop_all_rows, db_default)(pg_con, pg_table)
  )
  expect_identical(res$sent, paste0("TRUNCATE TABLE ", pg_table_quoted, ";"))
})

test_that("the PostgreSQL drop_rows_where quotes the table", {
  res <- capture_pg_sql(
    S7::method(drop_rows_where, db_postgres)(pg_con, pg_table, "n > 2")
  )
  expect_identical(
    res$sent,
    paste0("delete from ", pg_table_quoted, " where n > 2;")
  )
})

test_that("the PostgreSQL drop_table quotes the table", {
  res <- capture_pg_sql(
    S7::method(drop_table, db_postgres)(pg_con, pg_table)
  )
  expect_identical(res$sent, paste0("DROP TABLE ", pg_table_quoted))
  expect_null(res$error)
})

# The three statements keep_rows_where() sends, for one temporary name.
keep_rows_statements <- function(sent) {
  temp <- regmatches(sent[1], regexpr("\"tmp[0-9a-z]+\"", sent[1]))
  return(list(
    temp = temp,
    expected = c(
      paste0(
        "SELECT * INTO \"s q\".",
        temp,
        " FROM ",
        pg_table_quoted,
        " WHERE n <= 2"
      ),
      paste0("DROP TABLE ", pg_table_quoted),
      paste0("ALTER TABLE \"s q\".", temp, " RENAME TO \"t\"\"x\"")
    )
  ))
}

test_that("the PostgreSQL keep_rows_where quotes every name, in one transaction", {
  # The public DBTable_v9$keep_rows_where() calls the generic with three
  # arguments, so the method receives role_create_table = NULL.
  res <- capture_pg_sql(
    S7::method(keep_rows_where, db_postgres)(pg_con, pg_table, "n <= 2")
  )
  expect_null(res$error)
  expect_length(res$sent, 3L)
  k <- keep_rows_statements(res$sent)
  expect_length(k$temp, 1L)
  # The copy is in the schema of the source, and the rename target is the bare
  # table name.
  expect_identical(res$sent, k$expected)
  expect_identical(res$in_tx, c(TRUE, TRUE, TRUE))
  expect_identical(res$committed, k$expected)
})

test_that("the PostgreSQL keep_rows_where accepts every form of no role", {
  for (role in list(NULL, NA, NA_character_, "x")) {
    res <- NULL
    expect_no_error(
      res <- capture_pg_sql(
        S7::method(keep_rows_where, db_postgres)(
          pg_con,
          pg_table,
          "n <= 2",
          role_create_table = role
        )
      )
    )
    expect_null(res$error)
    expect_identical(res$sent, keep_rows_statements(res$sent)$expected)
  }

  # The three-argument call that DBTable_v9 makes.
  expect_no_error(
    res <- capture_pg_sql(
      S7::method(keep_rows_where, db_postgres)(pg_con, pg_table, "n <= 2")
    )
  )
  expect_null(res$error)
  expect_length(res$sent, 3L)
})

test_that("the PostgreSQL keep_rows_where takes a role around each statement", {
  res <- capture_pg_sql(
    S7::method(keep_rows_where, db_postgres)(
      pg_con,
      pg_table,
      "n <= 2",
      role_create_table = "r w"
    )
  )
  expect_null(res$error)
  expected <- keep_rows_statements(res$sent)$expected
  expect_identical(
    res$sent,
    paste0("SET ROLE \"r w\"; ", expected, "; RESET ROLE")
  )
  expect_identical(res$in_tx, c(TRUE, TRUE, TRUE))
})

test_that("a failure before the rename leaves the source table intact", {
  # The rename fails after the DROP has run. Inside the transaction the DROP
  # rolls back. Without it the source table is gone and the copy keeps its
  # temporary name.
  res <- capture_pg_sql(
    S7::method(keep_rows_where, db_postgres)(pg_con, pg_table, "n <= 2"),
    fail_on = "RENAME TO"
  )
  expect_match(res$error, "injected failure", fixed = TRUE)
  expect_length(res$sent, 3L)
  expect_false(any(grepl("^DROP TABLE", res$committed)))
  expect_length(res$committed, 0L)
})

test_that("SQL Server keeps the statements it always received", {
  # DBTable_v9 documents the SQL Server identity as `[db].[dbo].[table_name]`,
  # with each component already in square brackets. These three methods
  # therefore do not quote it.
  res <- capture_pg_sql(
    S7::method(drop_all_rows, db_mssql)(pg_con, "[db].[dbo].[tab]")
  )
  expect_identical(res$sent, "TRUNCATE TABLE [db].[dbo].[tab];")

  res <- capture_pg_sql(
    S7::method(add_constraint, db_mssql)(pg_con, "tab", c("a", "b"))
  )
  expect_match(
    res$sent,
    paste0(
      "ALTER table tab\n *ADD CONSTRAINT ",
      pk_physical_name("tab"),
      " PRIMARY KEY CLUSTERED \\(a, b\\);$"
    )
  )

  res <- capture_pg_sql(
    S7::method(drop_constraint, db_mssql)(pg_con, "tab")
  )
  expect_match(
    res$sent,
    paste0("ALTER table tab\n *DROP CONSTRAINT ", pk_physical_name("tab"), ";$")
  )
})
