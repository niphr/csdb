# The csdb field type DOUBLE is a PostgreSQL double precision column. Until
# 2026.10.16 csdb created real, which keeps about 7 significant digits.
#
# A table that exists keeps its rows. A real column declared DOUBLE counts as
# matching. With pg_widen_doubles = TRUE, DBTable_v9$create_table() widens
# it in place, with one ALTER TABLE in a transaction with lock_timeout. It
# still drops and creates a table whose column names differ.
#
# No PostgreSQL server is needed. DBI::ANSI() is the connection, and the DBI
# functions that send SQL are mocked.

# Run `code` with DBI::dbExecute(), DBI::dbGetQuery() and
# DBI::dbWithTransaction() mocked. dbGetQuery() returns `live`, the columns
# of the table as PostgreSQL lists them. The transaction is recorded as BEGIN
# and COMMIT, or BEGIN and ROLLBACK. A statement that matches `fail_on`
# raises an error.
#
# returns  A list: `sent`, every statement, `value`, the value of `code`,
#          `messages` and `error`, the condition message or NULL.
capture_pg_double_sql <- function(code, live = NULL, fail_on = NULL) {
  log <- new.env()
  log$sent <- character(0)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      s <- as.character(statement)
      log$sent <- c(log$sent, s)
      if (!is.null(fail_on) && grepl(fail_on, s)) {
        stop("injected failure: canceling statement due to lock timeout")
      }
      return(0L)
    },
    dbGetQuery = function(conn, statement, ...) {
      log$sent <- c(log$sent, as.character(statement))
      return(live)
    },
    dbWithTransaction = function(conn, code, ...) {
      log$sent <- c(log$sent, "BEGIN")
      tryCatch(force(code), error = function(e) {
        log$sent <- c(log$sent, "ROLLBACK")
        stop(e)
      })
      log$sent <- c(log$sent, "COMMIT")
      return(invisible(TRUE))
    },
    .package = "DBI"
  )
  messages <- character(0)
  value <- NULL
  error <- tryCatch(
    {
      value <- withCallingHandlers(force(code), message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      })
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  return(list(
    sent = log$sent,
    value = value,
    messages = messages,
    error = error
  ))
}

# A table identity that no other test uses, so the message of each test is
# its first one for that table in this session.
pg_double_table <- function() {
  return(DBI::Id(schema = "anon", table = paste0("t", random_uuid())))
}

pg_double_live <- function(name, type) {
  return(data.frame(name = name, type = type))
}

test_that("the PostgreSQL create_table makes double precision for DOUBLE", {
  res <- capture_pg_double_sql(
    S7::method(create_table, db_postgres)(
      DBI::ANSI(),
      "tab",
      c(a = "TEXT", x = "DOUBLE", n = "INTEGER"),
      keys = "a",
      role_create_table = "x"
    )
  )
  expect_length(res$sent, 1L)
  expect_match(res$sent, "\"x\" DOUBLE PRECISION,", fixed = TRUE)
  expect_false(grepl("REAL", res$sent, fixed = TRUE))
  expect_match(res$sent, "\"n\" INTEGER", fixed = TRUE)
})

test_that("the field decision widens a real column that is declared DOUBLE", {
  declared <- c(a = "TEXT", x = "DOUBLE", y = "DOUBLE", n = "INTEGER")
  expect_identical(
    table_fields_decision(
      declared,
      names(declared),
      c("character varying", "real", "real", "integer")
    ),
    list(action = "widen", columns = c("x", "y"))
  )
  expect_identical(
    table_fields_decision(
      declared,
      names(declared),
      c("character varying", "double precision", "real", "integer")
    ),
    list(action = "widen", columns = "y")
  )
})

test_that("the field decision keeps the old result for every other difference", {
  declared <- c(a = "TEXT", x = "DOUBLE")
  keep <- list(action = "keep", columns = character(0))
  recreate <- list(action = "recreate", columns = character(0))
  # The columns match.
  expect_identical(
    table_fields_decision(
      declared,
      c("a", "x"),
      c("character varying", "double precision")
    ),
    keep
  )
  # Another type difference. The decision compares names only, as before.
  expect_identical(
    table_fields_decision(declared, c("a", "x"), c("integer", "text")),
    keep
  )
  expect_identical(
    table_fields_decision(
      c(a = "TEXT", x = "INTEGER"),
      c("a", "x"),
      c("text", "real")
    ),
    keep
  )
  # No types: a driver other than PostgreSQL.
  expect_identical(table_fields_decision(declared, c("a", "x")), keep)
  # Other names, another order, a column more or a column less. A real
  # column does not change this.
  expect_identical(
    table_fields_decision(declared, c("a", "z"), c("text", "real")),
    recreate
  )
  expect_identical(
    table_fields_decision(declared, c("x", "a"), c("real", "text")),
    recreate
  )
  expect_identical(
    table_fields_decision(
      declared,
      c("a", "x", "b"),
      c("text", "real", "text")
    ),
    recreate
  )
  expect_identical(
    table_fields_decision(declared, "a", "text"),
    recreate
  )
})

test_that("the widening statement is one lossless ALTER TABLE", {
  expect_identical(
    pg_widen_doubles_sql(
      DBI::ANSI(),
      DBI::Id(schema = "s q", table = "t\"x"),
      c("x", "y z")
    ),
    paste0(
      "ALTER TABLE \"s q\".\"t\"\"x\" ",
      "ALTER COLUMN \"x\" TYPE DOUBLE PRECISION, ",
      "ALTER COLUMN \"y z\" TYPE DOUBLE PRECISION"
    )
  )
})

test_that("the PostgreSQL widen method leaves a real column real by default", {
  tab <- pg_double_table()
  call_it <- function() {
    return(capture_pg_double_sql(
      S7::method(widen_declared_doubles, db_postgres)(
        DBI::ANSI(),
        tab,
        c(a = "TEXT", x = "DOUBLE", y = "DOUBLE"),
        role_create_table = "x",
        table_name = "tab_name"
      ),
      live = pg_double_live(c("a", "x", "y"), c("text", "real", "real"))
    ))
  }
  res <- call_it()
  # Only the query of the column types: no transaction and no ALTER.
  expect_length(res$sent, 1L)
  expect_match(res$sent[[1]], "::regclass", fixed = TRUE)
  expect_false(any(grepl("ALTER|BEGIN|lock_timeout", res$sent)))
  expect_identical(res$value, character(0))
  expect_null(res$error)
  expect_length(res$messages, 1L)
  expect_match(
    res$messages,
    "Table tab_name: the column(s) x, y are declared DOUBLE but are real",
    fixed = TRUE
  )
  expect_match(res$messages, "pg_widen_doubles = TRUE", fixed = TRUE)
  # The second call for the same table in this session gives no message.
  again <- call_it()
  expect_length(again$sent, 1L)
  expect_length(again$messages, 0L)
})

test_that("the PostgreSQL widen method with widen = TRUE sends the ALTER in a transaction with lock_timeout", {
  res <- capture_pg_double_sql(
    S7::method(widen_declared_doubles, db_postgres)(
      DBI::ANSI(),
      DBI::Id(schema = "anon", table = "tab"),
      c(a = "TEXT", x = "DOUBLE", y = "DOUBLE"),
      role_create_table = "x",
      widen = TRUE
    ),
    live = pg_double_live(
      c("a", "x", "y"),
      c("text", "real", "double precision")
    )
  )
  expect_match(res$sent[[1]], "'\"anon\".\"tab\"'::regclass", fixed = TRUE)
  expect_identical(
    res$sent[-1],
    c(
      "BEGIN",
      "SET LOCAL lock_timeout = '60s'",
      "ALTER TABLE \"anon\".\"tab\" ALTER COLUMN \"x\" TYPE DOUBLE PRECISION",
      "COMMIT"
    )
  )
  expect_identical(res$value, "x")
  expect_length(res$messages, 0L)
})

test_that("the PostgreSQL widen method takes role_create_table", {
  res <- capture_pg_double_sql(
    S7::method(widen_declared_doubles, db_postgres)(
      DBI::ANSI(),
      DBI::Id(schema = "anon", table = "tab"),
      c(x = "DOUBLE"),
      role_create_table = "owner",
      widen = TRUE
    ),
    live = pg_double_live("x", "real")
  )
  expect_identical(
    res$sent[[4]],
    paste0(
      "SET ROLE \"owner\"; ",
      "ALTER TABLE \"anon\".\"tab\" ALTER COLUMN \"x\" TYPE DOUBLE PRECISION; ",
      "RESET ROLE"
    )
  )
})

test_that("a failed widening rolls back and stops with an error that names the table", {
  res <- capture_pg_double_sql(
    S7::method(widen_declared_doubles, db_postgres)(
      DBI::ANSI(),
      DBI::Id(schema = "anon", table = "tab"),
      c(x = "DOUBLE"),
      role_create_table = "x",
      widen = TRUE,
      table_name = "tab_name"
    ),
    live = pg_double_live("x", "real"),
    fail_on = "^ALTER TABLE"
  )
  expect_identical(res$sent[[length(res$sent)]], "ROLLBACK")
  expect_false("COMMIT" %in% res$sent)
  expect_match(res$error, "column(s) x of table tab_name", fixed = TRUE)
  expect_match(res$error, "they stay real", fixed = TRUE)
  expect_match(res$error, "lock timeout", fixed = TRUE)
})

test_that("the PostgreSQL widen method sends nothing when no column is real", {
  res <- capture_pg_double_sql(
    S7::method(widen_declared_doubles, db_postgres)(
      DBI::ANSI(),
      pg_double_table(),
      c(a = "TEXT", x = "DOUBLE"),
      role_create_table = "x",
      widen = TRUE
    ),
    live = pg_double_live(c("a", "x"), c("text", "double precision"))
  )
  expect_length(res$sent, 1L)
  expect_identical(res$value, character(0))
  expect_length(res$messages, 0L)
})

test_that("the widen method of other drivers sends nothing", {
  res <- capture_pg_double_sql(
    S7::method(widen_declared_doubles, db_default)(
      DBI::ANSI(),
      "tab",
      c(x = "DOUBLE"),
      role_create_table = "x"
    )
  )
  expect_length(res$sent, 0L)
  expect_identical(res$value, character(0))
})

# DBTable_v9$create_table() on SQLite, with the generic widen_declared_doubles
# mocked to record its calls. The SQLite table stands in for one on
# PostgreSQL: the wiring is the same for every driver.
widen_calls_table <- function(
  field_types,
  pg_widen_doubles = NULL,
  envir = parent.frame()
) {
  cfg <- sqlite_dbconfig(.local_envir = envir)
  first <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", x = "DOUBLE"),
    keys = "a"
  )
  suppressMessages(first$connect())
  suppressMessages(first$insert_data(data.table::data.table(a = "k", x = 0.5)))
  first$disconnect()
  cfg$pg_widen_doubles <- pg_widen_doubles
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = field_types,
    keys = "a"
  )
  withr::defer(tab$disconnect(), envir = envir)
  return(tab)
}

# Each call records its field types and its `widen`.
widen_calls_mock <- function(env = parent.frame()) {
  calls <- new.env()
  calls$list <- list()
  local_mocked_bindings(
    widen_declared_doubles = function(
      connection,
      table,
      field_types,
      widen = FALSE,
      ...
    ) {
      calls$list[[length(calls$list) + 1L]] <- list(field_types, widen)
      return(character(0))
    },
    .env = env
  )
  return(calls)
}

test_that("create_table() keeps a table whose names match, does not widen by default, and the table takes rows", {
  tab <- widen_calls_table(c(a = "TEXT", x = "DOUBLE"))
  calls <- widen_calls_mock()
  msgs <- testthat::capture_messages(tab$connect())
  expect_identical(calls$list, list(list(c(a = "TEXT", x = "DOUBLE"), FALSE)))
  expect_false(any(grepl("Dropping table", msgs, fixed = TRUE)))
  suppressMessages(tab$insert_data(data.table::data.table(a = "m", x = 1.5)))
  expect_equal(tab$nrow(use_count = TRUE), 2)
})

test_that("create_table() widens with pg_widen_doubles = TRUE in the dbconfig", {
  tab <- widen_calls_table(c(a = "TEXT", x = "DOUBLE"), pg_widen_doubles = TRUE)
  expect_true(tab$dbconnection$config$pg_widen_doubles)
  calls <- widen_calls_mock()
  suppressMessages(tab$connect())
  expect_identical(calls$list, list(list(c(a = "TEXT", x = "DOUBLE"), TRUE)))
  expect_equal(tab$nrow(use_count = TRUE), 1)
})

test_that("DBConnection_v9 keeps pg_widen_doubles, FALSE by default, and refuses any other value", {
  expect_false(DBConnection_v9$new(driver = "SQLite")$config$pg_widen_doubles)
  expect_false(
    DBConnection_v9$new(
      driver = "SQLite",
      pg_widen_doubles = NULL
    )$config$pg_widen_doubles
  )
  expect_true(
    DBConnection_v9$new(
      driver = "SQLite",
      pg_widen_doubles = TRUE
    )$config$pg_widen_doubles
  )
  for (bad in list("yes", NA, c(TRUE, TRUE), 1)) {
    expect_error(
      DBConnection_v9$new(driver = "SQLite", pg_widen_doubles = bad),
      "pg_widen_doubles MUST be TRUE or FALSE.",
      fixed = TRUE
    )
  }
})

test_that("create_table() drops a table whose names differ, and does not widen it", {
  tab <- widen_calls_table(c(a = "TEXT", x = "DOUBLE", y = "DOUBLE"))
  # add_indexes() reaches create_table() again after the new table exists,
  # so each call records the columns of the table that it saw.
  seen <- list()
  local_mocked_bindings(
    widen_declared_doubles = function(connection, table, field_types, ...) {
      seen[[length(seen) + 1L]] <<- DBI::dbListFields(connection, table)
      return(character(0))
    }
  )
  msgs <- testthat::capture_messages(tab$connect())
  for (fields in seen) {
    expect_identical(fields, c("a", "x", "y"))
  }
  expect_true(any(grepl(
    "Dropping table tab because fields dont match",
    msgs,
    fixed = TRUE
  )))
  expect_equal(tab$nrow(use_count = TRUE), 0)
})
