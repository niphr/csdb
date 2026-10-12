# DBConnection_v9$connect() retries every failed connection. From 2026.10.12
# it makes 3 attempts by default and waits 2 s before attempt 2 and 10 s
# before attempt 3, as the psql and bcp loads do. The Entra token proxy
# norsyss-pg-auth refused about 5 logins a minute under 4 parallel workers on
# 2026-10-12, and 2 attempts 1 s apart were too few.
#
# No test needs a database server. A mocked DBI::dbConnect() fails a set
# number of times and then returns an in-memory SQLite connection. A mocked
# retry_wait() records each wait instead of sleeping.

# Replace DBI::dbConnect(). The first `failures` calls stop, and every later
# call returns an in-memory SQLite connection. The returned environment
# counts the calls in `calls`.
local_flaky_connect <- function(failures, .local_envir = parent.frame()) {
  rec <- new.env()
  rec$calls <- 0L
  real_connect <- DBI::dbConnect
  local_mocked_bindings(
    dbConnect = function(drv, ...) {
      rec$calls <- rec$calls + 1L
      if (rec$calls <= failures) {
        stop("internal proxy error", call. = FALSE)
      }
      return(real_connect(RSQLite::SQLite(), ":memory:"))
    },
    .package = "DBI",
    .env = .local_envir
  )
  return(rec)
}

# Record every wait between attempts instead of sleeping. A csdb without
# retry_wait() has no such binding, and there the mock is left out, so a test
# fails on its assertions and not on a missing function.
local_connect_waits <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$waits <- numeric()
  if (exists("retry_wait", envir = asNamespace("csdb"), inherits = FALSE)) {
    local_mocked_bindings(
      retry_wait = function(seconds) {
        rec$waits <- c(rec$waits, seconds)
        return(invisible(NULL))
      },
      .env = .local_envir
    )
  }
  return(rec)
}

retry_test_connection <- function(.local_envir = parent.frame()) {
  withr::local_options(
    csdb.password_hook = NULL,
    csdb.auth_hook = NULL,
    .local_envir = .local_envir
  )
  db <- DBConnection_v9$new(
    driver = "PostgreSQL Unicode",
    server = "norsyss-pg-auth",
    port = 5432L,
    db = "d",
    user = "u",
    password = "cfg-pw"
  )
  withr::defer(db$disconnect(), envir = .local_envir)
  return(db)
}

# Run `expr`. Return its error, or NULL, and every message it emitted.
collect_connect_conditions <- function(expr) {
  msgs <- character()
  err <- withCallingHandlers(
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) {
        return(e)
      }
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  return(list(error = err, messages = msgs))
}

test_that("connect() succeeds on attempt 3 after 2 failures, waiting 2 s and 10 s", {
  rec <- local_flaky_connect(failures = 2L)
  waits <- local_connect_waits()
  db <- retry_test_connection()
  got <- collect_connect_conditions(db$connect())
  expect_null(got$error)
  expect_true(db$is_connected())
  expect_identical(rec$calls, 3L)
  expect_identical(waits$waits, c(2, 10))
  expect_match(
    got$messages,
    "Attempt 1 of 3 to connect failed. Retrying in 2 s: Could not connect",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    got$messages,
    "Attempt 2 of 3 to connect failed. Retrying in 10 s: Could not connect",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(got$messages, "internal proxy error", fixed = TRUE, all = FALSE)
})

test_that("connect() stops after 3 failed attempts by default", {
  rec <- local_flaky_connect(failures = Inf)
  waits <- local_connect_waits()
  db <- retry_test_connection()
  got <- collect_connect_conditions(db$connect())
  expect_match(
    conditionMessage(got$error),
    "Failed to connect to database after 3 attempts",
    fixed = TRUE
  )
  expect_identical(rec$calls, 3L)
  expect_identical(waits$waits, c(2, 10))
  expect_match(
    got$messages,
    "Attempt 3 of 3 to connect failed: Could not connect",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("connect(attempts = 4) waits 10 s before each attempt after the third", {
  rec <- local_flaky_connect(failures = Inf)
  waits <- local_connect_waits()
  db <- retry_test_connection()
  got <- collect_connect_conditions(db$connect(attempts = 4))
  expect_match(
    conditionMessage(got$error),
    "Failed to connect to database after 4 attempts",
    fixed = TRUE
  )
  expect_identical(rec$calls, 4L)
  expect_identical(waits$waits, c(2, 10, 10))
})
