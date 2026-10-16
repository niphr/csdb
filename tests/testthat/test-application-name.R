# With csdb_set_context(), each PostgreSQL connection runs SET
# application_name, so pg_stat_activity names the caller. The tests use SQLite
# connections, so they run on every OS. The p33 PostgreSQL harness checks a
# real server.

local_context <- function(x = "p33|plan7", .local_envir = parent.frame()) {
  withr::local_options(csdb.context = x, .local_envir = .local_envir)
  return(invisible(NULL))
}

test_that("csdb_set_context() sets the context and returns the previous one", {
  withr::local_options(csdb.context = NULL)
  expect_identical(csdb_get_context(), "")
  expect_identical(csdb_set_context("t|p"), "")
  expect_identical(csdb_get_context(), "t|p")
  expect_identical(csdb_set_context(""), "t|p")
  expect_error(csdb_set_context(c("a", "b")), "one string")
  expect_error(csdb_set_context(NA_character_), "one string")
})

test_that("a PostgreSQL connection gets SET application_name, and no other driver does", {
  local_context()
  expect_identical(
    connection_application_name_sql("PostgreSQL Unicode"),
    "SET application_name = 'p33|plan7'"
  )
  expect_null(connection_application_name_sql("SQLite"))
  expect_null(connection_application_name_sql("ODBC Driver 17 for SQL Server"))
  local_context("")
  expect_null(connection_application_name_sql("PostgreSQL Unicode"))
})

# A context with a backslash, an apostrophe, a semicolon, SQL, a space and a
# non-ASCII character. Under standard_conforming_strings = off, a backslash
# before the apostrophe would end a quoted literal early.
hostile_context <- paste0(
  "t\\'; DROP TABLE x; --|p.1:a_b-c",
  intToUtf8(229)
)
hostile_name <- "tDROPTABLEx--|p.1:a_b-c"

test_that("application_name keeps only [A-Za-z0-9_|.:-] and the first 63 characters", {
  expect_identical(pg_application_name(hostile_context), hostile_name)
  expect_identical(
    pg_application_name_sql(hostile_context),
    paste0("SET application_name = '", hostile_name, "'")
  )
  expect_null(pg_application_name_sql(""))
  expect_null(pg_application_name_sql("'; \\"))
  expect_identical(pg_application_name(strrep("a", 70)), strrep("a", 63))
  expect_identical(
    pg_application_name(paste0(strrep(intToUtf8(229), 10), "abc")),
    "abc"
  )
})

test_that("the SET of a connection holds only the kept characters of a hostile context", {
  local_context(hostile_context)
  want <- paste0("SET application_name = '", hostile_name, "'")
  expect_identical(connection_application_name_sql("PostgreSQL Unicode"), want)
})

test_that("connect() runs the application_name statement on the new connection", {
  local_context()
  # A statement that SQLite runs and that leaves a trace stands in for SET.
  local_mocked_bindings(
    connection_application_name_sql = function(driver) {
      return("CREATE TABLE app_name_sent (x INTEGER)")
    }
  )
  conn <- sqlite_connection(sqlite_dbconfig())
  conn$connect()
  withr::defer(conn$disconnect())
  expect_true("app_name_sent" %in% DBI::dbListTables(conn$connection))
})

test_that("a failed SET on a connection that answers SELECT 1 keeps the connection and reports nothing", {
  local_context()
  # SQLite has no SET statement, so it fails as a refused SET does.
  local_mocked_bindings(
    connection_application_name_sql = function(driver) {
      return("SET application_name = 'p33|plan7'")
    }
  )
  conn <- sqlite_connection(sqlite_dbconfig())
  expect_no_condition(conn$connect())
  withr::defer(conn$disconnect())
  expect_true(conn$is_connected())
})

test_that("a failed SET on a connection that does not answer SELECT 1 is a failed connect attempt", {
  local_context()
  waits <- numeric()
  local_mocked_bindings(
    connection_application_name_sql = function(driver) {
      return("SET application_name = 'p33|plan7'")
    },
    # The connection is lost after the SET, so SELECT 1 fails.
    probe_connection = function(connection) {
      DBI::dbDisconnect(connection)
      stop(
        "SET application_name failed, and the connection did not answer ",
        "SELECT 1: lost (mocked)",
        call. = FALSE
      )
    },
    retry_wait = function(seconds) {
      waits <<- c(waits, seconds)
      return(invisible(NULL))
    }
  )
  conn <- sqlite_connection(sqlite_dbconfig())
  msgs <- character()
  err <- withCallingHandlers(
    tryCatch(conn$connect(attempts = 2), error = function(e) e),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "after 2 attempts", fixed = TRUE)
  expect_length(grep("did not answer SELECT 1", msgs, fixed = TRUE), 2)
  expect_identical(waits, 2)
})

test_that("probe_connection() passes a live connection and stops on a closed one", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_no_condition(probe_connection(con))
  DBI::dbDisconnect(con)
  expect_error(probe_connection(con), "did not answer SELECT 1", fixed = TRUE)
})
