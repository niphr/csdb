# With csdb_set_context(), each PostgreSQL connection and each psql load runs
# SET application_name, so pg_stat_activity names the caller. The tests use
# SQLite connections, and they replace run_load_tool(), so no psql runs and
# the tests run on every OS. The p33 PostgreSQL harness checks a real server.

local_context <- function(x = "p33|plan7", .local_envir = parent.frame()) {
  withr::local_options(csdb.context = x, .local_envir = .local_envir)
  return(invisible(NULL))
}

# A SQLite connection with table t1 (id, x), for the psql command builders.
local_t1 <- function(.local_envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  DBI::dbExecute(con, "CREATE TABLE t1 (id INTEGER, x TEXT)")
  return(con)
}

pg_dbconfig <- function() {
  return(list(
    driver = "PostgreSQL Unicode",
    server = "pg.example",
    port = 5432,
    db = "db1",
    user = "u1",
    password = "pw-SENTINEL"
  ))
}

# Replace run_load_tool() with a client that records its arguments and the
# lines of the script that -f names, and prints "COPY 2". Put an empty psql
# file first on PATH, because run_psql_load() stops when it finds no psql.
local_recording_client <- function(.local_envir = parent.frame()) {
  seen <- new.env()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      seen$args <- args
      at <- match("-f", args)
      if (!is.na(at)) {
        seen$script <- readLines(args[[at + 1L]])
      }
      return(list(status = 0L, output = "COPY 2"))
    },
    .env = .local_envir
  )
  bin <- withr::local_tempdir(.local_envir = .local_envir)
  file.create(file.path(bin, c("psql", "psql.exe")))
  Sys.chmod(file.path(bin, "psql"), "0755")
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  return(seen)
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

test_that("both SETs hold only the kept characters of a hostile context", {
  local_context(hostile_context)
  con <- local_t1()
  seen <- local_recording_client()
  dt <- data.table::data.table(id = 1:2, x = c("a", "b"))
  want <- paste0("SET application_name = '", hostile_name, "'")
  psql_load_session(
    con,
    pg_dbconfig(),
    list("t1"),
    list(dt),
    list(withr::local_tempfile())
  )
  expect_identical(seen$script[[1]], paste0(want, ";"))
  pg <- S7::method(load_data_infile, db_postgres)
  pg(con, dbconfig = pg_dbconfig(), table = "t1", dt = dt)
  expect_identical(seen$args[[which(seen$args == "-c")[1] + 1L]], want)
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

test_that("the psql session script sets application_name in its first line", {
  local_context()
  con <- local_t1()
  seen <- local_recording_client()
  dt <- data.table::data.table(id = 1:2, x = c("a", "b"))
  psql_load_session(
    con,
    pg_dbconfig(),
    list("t1"),
    list(dt),
    list(withr::local_tempfile())
  )
  expect_identical(seen$script[[1]], "SET application_name = 'p33|plan7';")
  expect_match(seen$script[[2]], "^SET lock_timeout")
  expect_false(any(grepl("application_name", seen$args, fixed = TRUE)))

  local_context("")
  psql_load_session(
    con,
    pg_dbconfig(),
    list("t1"),
    list(dt),
    list(withr::local_tempfile())
  )
  expect_match(seen$script[[1]], "^SET lock_timeout")
  expect_false(any(grepl("application_name", seen$script, fixed = TRUE)))
})

test_that("a single-table psql load sets application_name with a -c before the copy, under ON_ERROR_STOP", {
  local_context()
  con <- local_t1()
  seen <- local_recording_client()
  pg <- S7::method(load_data_infile, db_postgres)
  dt <- data.table::data.table(id = 1:2, x = c("a", "b"))
  pg(con, dbconfig = pg_dbconfig(), table = "t1", dt = dt)
  at <- which(seen$args == "-c")
  expect_length(at, 2)
  expect_identical(
    seen$args[[at[1] + 1L]],
    "SET application_name = 'p33|plan7'"
  )
  expect_match(seen$args[[at[2] + 1L]], "^\\\\copy")
  expect_false(any(grepl("application_name=", seen$args, fixed = TRUE)))
  # A failed SET stops psql before the copy.
  expect_identical(seen$args[[match("ON_ERROR_STOP=1", seen$args) - 1L]], "-v")

  local_context("")
  pg(con, dbconfig = pg_dbconfig(), table = "t1", dt = dt)
  expect_length(which(seen$args == "-c"), 1)
  expect_false("ON_ERROR_STOP=1" %in% seen$args)
  expect_false(any(grepl("application_name", seen$args, fixed = TRUE)))
})
