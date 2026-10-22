# csdb_connection_is_alive(), and json_load_alive(), which it guards. The
# PostgreSQL test of a connection that the server ended is in
# test-pg-integration.R.

test_that("csdb_connection_is_alive() is TRUE on an open connection, and FALSE on a closed connection and on NULL", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_true(csdb_connection_is_alive(con))
  DBI::dbDisconnect(con)
  expect_false(csdb_connection_is_alive(con))
  expect_false(csdb_connection_is_alive(NULL))
})

# The messages that the catalog request gave on 2026-10-22, after
# pg_terminate_backend() and after a restart of the server.
measured_lost_messages <- c(
  "nanodbc/nanodbc.cpp:5775: 00000
The connection has been lost 
 he connection has been lost  ",
  "nanodbc/nanodbc.cpp:5775: 00000
SQLTables unable due to the connection lost 
 QLTables unable due to the connection lost  ",
  "nanodbc/nanodbc.cpp:5775: 00000 The connection is down Failed to send 'select relname'"
)

# Makes DBI::dbExistsTable() raise `condition` until the calling test ends.
local_probe_condition <- function(condition, envir = parent.frame()) {
  local_mocked_bindings(
    dbExistsTable = function(conn, name, ...) {
      if (inherits(condition, "warning")) {
        warning(condition)
      }
      stop(condition)
    },
    .package = "DBI",
    .env = envir
  )
}

test_that("csdb_connection_is_alive() is FALSE when the catalog request reports a lost connection", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(DBI::dbIsValid(con))
  for (text in measured_lost_messages) {
    local({
      local_probe_condition(simpleError(text))
      expect_false(csdb_connection_is_alive(con), label = text)
    })
  }
})

test_that("csdb_connection_is_alive() is TRUE when the catalog request fails or warns for another reason", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  local({
    local_probe_condition(simpleError("permission denied for schema anon"))
    expect_true(csdb_connection_is_alive(con))
  })
  local({
    local_probe_condition(simpleWarning("the catalog skipped a row"))
    expect_true(csdb_connection_is_alive(con))
  })
})

test_that("json_load_alive() sends no SELECT 1 on a connection that is not alive", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sent <- character()
  local_mocked_bindings(csdb_connection_is_alive = function(connection) FALSE)
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      sent <<- c(sent, statement)
      return(data.frame(x = 1L))
    },
    .package = "DBI"
  )
  expect_false(json_load_alive(con))
  expect_identical(sent, character())
})

# The upsert methods of SQL Server and of the default backend drop their
# staging table in on.exit. Each method is called directly, on a SQLite handle,
# with a first statement that fails.
upsert_drops <- function(method, alive) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dropped <- character()
  local_mocked_bindings(csdb_connection_is_alive = function(connection) alive)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      stop("csdb test: the statement failed", call. = FALSE)
    },
    dbRemoveTable = function(conn, name, ...) {
      dropped <<- c(dropped, as.character(name))
      return(TRUE)
    },
    .package = "DBI"
  )
  expect_error(
    method(
      connection = con,
      dbconfig = list(),
      table = "t",
      dt = data.table::data.table(k = 1L),
      fields = "k",
      keys = "k"
    ),
    "the statement failed"
  )
  return(dropped)
}

test_that("the SQL Server and default upserts send no DROP on a connection that is not alive", {
  for (cls in list(db_mssql, db_default)) {
    method <- S7::method(upsert_load_data_infile, cls)
    expect_length(upsert_drops(method, alive = FALSE), 0)
    expect_length(upsert_drops(method, alive = TRUE), 1)
  }
})
