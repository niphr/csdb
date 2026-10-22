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

test_that("csdb_connection_is_alive() is FALSE when the catalog request fails", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  local_mocked_bindings(
    dbExistsTable = function(conn, name, ...) {
      stop("SQLTables unable due to the connection lost", call. = FALSE)
    },
    .package = "DBI"
  )
  expect_true(DBI::dbIsValid(con))
  expect_false(csdb_connection_is_alive(con))
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
