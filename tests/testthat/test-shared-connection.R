# DBTable_v9 builds its own DBConnection_v9, unless the caller supplies one
# through the `dbconnection` argument. These blocks cover both routes, and the
# ownership rule that follows: the object closes only a connection it built.
#
# Every block passes an explicit SQLite dbconfig from helper-sqlite.R. csdb
# reads no environment variable for the connection settings, so no test here
# can reach a real server.
#
# Assert on `conn$connection`, never on `conn$autoconnection`. The
# `autoconnection` binding calls connect() before it returns the handle, so a
# closed connection reopens and the assertion passes for the wrong reason.

test_that("a supplied dbconnection is the connection the table uses", {
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)

  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a",
    dbconnection = conn
  )

  # An R6 object is an environment, so identical() compares object identity.
  expect_identical(tab$dbconnection, conn)

  # The table also works through the borrowed connection.
  suppressMessages(tab$connect())
  expect_true("tab" %in% DBI::dbListTables(conn$connection))

  conn$disconnect()
})

test_that("without the argument the table builds its own connection", {
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)

  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a"
  )

  expect_s3_class(tab$dbconnection, "DBConnection_v9")
  expect_false(identical(tab$dbconnection, conn))
  # It carries the settings from dbconfig, exactly as before.
  expect_identical(tab$dbconnection$config$db, cfg$db)

  tab$disconnect()
})

test_that("disconnect() on a borrower leaves the shared connection open", {
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)

  tab1 <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab1",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a",
    dbconnection = conn
  )
  tab2 <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab2",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a",
    dbconnection = conn
  )

  # One connection object serves both tables.
  expect_identical(tab1$dbconnection, tab2$dbconnection)

  suppressMessages(tab1$connect())
  expect_true(DBI::dbIsValid(conn$connection))
  handle_before <- conn$connection

  tab2$disconnect()

  expect_true(DBI::dbIsValid(conn$connection))
  # The handle is the one from before, so nothing closed and reopened it.
  expect_identical(conn$connection, handle_before)
  expect_true(tab1$dbconnection$is_connected())

  conn$disconnect()
})

test_that("disconnect() on an owner closes the connection it built", {
  cfg <- sqlite_dbconfig()

  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a"
  )

  suppressMessages(tab$connect())
  expect_true(DBI::dbIsValid(tab$dbconnection$connection))

  tab$disconnect()
  expect_false(DBI::dbIsValid(tab$dbconnection$connection))
})

test_that("a repeated disconnect() on the owner does not error", {
  cfg <- sqlite_dbconfig()

  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a"
  )

  suppressMessages(tab$connect())
  tab$disconnect()

  expect_no_error(tab$disconnect())
  expect_false(DBI::dbIsValid(tab$dbconnection$connection))
})
