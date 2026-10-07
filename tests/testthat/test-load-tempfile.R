# Until 2026.10.7 the default MySQL upsert method wrote its load file to the
# fixed path "/tmp/x123.csv", and the default load method to "/xtmp/x123.csv".
# Two processes that upserted at the same time wrote the same file, and CRAN
# forbids a write outside tempdir(). Both defaults are now
# tempfile(fileext = ".csv"), which R evaluates once per call.
#
# The two methods need a MySQL server. The tests replace the DBI calls that
# reach the server, call load_data_infile() through the db_default method, and
# record every file that write_data_infile() writes. dbQuoteString() and
# dbQuoteIdentifier() stay real, on an in-memory SQLite connection.

mysql_upsert <- S7::method(upsert_load_data_infile, db_default)
mysql_load <- S7::method(load_data_infile, db_default)

# Return an environment whose `files` holds every path that
# write_data_infile() wrote, in call order. load_data_infile() reaches the
# method in `load`, which is the MySQL method unless a test names another.
local_mysql_load_path <- function(env = parent.frame(), load = mysql_load) {
  seen <- new.env()
  seen$files <- character(0)
  real_write <- write_data_infile
  testthat::local_mocked_bindings(
    write_data_infile = function(dt, file, ...) {
      seen$files <- c(seen$files, file)
      return(real_write(dt = dt, file = file, ...))
    },
    load_data_infile = function(connection, ...) {
      return(load(connection, ...))
    },
    .env = env
  )
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      return(0L)
    },
    dbListFields = function(conn, name, ...) {
      return(c("a", "b"))
    },
    dbRemoveTable = function(conn, name, ...) {
      return(TRUE)
    },
    .package = "DBI",
    .env = env
  )
  return(seen)
}

local_sqlite_con <- function(env = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  return(con)
}

new_rows <- function() {
  return(data.table::data.table(a = 1:2, b = c("x", "y")))
}

call_upsert <- function(con) {
  return(mysql_upsert(
    connection = con,
    table = "tab",
    dt = new_rows(),
    fields = c("a", "b"),
    keys = "a"
  ))
}

call_load <- function(con) {
  return(mysql_load(connection = con, table = "tab", dt = new_rows()))
}

test_that("two default MySQL upserts write two different files", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  call_upsert(con)
  call_upsert(con)
  expect_length(seen$files, 2L)
  expect_false(seen$files[[1]] == seen$files[[2]])
})

test_that("two default MySQL loads write two different files", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  call_load(con)
  call_load(con)
  expect_length(seen$files, 2L)
  expect_false(seen$files[[1]] == seen$files[[2]])
})

test_that("the default MySQL load file does not exist after the call", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  call_upsert(con)
  call_load(con)
  expect_length(seen$files, 2L)
  expect_identical(file.exists(seen$files), c(FALSE, FALSE))
})

test_that("the default MySQL load file is directly in tempdir()", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  call_upsert(con)
  call_load(con)
  expect_length(seen$files, 2L)
  expect_identical(
    normalizePath(dirname(seen$files), winslash = "/"),
    rep(normalizePath(tempdir(), winslash = "/"), 2L)
  )
  expect_true(all(grepl("\\.csv$", seen$files)))
})

# A write that fails partway. The mocked fwrite() writes one line and then
# stops, so load_data_infile() never reaches the unlink that it registers
# after the write. Only the unlink registered before the write deletes the
# file.
local_partial_fwrite <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    fwrite = function(x, file, ...) {
      writeLines("a,b", file)
      stop("disk full", call. = FALSE)
    },
    .env = env
  )
  return(invisible(NULL))
}

test_that("a default MySQL load deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  local_partial_fwrite()
  expect_error(call_load(con), "disk full")
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})

test_that("a default MySQL upsert deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  local_partial_fwrite()
  expect_error(call_upsert(con), "disk full")
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})

test_that("a MySQL load leaves a partly written file that the caller named", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path()
  local_partial_fwrite()
  file <- withr::local_tempfile(fileext = ".csv")
  expect_error(
    mysql_load(connection = con, table = "tab", dt = new_rows(), file = file),
    "disk full"
  )
  expect_identical(seen$files, file)
  expect_true(file.exists(file))
})

# PostgreSQL and SQL Server. The mocked fwrite() stops before psql or bcp can
# run, so no client and no server is needed.
pg_load <- S7::method(load_data_infile, db_postgres)
pg_upsert <- S7::method(upsert_load_data_infile, db_postgres)
ms_load <- S7::method(load_data_infile, db_mssql)
ms_upsert <- S7::method(upsert_load_data_infile, db_mssql)

test_that("a default PostgreSQL load deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path(load = pg_load)
  local_partial_fwrite()
  expect_error(
    pg_load(connection = con, table = "tab", dt = new_rows()),
    "disk full"
  )
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})

test_that("a default PostgreSQL upsert deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path(load = pg_load)
  local_partial_fwrite()
  expect_error(
    pg_upsert(
      connection = con,
      table = DBI::Id(schema = "public", table = "tab"),
      dt = new_rows(),
      fields = c("a", "b"),
      keys = "a"
    ),
    "disk full"
  )
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})

test_that("a default SQL Server load deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path(load = ms_load)
  local_partial_fwrite()
  expect_error(
    ms_load(connection = con, table = "tab", dt = new_rows()),
    "disk full"
  )
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})

test_that("a default SQL Server upsert deletes a partly written file", {
  con <- local_sqlite_con()
  seen <- local_mysql_load_path(load = ms_load)
  local_partial_fwrite()
  expect_error(
    ms_upsert(
      connection = con,
      table = "tab",
      dt = new_rows(),
      fields = c("a", "b"),
      keys = "a"
    ),
    "disk full"
  )
  expect_length(seen$files, 1L)
  expect_false(file.exists(seen$files[[1]]))
})
