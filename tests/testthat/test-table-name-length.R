# PostgreSQL silently truncates an identifier longer than 63 bytes.
# DBTable_v9$new() stops on such a table name for the PostgreSQL driver. The
# check runs before any connection exists, so no server is needed.

name_of_bytes <- function(n) {
  strrep("a", n)
}

pg_dbconfig <- list(
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  db = "db",
  schema = "public",
  user = "user",
  password = "password",
  trusted_connection = NULL,
  sslmode = NULL,
  role_create_table = NULL
)

new_table <- function(dbconfig, table_name) {
  DBTable_v9$new(
    dbconfig = dbconfig,
    table_name = table_name,
    field_types = c("x" = "INTEGER"),
    keys = "x"
  )
}

test_that("PostgreSQL rejects a 64-byte table name at construction", {
  table_name <- name_of_bytes(64)
  expect_error(
    new_table(pg_dbconfig, table_name),
    paste0("'", table_name, "' is 64 bytes.*limit.*63 bytes"),
    class = "simpleError"
  )
})

test_that("PostgreSQL counts bytes, not characters", {
  # 32 characters of two bytes each make 64 bytes.
  table_name <- strrep("æ", 32)
  expect_equal(nchar(table_name), 32)
  expect_error(new_table(pg_dbconfig, table_name), "is 64 bytes")
})

test_that("PostgreSQL accepts a 63-byte table name", {
  table_name <- name_of_bytes(63)
  x <- NULL
  expect_no_error(x <- new_table(pg_dbconfig, table_name))
  expect_identical(x$table_name, table_name)
})

test_that("SQLite accepts a 64-byte table name", {
  table_name <- name_of_bytes(64)
  dbconfig <- sqlite_dbconfig()
  x <- NULL
  expect_no_error(x <- new_table(dbconfig, table_name))
  expect_identical(x$table_name, table_name)
})
