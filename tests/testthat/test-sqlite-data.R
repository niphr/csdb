# The write and delete paths, driven through DBTable_v9 rather than through
# the S7 generics, so what runs is what insert_data() and friends actually
# call. The one exception is the fields precondition, which DBTable_v9 cannot
# reach and which is explained where it is tested.

test_that("insert_data round-trips TEXT, INTEGER, DOUBLE, DATE and DATETIME", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(
      a = "TEXT",
      b = "INTEGER",
      c = "DOUBLE",
      d = "DATE",
      e = "DATETIME"
    ),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  d <- data.table::data.table(
    a = c("x", "y"),
    b = c(1L, 2L),
    c = c(1.5, -2.25),
    d = as.Date(c("2020-01-01", "2021-06-30")),
    e = as.POSIXct(
      c("2020-01-01 10:00:00", "2021-06-30 23:59:59"),
      tz = "UTC"
    )
  )
  suppressMessages(tab$insert_data(d))

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")

  # The columns come back in the table's order, which is names(field_types).
  expect_identical(names(out), c("a", "b", "c", "d", "e"))

  # Class per column is the point of the block: without extended_types = TRUE
  # the DATE column reads back as a number and the DATETIME as a string.
  expect_identical(class(out$a), "character")
  expect_identical(class(out$b), "integer")
  expect_identical(class(out$c), "numeric")
  expect_s3_class(out$d, "Date")
  expect_s3_class(out$e, "POSIXct")

  expect_identical(out$a, c("x", "y"))
  expect_identical(out$b, c(1L, 2L))
  expect_identical(out$c, c(1.5, -2.25))
  expect_identical(out$d, as.Date(c("2020-01-01", "2021-06-30")))
  # Compared as seconds, because the session timezone is not UTC and the
  # printed representation would differ while the instant does not.
  expect_identical(as.numeric(out$e), as.numeric(d$e))

  # Integer, not double: dplyr::summarize(n = dplyr::n()) on a SQLite tbl
  # returns an integer where the other backends return numeric, so this is
  # compared with == and never with identical() against 2.
  expect_true(tab$nrow(use_count = TRUE) == 2)

  tab$disconnect()
})

test_that("non-finite values become NA", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  d <- data.table::data.table(
    a = c("x", "y", "z"),
    b = c(1L, 2L, 3L),
    c = c(Inf, -Inf, NaN)
  )
  suppressMessages(tab$insert_data(d))

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")

  # DBI::dbAppendTable() stores Inf and reads it back as Inf, so without the
  # scrub this column would be c(Inf, -Inf, NaN) and SQLite would disagree
  # with the two CSV backends, which write NA. The assertion is on the value
  # rather than on is.na(), so a surviving Inf reads as a wrong value.
  expect_identical(out$c, c(NA_real_, NA_real_, NA_real_))
  expect_false(any(is.infinite(out$c)))

  tab$disconnect()
})

test_that("insert_data does not modify the caller's data.table", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  # Deliberately not in the table's column order, so the reorder inside the
  # method has something to do.
  d <- data.table::data.table(
    c = c(Inf, 2.5),
    a = c("x", "y"),
    b = c(1L, 2L)
  )
  before <- data.table::copy(d)

  suppressMessages(tab$insert_data(d))

  # The write path scrubs Inf to NA and reorders the columns. Both happen on
  # a copy(), so the caller's object still holds Inf in its original position.
  expect_identical(d, before)
  expect_true(is.infinite(d$c[1]))
  expect_identical(names(d), c("c", "a", "b"))

  # ... and the scrub still reached the database.
  out <- dplyr::collect(tab$tbl())
  expect_true(is.na(out$c[out$b == 1L]))

  tab$disconnect()
})

test_that("upsert updates an existing key instead of duplicating", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE", d = "DATE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  suppressMessages(tab$insert_data(data.table::data.table(
    a = c("x", "y"),
    b = c(1L, 2L),
    c = c(1.5, 2.5),
    d = as.Date(c("2020-01-01", "2020-01-02"))
  )))

  # One row on an existing key, one row on a new key.
  suppressMessages(tab$upsert_data(data.table::data.table(
    a = c("x", "z"),
    b = c(1L, 3L),
    c = c(99.5, 3.5),
    d = as.Date(c("2024-12-24", "2020-01-03"))
  )))

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")

  # Three rows, not four: the ("x", 1) row was updated in place.
  expect_true(tab$nrow(use_count = TRUE) == 3)
  expect_identical(nrow(out), 3L)
  expect_identical(out$a, c("x", "y", "z"))
  expect_identical(out$c, c(99.5, 2.5, 3.5))
  expect_identical(
    out$d,
    as.Date(c("2024-12-24", "2020-01-02", "2020-01-03"))
  )

  # Upserting the same rows again is idempotent.
  suppressMessages(tab$upsert_data(data.table::data.table(
    a = "x",
    b = 1L,
    c = 99.5,
    d = as.Date("2024-12-24")
  )))
  expect_true(tab$nrow(use_count = TRUE) == 3)

  tab$disconnect()
})

test_that("upsert rejects empty keys", {
  # keys = character(0) is a reachable state, not a contrived one: create_table
  # then omits the PRIMARY KEY clause, and the upsert would emit
  # ON CONFLICT (), which is a syntax error saying nothing about the cause.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = character(0)
  )
  suppressMessages(tab$connect())

  expect_error(
    suppressMessages(tab$upsert_data(
      data.table::data.table(a = "x", b = 1L)
    )),
    "keys is empty"
  )

  # The precondition is checked before any SQL runs, so no staging table was
  # left behind.
  expect_identical(
    DBI::dbListTables(tab$dbconnection$connection),
    "tab"
  )

  tab$disconnect()
})

test_that("upsert rejects fields that do not match the table", {
  # DBTable_v9$upsert_data() always passes names(self$field_types), which
  # create_table() has already forced to match the live columns, so this
  # precondition cannot be reached through the R6 wrapper. The generic is
  # called directly, with the same connection and the same table identifier
  # the wrapper would have passed.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  d <- data.table::data.table(a = "x", b = 1L)

  # A field the table does not have.
  expect_error(
    upsert_load_data_infile(
      connection = tab$dbconnection$autoconnection,
      dbconfig = tab$dbconnection$config,
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
      dt = d,
      fields = c("a", "b", "nonesuch"),
      keys = c("a", "b")
    ),
    "In fields but not the table: nonesuch"
  )

  # A field of the table left out. CREATE TABLE ... AS SELECT discards
  # defaults, so a partial field list cannot be made to work correctly.
  expect_error(
    upsert_load_data_infile(
      connection = tab$dbconnection$autoconnection,
      dbconfig = tab$dbconnection$config,
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
      dt = d,
      fields = c("a", "b"),
      keys = c("a", "b")
    ),
    "In the table but not fields: c"
  )

  # A key that is not among the fields is rejected before either of those.
  expect_error(
    upsert_load_data_infile(
      connection = tab$dbconnection$autoconnection,
      dbconfig = tab$dbconnection$config,
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
      dt = d,
      fields = c("a", "b", "c"),
      keys = c("a", "nonesuch")
    ),
    "Missing from fields: nonesuch"
  )

  tab$disconnect()
})

test_that("keep_rows_where drops a row whose condition is NULL", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    # c is not a key, so it is nullable.
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())

  suppressMessages(tab$insert_data(data.table::data.table(
    a = c("x", "y", "z"),
    b = c(1L, 2L, 3L),
    c = c(5, NA_real_, -5)
  )))
  expect_true(tab$nrow(use_count = TRUE) == 3)

  # `c > 0` is TRUE for row 1, NULL for row 2 and FALSE for row 3. A
  # SELECT ... WHERE `c` > 0 keeps only row 1, so keep_rows_where must leave
  # only row 1. Emitted as NOT (`c` > 0) the predicate is NULL on row 2,
  # DELETE removes only rows whose predicate is TRUE, and row 2 survives.
  suppressMessages(tab$keep_rows_where("`c` > 0"))

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")

  expect_identical(nrow(out), 1L)
  expect_identical(out$a, "x")
  expect_false("y" %in% out$a)
  expect_identical(out$c, 5)

  # drop_rows_where is the mirror image, and DELETE's own semantics already
  # give it the right answer: only a TRUE predicate deletes.
  suppressMessages(tab$insert_data(data.table::data.table(
    a = c("y", "z"),
    b = c(2L, 3L),
    c = c(NA_real_, -5)
  )))
  suppressMessages(tab$drop_rows_where("`c` < 0"))
  out2 <- dplyr::collect(tab$tbl())
  data.table::setDT(out2)
  data.table::setorderv(out2, "b")
  expect_identical(out2$a, c("x", "y"))

  tab$disconnect()
})

test_that("drop_all_rows empties the table but keeps its indexes", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$connection

  # Created by hand rather than through self$indexes: the SQLite add_index
  # method is Phase 4's, and until then that path is a silent no-op.
  DBI::dbExecute(con, "CREATE INDEX `ind1` ON `tab` (`c`)")

  indexes_before <- DBI::dbGetQuery(con, "PRAGMA index_list(`tab`)")$name
  pk_before <- DBI::dbGetQuery(con, "PRAGMA table_info(`tab`)")$pk
  # The primary key autoindex and the hand-made one.
  expect_true("ind1" %in% indexes_before)
  expect_true(any(startsWith(indexes_before, "sqlite_autoindex")))

  suppressMessages(tab$insert_data(data.table::data.table(
    a = c("x", "y"),
    b = c(1L, 2L),
    c = c(1.5, 2.5)
  )))
  expect_true(tab$nrow(use_count = TRUE) == 2)

  # TRUNCATE TABLE is a syntax error in SQLite, so reaching db_default here
  # would error rather than empty the table.
  suppressMessages(tab$drop_all_rows())

  expect_true(tab$nrow(use_count = TRUE) == 0)
  expect_identical(nrow(dplyr::collect(tab$tbl())), 0L)

  # DELETE leaves the schema alone, which is what makes it the right choice:
  # the SQLite add_constraint method cannot put a dropped primary key back.
  expect_identical(
    DBI::dbGetQuery(con, "PRAGMA index_list(`tab`)")$name,
    indexes_before
  )
  expect_identical(
    DBI::dbGetQuery(con, "PRAGMA table_info(`tab`)")$pk,
    pk_before
  )

  # The table still rejects a duplicate key, so the key is real and not just
  # a row in the schema.
  suppressMessages(tab$insert_data(data.table::data.table(
    a = "x",
    b = 1L,
    c = 1.5
  )))
  expect_error(
    DBI::dbExecute(con, "INSERT INTO `tab` VALUES ('x', 1, 9.9)"),
    "UNIQUE"
  )

  tab$disconnect()
})

test_that("identifiers needing quoting survive every path", {
  # `order`, `select`, `group` and `where` are all SQLite keywords. Unquoted,
  # every statement below is a syntax error, so this block is what proves the
  # dbQuoteIdentifier() calls are doing something.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "order",
    field_types = c(select = "TEXT", group = "INTEGER", where = "DOUBLE"),
    keys = c("select", "group")
  )
  suppressMessages(tab$connect())

  expect_identical(DBI::dbListTables(tab$dbconnection$connection), "order")

  # insert
  suppressMessages(tab$insert_data(data.table::data.table(
    select = c("x", "y", "z"),
    group = c(1L, 2L, 3L),
    where = c(1.5, NA_real_, Inf)
  )))
  expect_true(tab$nrow(use_count = TRUE) == 3)

  # upsert, on an existing key and a new one
  suppressMessages(tab$upsert_data(data.table::data.table(
    select = c("x", "w"),
    group = c(1L, 4L),
    where = c(11.5, 4.5)
  )))
  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "group")
  expect_identical(nrow(out), 4L)
  expect_identical(out$where, c(11.5, NA_real_, NA_real_, 4.5))

  # drop_rows_where
  suppressMessages(tab$drop_rows_where("`where` > 10"))
  expect_true(tab$nrow(use_count = TRUE) == 3)

  # keep_rows_where, with the NULL row again
  suppressMessages(tab$keep_rows_where("`where` > 0"))
  out2 <- dplyr::collect(tab$tbl())
  data.table::setDT(out2)
  expect_identical(nrow(out2), 1L)
  expect_identical(out2$select, "w")

  # drop_all_rows
  suppressMessages(tab$drop_all_rows())
  expect_true(tab$nrow(use_count = TRUE) == 0)

  tab$disconnect()
})
