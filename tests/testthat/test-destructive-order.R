# `drop_all_rows_and_then_upsert_data()` and
# `drop_all_rows_and_then_insert_data()` empty the table before they write.
# Both reject four kinds of `newdata` before the first row is dropped. They
# are a `NULL`, an object that is not a `data.frame`, a row count that is
# unusable or unstable, and data that the validator refuses.
#
# That list is the whole claim. `upsert_data()` and `insert_data()` read
# `newdata` again after the drop. A `dim()` method that answers differently on
# a later call therefore still empties the table and then raises. No test
# below claims otherwise.
#
# Two separate defects lived here, and the second is the reason the guard sits
# in the destructive methods rather than earlier inside `upsert_data()`.
#
#  1. Invalid data. The methods called `drop_all_rows()` and then
#     `upsert_data()`, and the validator runs inside `upsert_data()`. Measured
#     on 2026-08-15: a table holding 3 sentinel rows raised
#     "upsert_load_data_infile not validated in tt. g" and held 0 rows after.
#  2. A NULL, and a zero-row frame that fails validation. `upsert_data()` and
#     `insert_data()` return early on both, BEFORE they reach the validator. A
#     NULL is therefore not invalid data: the validator never sees it.
#     Measured on 2026-08-15: 2 sentinel rows before, 0 rows after, and NO
#     ERROR AT ALL.
#
# Clearing a table with a zero-row frame is legitimate, and
# `cs9::DBPartitionedTableExtended_v9` uses it to clear every partition. Every
# zero-row test below asserts that the clear still happens.

# Rejects a frame without a `c` column, and any row whose `a` is "bad". The
# `var` attribute is the contract `DBTable_v9` reads from a validator.
#
# `any(character(0) == "bad")` is FALSE, so a zero-row frame that carries `c`
# passes. That is what separates the two zero-row cases below.
validator_reject_bad <- function(data) {
  if (!"c" %in% names(data)) {
    retval <- FALSE
    attr(retval, "var") <- "c"
    return(retval)
  }
  if (any(data$a == "bad")) {
    retval <- FALSE
    attr(retval, "var") <- "a"
    return(retval)
  }
  TRUE
}

# Three rows, so a partial wipe is visible as a row count and not only as an
# empty table.
sentinel_rows <- function() {
  data.table::data.table(
    a = c("s1", "s2", "s3"),
    b = c(1L, 2L, 3L),
    c = c(1.5, 2.5, 3.5)
  )
}

# A connected table holding the three sentinel rows.
#
# The default validator is the permissive one, so a test that passes it
# measures the guard and nothing else. `validator_reject_bad` above rejects a
# NULL, because `names(NULL)` holds no `c`. It ACCEPTS the list tested below,
# because that list carries `a`, `b` and `c`, and its `a` is not "bad". A table
# built on it would report a rejection that the guard did not make.
#
# `envir` is mandatory, not decoration. Both `sqlite_dbconfig()` and
# `withr::defer()` tie their cleanup to it, and the default is this function's
# own frame. The database file would then go away as soon as this function
# returned.
sentinel_table <- function(
  validator = validator_field_contents_blank,
  envir = parent.frame()
) {
  cfg <- sqlite_dbconfig(.local_envir = envir)
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    validator_field_contents = validator
  )
  suppressMessages(tab$connect())
  withr::defer(tab$disconnect(), envir = envir)
  suppressMessages(tab$insert_data(sentinel_rows()))
  tab
}

# Every sentinel row is still there, with its own values.
expect_sentinels_intact <- function(tab) {
  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")
  expect_identical(nrow(out), 3L)
  expect_identical(out$a, c("s1", "s2", "s3"))
  expect_identical(out$b, c(1L, 2L, 3L))
  expect_identical(out$c, c(1.5, 2.5, 3.5))
  expect_true(tab$nrow(use_count = TRUE) == 3)
}

# The table is empty.
expect_table_empty <- function(tab) {
  expect_identical(nrow(dplyr::collect(tab$tbl())), 0L)
  expect_true(tab$nrow(use_count = TRUE) == 0)
}

# NULL ----

test_that("upsert raises on a NULL and every sentinel row survives", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(NULL))
  )
  # The message names the problem, so a reader learns what happened from the
  # error alone.
  expect_match(conditionMessage(err), "newdata is NULL", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "drop_all_rows_and_then_upsert_data",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "tab", fixed = TRUE)

  # This is the point of the test. The error alone proves nothing: before this
  # release the call raised nothing and emptied the table.
  expect_sentinels_intact(tab)
})

test_that("insert raises on a NULL and every sentinel row survives", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(NULL))
  )
  expect_match(conditionMessage(err), "newdata is NULL", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "drop_all_rows_and_then_insert_data",
    fixed = TRUE
  )

  expect_sentinels_intact(tab)
})

test_that("upsert raises on a NULL before the table is created", {
  # The rejection comes before lazy creation, so a NULL cannot create a table
  # either. `autoconnection` opens the connection and creates nothing.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    validator_field_contents = validator_field_contents_blank
  )
  withr::defer(tab$disconnect())

  expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(NULL)),
    "newdata is NULL"
  )
  expect_identical(
    DBI::dbListTables(tab$dbconnection$autoconnection),
    character(0)
  )
})

# Not a data.frame ----

test_that("upsert raises on a list and every sentinel row survives", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(
      list(a = "s4", b = 4L, c = 4.5)
    ))
  )
  expect_match(
    conditionMessage(err),
    "newdata is not a data.frame",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "list", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("insert raises on a list and every sentinel row survives", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(
      list(a = "s4", b = 4L, c = 4.5)
    ))
  )
  expect_match(
    conditionMessage(err),
    "newdata is not a data.frame",
    fixed = TRUE
  )

  expect_sentinels_intact(tab)
})

# A data.frame subclass with no usable row count ----

# `is.data.frame()` is TRUE on each class below, and a permissive validator
# accepts each one. The guard MUST therefore reject the row count itself,
# before the drop.
#
# Each registration goes into the S3 method table of base, which the internal
# dispatch inside `nrow()` reads. Measured on the pod on 2026-08-15: the
# method fires on a call made from inside the csdb namespace, which is where
# the guard runs. Each class name is unique to this file, so nothing else in
# the suite can dispatch on one.

# `nrow()` is NA.
dim.csdb_broken_dim <- function(x) c(NA_integer_, NA_integer_)
registerS3method("dim", "csdb_broken_dim", dim.csdb_broken_dim)

# `nrow()` is Inf. Inf is numeric, it has length 1, it is not NA, it is not
# negative, and it equals `trunc(Inf)`. Only a finiteness test rejects it.
dim.csdb_infinite_dim <- function(x) c(Inf, 3L)
registerS3method("dim", "csdb_infinite_dim", dim.csdb_infinite_dim)

# `nrow()` is 3 on the first call and 5 on every later call. The counter lives
# in its own environment, and `unstable_dim_frame()` resets it, so each test
# starts from call one.
unstable_dim_state <- new.env(parent = emptyenv())
unstable_dim_state$calls <- 0L
dim.csdb_unstable_dim <- function(x) {
  unstable_dim_state$calls <- unstable_dim_state$calls + 1L
  c(if (unstable_dim_state$calls == 1L) 3L else 5L, 3L)
}
registerS3method("dim", "csdb_unstable_dim", dim.csdb_unstable_dim)

frame_with_class <- function(cls) {
  d <- data.frame(a = "s4", b = 4L, c = 4.5, stringsAsFactors = FALSE)
  class(d) <- c(cls, "data.frame")
  d
}

broken_dim_frame <- function() frame_with_class("csdb_broken_dim")

infinite_dim_frame <- function() frame_with_class("csdb_infinite_dim")

unstable_dim_frame <- function() {
  unstable_dim_state$calls <- 0L
  frame_with_class("csdb_unstable_dim")
}

test_that("upsert raises on a subclass with no row count and keeps every row", {
  tab <- sentinel_table()

  d <- broken_dim_frame()
  expect_true(is.data.frame(d))
  expect_true(is.na(nrow(d)))

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(d))
  )
  expect_match(
    conditionMessage(err),
    "newdata has no usable row count",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Nothing was dropped.", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("insert raises on a subclass with no row count and keeps every row", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(broken_dim_frame()))
  )
  expect_match(
    conditionMessage(err),
    "newdata has no usable row count",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Nothing was dropped.", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("upsert raises on an infinite row count and keeps every row", {
  tab <- sentinel_table()

  d <- infinite_dim_frame()
  expect_true(is.data.frame(d))
  expect_identical(nrow(d), Inf)

  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(d))
  )
  expect_match(
    conditionMessage(err),
    "newdata has no usable row count",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "as Inf.", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("insert raises on an infinite row count and keeps every row", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(
      tab$drop_all_rows_and_then_insert_data(infinite_dim_frame())
    )
  )
  expect_match(
    conditionMessage(err),
    "newdata has no usable row count",
    fixed = TRUE
  )

  expect_sentinels_intact(tab)
})

test_that("upsert raises when two reads of the row count disagree", {
  tab <- sentinel_table()

  # Two reads on one fresh object, so the disagreement is a property of the
  # fixture and not of the guard.
  probe <- unstable_dim_frame()
  expect_identical(nrow(probe), 3L)
  expect_identical(nrow(probe), 5L)

  err <- expect_error(
    suppressMessages(
      tab$drop_all_rows_and_then_upsert_data(unstable_dim_frame())
    )
  )
  expect_match(
    conditionMessage(err),
    "newdata has an unstable row count",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "as 3, and then as 5.", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("insert raises when two reads of the row count disagree", {
  tab <- sentinel_table()

  err <- expect_error(
    suppressMessages(
      tab$drop_all_rows_and_then_insert_data(unstable_dim_frame())
    )
  )
  expect_match(
    conditionMessage(err),
    "newdata has an unstable row count",
    fixed = TRUE
  )

  expect_sentinels_intact(tab)
})

# Zero rows, structurally valid ----

test_that("upsert clears the table on a zero-row frame that validates", {
  # `cs9::DBPartitionedTableExtended_v9` clears every partition this way, so
  # this path MUST keep working.
  tab <- sentinel_table()

  empty <- data.table::data.table(
    a = character(0),
    b = integer(0),
    c = numeric(0)
  )
  expect_no_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(empty))
  )

  expect_table_empty(tab)
})

test_that("insert clears the table on a zero-row frame that validates", {
  tab <- sentinel_table()

  empty <- data.table::data.table(
    a = character(0),
    b = integer(0),
    c = numeric(0)
  )
  expect_no_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(empty))
  )

  expect_table_empty(tab)
})

# Zero rows, fails validation ----

test_that("upsert raises on a zero-row frame that fails validation", {
  tab <- sentinel_table(validator = validator_reject_bad)

  # No `c` column, so the validator rejects it. Before this release
  # `upsert_data()` returned on the row count before it reached the validator,
  # and the table was already empty.
  empty <- data.table::data.table(a = character(0), b = integer(0))
  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(empty))
  )
  expect_match(
    conditionMessage(err),
    "newdata failed validator_field_contents",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Field: c", fixed = TRUE)

  expect_sentinels_intact(tab)
})

test_that("insert raises on a zero-row frame that fails validation", {
  tab <- sentinel_table(validator = validator_reject_bad)

  empty <- data.table::data.table(a = character(0), b = integer(0))
  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(empty))
  )
  expect_match(
    conditionMessage(err),
    "newdata failed validator_field_contents",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Field: c", fixed = TRUE)

  expect_sentinels_intact(tab)
})

# Nonzero rows, fails validation ----

test_that("upsert raises on invalid rows before it drops any row", {
  tab <- sentinel_table(validator = validator_reject_bad)

  bad <- data.table::data.table(a = "bad", b = 4L, c = 4.5)
  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(bad))
  )
  expect_match(
    conditionMessage(err),
    "newdata failed validator_field_contents",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Field: a", fixed = TRUE)

  # Measured before this release: the same call raised
  # "upsert_load_data_infile not validated" and left 0 rows.
  expect_sentinels_intact(tab)
})

test_that("insert raises on invalid rows before it drops any row", {
  tab <- sentinel_table(validator = validator_reject_bad)

  bad <- data.table::data.table(a = "bad", b = 4L, c = 4.5)
  err <- expect_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(bad))
  )
  expect_match(
    conditionMessage(err),
    "newdata failed validator_field_contents",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Field: a", fixed = TRUE)

  expect_sentinels_intact(tab)
})

# Nonzero rows, valid ----

test_that("upsert replaces the table contents on valid rows", {
  tab <- sentinel_table()

  new <- data.table::data.table(
    a = c("n1", "n2"),
    b = c(7L, 8L),
    c = c(7.5, 8.5)
  )
  expect_no_error(
    suppressMessages(tab$drop_all_rows_and_then_upsert_data(new))
  )

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")
  expect_identical(nrow(out), 2L)
  expect_identical(out$a, c("n1", "n2"))
  expect_identical(out$c, c(7.5, 8.5))
})

test_that("insert replaces the table contents on valid rows", {
  tab <- sentinel_table()

  new <- data.table::data.table(
    a = c("n1", "n2"),
    b = c(7L, 8L),
    c = c(7.5, 8.5)
  )
  expect_no_error(
    suppressMessages(tab$drop_all_rows_and_then_insert_data(new))
  )

  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "b")
  expect_identical(nrow(out), 2L)
  expect_identical(out$a, c("n1", "n2"))
  expect_identical(out$c, c(7.5, 8.5))
})
