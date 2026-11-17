# setorder_by_pk() sorts a data.table by reference: first on the primary key
# columns that d has, in key order, and then on every other column of d.
#
# The rows of `shuffled()` give three different orders, so a test can tell
# them apart: the input order, the order by key (b, a, then v), and the order
# by column (a, b, v).
shuffled <- function() {
  return(data.table::data.table(
    a = c(2L, 1L, 2L, 1L, 1L),
    b = c("x", "y", "y", "x", "x"),
    v = c(5, 4, 3, 2, 1)
  ))
}

test_that("setorder_by_pk orders by the keys, in key order, then the rest", {
  d <- shuffled()
  setorder_by_pk(d, list(keys = c("b", "a")))

  # Rows a = 1, b = "x" tie on the key, so v decides between them.
  expect_identical(d$b, c("x", "x", "x", "y", "y"))
  expect_identical(d$a, c(1L, 1L, 2L, 1L, 2L))
  expect_identical(d$v, c(1, 2, 5, 4, 3))
  # The columns keep their places. Only the rows move.
  expect_identical(names(d), c("a", "b", "v"))
})

test_that("setorder_by_pk with table = NULL orders by every column", {
  d <- data.table::data.table(
    c = c(2, 1, 1),
    a = c("z", "y", "x")
  )
  setorder_by_pk(d)
  expect_identical(d$c, c(1, 1, 2))
  expect_identical(d$a, c("x", "y", "z"))

  # A table whose keys is NULL also orders by every column.
  d <- shuffled()
  setorder_by_pk(d, list(keys = NULL))
  expect_identical(d$v, c(1, 2, 4, 5, 3))
})

test_that("setorder_by_pk ignores a key column that d does not have", {
  d <- shuffled()
  setorder_by_pk(d, list(keys = c("zz", "v", "yy")))
  expect_identical(d$v, c(1, 2, 3, 4, 5))
  expect_identical(d$a, c(1L, 1L, 2L, 1L, 2L))
})

test_that("setorder_by_pk refuses a data.frame and a tibble", {
  df <- as.data.frame(shuffled())
  expect_error(
    setorder_by_pk(df, list(keys = "b")),
    "`d` must be a data.table, but it has class data.frame"
  )
  expect_error(
    setorder_by_pk(dplyr::as_tibble(df)),
    "`d` must be a data.table"
  )
})

test_that("setorder_by_pk refuses keys that are not character", {
  expect_error(
    setorder_by_pk(shuffled(), list(keys = 1:2)),
    "`table\\$keys` must be a character vector or NULL"
  )
})

test_that("setorder_by_pk returns d itself, so it sorts by reference and pipes", {
  d <- shuffled()
  before <- data.table::address(d)
  res <- setorder_by_pk(d, list(keys = c("b", "a")))
  expect_identical(data.table::address(res), before)
  expect_identical(data.table::address(d), before)

  piped <- shuffled() |>
    setorder_by_pk(list(keys = c("b", "a")))
  expect_identical(piped$v, c(1, 2, 5, 4, 3))
})

test_that("setorder_by_pk reads the keys of a SQLite DBTable_v9", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "shuffled",
    field_types = c(a = "INTEGER", b = "TEXT", v = "DOUBLE"),
    keys = c("b", "a", "v")
  )
  suppressMessages(tab$connect())
  on.exit(tab$disconnect(), add = TRUE)
  suppressMessages(tab$insert_data(shuffled()))

  d <- dplyr::collect(tab$tbl())
  data.table::setDT(d)
  setorder_by_pk(d, tab)

  expect_identical(tab$keys, c("b", "a", "v"))
  expect_identical(d$b, c("x", "x", "x", "y", "y"))
  expect_identical(d$a, c(1L, 1L, 2L, 1L, 2L))
  expect_identical(d$v, c(1, 2, 5, 4, 3))
})

test_that("setorder_by_pk skips a list column and still sorts on the rest", {
  d <- data.table::data.table(id = c(2L, 1L, 3L))
  d[, extra := list(list("b", "a", "c"))]
  setorder_by_pk(d, list(keys = "id"))
  expect_identical(d$id, c(1L, 2L, 3L))
  expect_identical(unlist(d$extra), c("a", "b", "c"))
})
