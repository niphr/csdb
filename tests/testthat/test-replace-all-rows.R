# replace_all_rows() either replaces every row of the table or leaves it
# unchanged. The three blocks share one table, so each block builds it again
# from the same fixture.

replace_fixture <- function(.local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(id = "INTEGER", d = "DATE", txt = "TEXT"),
    keys = "id"
  )
  suppressMessages(tab$connect())
  withr::defer(tab$disconnect(), envir = .local_envir)
  first <- data.table::data.table(
    id = 1:3,
    d = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    txt = c("a", "b", "c")
  )
  suppressMessages(tab$insert_data(first))
  stopifnot(tab$nrow(use_count = TRUE) == 3)
  second <- data.table::data.table(
    id = c(10L, 20L),
    d = as.Date(c("2024-02-29", "2025-12-31")),
    txt = c("new1", "new2")
  )
  tab$replace_all_rows(second)
  list(tab = tab, second = second)
}

read_rows <- function(tab) {
  out <- dplyr::collect(tab$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "id")
  out[]
}

test_that("replace_all_rows leaves exactly the new rows, Date included", {
  f <- replace_fixture()
  out <- read_rows(f$tab)
  expect_identical(out$id, c(10L, 20L))
  expect_s3_class(out$d, "Date")
  expect_identical(out$d, as.Date(c("2024-02-29", "2025-12-31")))
  expect_identical(out$txt, c("new1", "new2"))
})

test_that("replace_all_rows with a duplicate key errors and keeps the old rows", {
  f <- replace_fixture()
  dup <- data.table::data.table(
    id = c(7L, 7L),
    d = as.Date(c("2030-01-01", "2030-01-02")),
    txt = c("dup1", "dup2")
  )
  expect_error(f$tab$replace_all_rows(dup), "UNIQUE constraint failed")
  out <- read_rows(f$tab)
  expect_identical(out$id, c(10L, 20L))
  expect_identical(out$d, as.Date(c("2024-02-29", "2025-12-31")))
  expect_identical(out$txt, c("new1", "new2"))
})

test_that("replace_all_rows stops on a column mismatch before any statement", {
  f <- replace_fixture()
  wrong <- data.table::data.table(
    id = 99L,
    d = as.Date("2030-01-01"),
    other = "x"
  )
  expect_error(
    f$tab$replace_all_rows(wrong),
    "the columns of newdata \\(id, d, other\\) differ from the fields of tab"
  )
  out <- read_rows(f$tab)
  expect_identical(out$id, c(10L, 20L))
  expect_identical(out$txt, c("new1", "new2"))
})

test_that("replace_all_rows stops on a duplicated column name before any statement", {
  f <- replace_fixture()
  dup_names <- data.frame(
    id = 99L,
    d = as.Date("2030-01-01"),
    txt = "x",
    txt = "y",
    check.names = FALSE
  )
  expect_identical(names(dup_names), c("id", "d", "txt", "txt"))
  expect_error(
    f$tab$replace_all_rows(dup_names),
    "the columns of newdata (id, d, txt, txt) differ from the fields of tab",
    fixed = TRUE
  )
  out <- read_rows(f$tab)
  expect_identical(out$id, c(10L, 20L))
  expect_identical(out$txt, c("new1", "new2"))
})
