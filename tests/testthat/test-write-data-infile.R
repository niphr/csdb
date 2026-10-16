# bcp and the MySQL load read the file that write_data_infile() writes. On
# 2026-10-05 a norsyss import through psql failed because fwrite() wrote the
# double 100000 as "1e+05", and PostgreSQL refused it for an integer column.

test_that("write_data_infile() never writes scientific notation", {
  # R starts with scipen = 0, so the test uses that value
  withr::local_options(scipen = 0)
  dt <- data.table::data.table(
    x = c(100000, 1e7, 123456789, 0.5),
    n = 100000L
  )
  file <- withr::local_tempfile(fileext = ".csv")
  write_data_infile(dt, file = file)
  lines <- readLines(file)

  expect_identical(
    lines,
    c(
      "x,n",
      "100000,100000",
      "10000000,100000",
      "123456789,100000",
      "0.5,100000"
    )
  )
  expect_false(any(grepl("e[+-]", lines)))
})

# data.table() refuses the name .SD, so setnames() gives it.
awkward_names <- function() {
  dt <- data.table::data.table(
    i = c(1, Inf),
    j = c(NaN, 2),
    by = c("a", "b"),
    sd = c(-Inf, 3),
    x = as.POSIXct(c("2024-01-02 03:04:05", NA), tz = "UTC")
  )
  data.table::setnames(dt, "sd", ".SD")
  return(dt)
}

test_that("scrub_non_finite() works on columns named i, j, by, .SD and x", {
  dt <- awkward_names()
  scrub_non_finite(dt)
  expect_identical(dt$i, c(1, NA))
  expect_identical(dt$j, c(NA, 2))
  expect_identical(dt$by, c("a", "b"))
  expect_identical(dt$.SD, c(NA, 3))
})

test_that("write_data_infile() works on columns named i, j, by, .SD and x", {
  file <- withr::local_tempfile(fileext = ".csv")
  write_data_infile(awkward_names(), file = file, na = "")
  expect_identical(
    readLines(file),
    c("i,j,by,.SD,x", "1,,a,,2024-01-02 03:04:05", ",2,b,3,")
  )
})
