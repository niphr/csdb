# psql and bcp load the file that write_data_infile() writes. On 2026-10-05
# a norsyss import failed because fwrite() wrote the double 100000 as "1e+05",
# and PostgreSQL refused it for an integer column.

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
