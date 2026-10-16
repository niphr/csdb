# The psql load writes a file and names its format in the \copy command.
# pg_file_round_trip() reads the file back by the rules of the format that the
# command names, as PostgreSQL does. A value that reads back different here is
# stored different by PostgreSQL.
#
# No PostgreSQL server is needed. DBI::ANSI() is the connection, and
# DBI::dbListFields() is mocked to give the columns of the data.table.

# The options of the command since 2026.10.16, and of the command before.
pg_file_options <- "(FORMAT TEXT, DELIMITER '\t', NULL '\\N', ENCODING 'UTF8')"
pg_file_options_old <- "(FORMAT CSV, DELIMITER '\t')"

# Read a file in PostgreSQL's text format, with DELIMITER tab, NULL \N and
# ENCODING UTF8. PostgreSQL and psql stop the load where this function stops.
#
# file     The load file.
# returns  A list with one character vector for each row. NA is a NULL.
pg_text_parse <- function(file) {
  text <- rawToChar(readBin(file, "raw", file.info(file)$size))
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    stop("invalid byte sequence for encoding \"UTF8\"")
  }
  if (!endsWith(text, "\n")) {
    stop("the last row has no line end")
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  rows <- vector("list", length(lines))
  for (k in seq_along(lines)) {
    if (identical(lines[[k]], "\\.")) {
      stop("end-of-copy marker found in data")
    }
    if (grepl("\r", lines[[k]], fixed = TRUE)) {
      stop("literal carriage return found in data")
    }
    # The added tab keeps an empty last field, which strsplit() drops.
    fields <- strsplit(paste0(lines[[k]], "\t"), "\t", fixed = TRUE)[[1]]
    rows[[k]] <- vapply(
      fields,
      pg_text_unescape,
      character(1),
      USE.NAMES = FALSE
    )
  }
  return(rows)
}

# One field of the text format. The NULL string is compared before any
# escape is read, so only an unescaped \N is NULL.
pg_text_unescape <- function(field) {
  if (identical(field, "\\N")) {
    return(NA_character_)
  }
  chars <- strsplit(field, "")[[1]]
  out <- character(0)
  i <- 1L
  while (i <= length(chars)) {
    if (chars[[i]] != "\\" || i == length(chars)) {
      out <- c(out, chars[[i]])
      i <- i + 1L
      next
    }
    rest <- paste(chars[-seq_len(i)], collapse = "")
    # \ddd is an octal byte and \xhh a hexadecimal byte.
    code <- regmatches(rest, regexpr("^([0-7]{1,3}|x[0-9A-Fa-f]{1,2})", rest))
    if (length(code) == 1L) {
      value <- if (startsWith(code, "x")) {
        strtoi(substring(code, 2), 16L)
      } else {
        strtoi(code, 8L)
      }
      out <- c(out, intToUtf8(value))
      i <- i + 1L + nchar(code)
      next
    }
    nx <- chars[[i + 1L]]
    out <- c(
      out,
      switch(nx, b = "\b", f = "\f", n = "\n", r = "\r", t = "\t", v = "\v", nx)
    )
    i <- i + 2L
  }
  return(paste(out, collapse = ""))
}

# Read a file in PostgreSQL's CSV format with DELIMITER tab and the default
# QUOTE ", ESCAPE " and NULL '', an unquoted empty field. That was the format
# of the command before 2026.10.16.
#
# file     The load file.
# returns  A list with one character vector for each row. NA is a NULL.
pg_csv_parse <- function(file) {
  bytes <- readBin(file, "raw", file.info(file)$size)
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    stop("invalid byte sequence for encoding \"UTF8\"")
  }
  chars <- strsplit(text, "")[[1]]
  rows <- list()
  row <- character(0)
  buf <- character(0)
  quoted <- FALSE
  in_quote <- FALSE
  end_field <- function() {
    value <- paste(buf, collapse = "")
    # Only an unquoted field can be the NULL string. A quoted "" is a value.
    if (!quoted && identical(value, "")) {
      value <- NA_character_
    }
    row <<- c(row, value)
    buf <<- character(0)
    quoted <<- FALSE
    return(invisible(NULL))
  }
  i <- 1L
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (in_quote) {
      if (ch == "\"" && i < length(chars) && chars[[i + 1L]] == "\"") {
        buf <- c(buf, "\"")
        i <- i + 1L
      } else if (ch == "\"") {
        in_quote <- FALSE
      } else {
        buf <- c(buf, ch)
      }
    } else if (ch == "\"") {
      in_quote <- TRUE
      quoted <- TRUE
    } else if (ch == "\t") {
      end_field()
    } else if (ch == "\r") {
      stop("unquoted carriage return found in data")
    } else if (ch == "\n") {
      line_quoted <- quoted
      end_field()
      if (length(row) == 1L && !line_quoted && identical(row, "\\.")) {
        stop("end-of-copy marker found in data")
      }
      rows[[length(rows) + 1L]] <- row
      row <- character(0)
    } else {
      buf <- c(buf, ch)
    }
    i <- i + 1L
  }
  if (in_quote) {
    stop("unterminated CSV quoted field")
  }
  if (length(buf) > 0L || length(row) > 0L) {
    stop("the last row has no line end")
  }
  return(rows)
}

# Write `dt` with pg_write_copy_command() and read the file back by the
# format that the command names.
#
# returns  A list: `rows`, one character vector for each row, and `sql`, the
#          \copy command.
pg_file_round_trip <- function(dt) {
  file <- withr::local_tempfile(fileext = ".txt")
  sql <- with_mocked_bindings(
    pg_write_copy_command(DBI::ANSI(), "tab", dt, file),
    dbListFields = function(conn, name, ...) names(dt),
    .package = "DBI"
  )
  if (endsWith(sql, pg_file_options)) {
    rows <- pg_text_parse(file)
  } else if (endsWith(sql, pg_file_options_old)) {
    rows <- pg_csv_parse(file)
  } else {
    stop("no parser for the options of: ", sql)
  }
  return(list(rows = rows, sql = sql))
}

# Column `j` of the parsed rows.
pg_file_column <- function(rows, j) {
  return(vapply(rows, function(r) r[[j]], character(1)))
}

pg_edge_text <- c(
  "\"",
  "a\"b",
  "\"\"",
  "\\",
  "a\\b",
  "\t",
  "a\tb",
  "\n",
  "a\nb",
  "\r",
  "a\r\nb",
  "\\.",
  "a\n\\.\nb",
  "\\N",
  "NA",
  "NULL",
  ",",
  "'",
  " lead",
  "trail ",
  "\u00e6\u00f8\u00e5",
  "\u65e5\u672c\u8a9e",
  "\U0001F600"
)

test_that("the psql load file keeps quotes, backslashes, tabs, line ends and unicode", {
  dt <- data.table::data.table(id = seq_along(pg_edge_text), txt = pg_edge_text)
  got <- pg_file_round_trip(dt)
  expect_length(got$rows, length(pg_edge_text))
  expect_identical(pg_file_column(got$rows, 2L), pg_edge_text)
  expect_identical(
    pg_file_column(got$rows, 1L),
    as.character(seq_along(pg_edge_text))
  )
})

test_that("the psql load file holds a latin1 string as UTF-8", {
  x <- rawToChar(as.raw(c(0xe6, 0xf8, 0xe5)))
  Encoding(x) <- "latin1"
  dt <- data.table::data.table(id = 1L, txt = x)
  got <- pg_file_round_trip(dt)
  expect_identical(pg_file_column(got$rows, 2L), intToUtf8(c(230, 248, 229)))
})

test_that("the psql load file writes a factor as its labels", {
  dt <- data.table::data.table(id = 1:3, f = factor(c("a\tb", "c\\d", NA)))
  got <- pg_file_round_trip(dt)
  expect_identical(pg_file_column(got$rows, 2L), c("a\tb", "c\\d", NA))
})

test_that("the psql load file reads back a text value with a double quote", {
  dt <- data.table::data.table(id = 1:2, txt = c("a\"b", "x"))
  got <- pg_file_round_trip(dt)
  expect_identical(got$rows, list(c("1", "a\"b"), c("2", "x")))
})

test_that("the psql load file keeps an empty string apart from NA", {
  dt <- data.table::data.table(
    id = 1:4,
    txt = c("", NA, "x", ""),
    n = c(1L, NA, 3L, 4L)
  )
  got <- pg_file_round_trip(dt)
  expect_identical(pg_file_column(got$rows, 2L), c("", NA, "x", ""))
  expect_identical(pg_file_column(got$rows, 3L), c("1", NA, "3", "4"))
})

test_that("the psql copy command names the format that the file has", {
  dt <- data.table::data.table(id = 1L, txt = "a")
  got <- pg_file_round_trip(dt)
  expect_true(endsWith(got$sql, pg_file_options))
})

test_that("the psql load file holds each double as text that reads back as the same double", {
  x <- c(
    0.1 + 0.2,
    1 / 3,
    pi,
    -2.5e-8,
    1e-300,
    5e-324,
    .Machine$double.xmax,
    123456.7890123,
    100000,
    2^53 + 2,
    2^62,
    -0,
    NA
  )
  dt <- data.table::data.table(id = seq_along(x), x = x)
  got <- pg_file_round_trip(dt)
  text <- pg_file_column(got$rows, 2L)
  expect_identical(as.numeric(text), x)
  # A whole double has no exponent, so an INTEGER or BIGINT column takes it.
  expect_identical(
    text[9:11],
    c("100000", "9007199254740994", "4611686018427387904")
  )
  expect_identical(text[[12]], "-0")
})

test_that("the psql load file holds the shortest text of each double", {
  expect_identical(
    pg_double_text(c(0.1, 0.1 + 0.2, 1 / 3, 1e-300, 123456.5, 5, -7)),
    c(
      "0.1",
      "0.30000000000000004",
      "0.3333333333333333",
      "1e-300",
      "123456.5",
      "5",
      "-7"
    )
  )
})

test_that("the psql load file holds each whole double below 2^63 in full", {
  withr::local_seed(37)
  x <- round(runif(2000) * 10^sample(0:18, 2000, replace = TRUE))
  x <- x * sample(c(-1, 1), 2000, replace = TRUE)
  expect_identical(pg_double_text(x), sprintf("%.0f", x))
})

test_that("the psql load file writes a Date and a POSIXct as text", {
  dt <- data.table::data.table(
    id = 1:2,
    d = as.Date(c("2024-01-02", NA)),
    t = as.POSIXct(c("2024-01-02 03:04:05", NA), tz = "UTC")
  )
  got <- pg_file_round_trip(dt)
  expect_identical(
    got$rows,
    list(c("1", "2024-01-02", "2024-01-02 03:04:05"), c("2", NA, NA))
  )
})

test_that("the psql load file holds Inf, -Inf and NaN as NULL", {
  dt <- data.table::data.table(id = 1:4, x = c(Inf, -Inf, NaN, 1.5))
  got <- pg_file_round_trip(dt)
  expect_identical(pg_file_column(got$rows, 2L), c(NA, NA, NA, "1.5"))
})

test_that("the psql load leaves the caller's data.table unchanged", {
  dt <- data.table::data.table(id = 1:2, x = c(0.5, 1 / 3), txt = c("a", ""))
  before <- data.table::copy(dt)
  pg_file_round_trip(dt)
  expect_identical(dt, before)
})

# data.table() refuses the name .SD, so setnames() gives it.
pg_awkward_names <- function() {
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
  dt <- pg_awkward_names()
  scrub_non_finite(dt)
  expect_identical(dt$i, c(1, NA))
  expect_identical(dt$j, c(NA, 2))
  expect_identical(dt$by, c("a", "b"))
  expect_identical(dt$.SD, c(NA, 3))
})

test_that("write_data_infile() works on columns named i, j, by, .SD and x", {
  file <- withr::local_tempfile(fileext = ".csv")
  write_data_infile(pg_awkward_names(), file = file, na = "")
  expect_identical(
    readLines(file),
    c("i,j,by,.SD,x", "1,,a,,2024-01-02 03:04:05", ",2,b,3,")
  )
})

test_that("the psql load file works on columns named i, j, by, .SD and x", {
  got <- pg_file_round_trip(pg_awkward_names())
  expect_identical(
    got$rows,
    list(
      c("1", NA, "a", NA, "2024-01-02 03:04:05"),
      c(NA, "2", "b", "3", NA)
    )
  )
})
