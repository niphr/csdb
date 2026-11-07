# Disclosure control of write_profile(). A SQLite table holds sentinel values,
# and each test reads the YAML text or the parsed YAML that write_profile()
# wrote. The artefact under test is the written file, not the R object.

disclosure_k <- 5L

# 200 rows. The ids are SENTINEL_ID_1 to SENTINEL_ID_50: 48 ids hold 4 rows,
# SENTINEL_ID_49 holds 3 rows and SENTINEL_ID_50 holds 5 rows.
disclosure_data <- function() {
  n <- 200L
  sizes <- c(rep(4L, 48), 3L, 5L)
  sex <- rep(c("female", "male"), length.out = n)
  sex[1:4] <- "rare_level"
  some_missing <- rep(1:10, length.out = n)
  some_missing[c(7, 9)] <- NA
  many_missing <- rep(1:10, length.out = n)
  many_missing[11:20] <- NA
  return(data.frame(
    person_id = rep(paste0("SENTINEL_ID_", 1:50), sizes),
    secret_exclude = "SENTINEL_EXCLUDE",
    secret_schema = "SENTINEL_SCHEMA",
    secret_unlisted = "SENTINEL_UNLISTED",
    sex = sex,
    region = rep(c("north", "south", "east", "west"), each = 50),
    age = seq(0, 100, length.out = n),
    day = as.Date("2024-01-01") + c(rep(0:27, each = 7), 40:43),
    some_missing = some_missing,
    many_missing = many_missing
  ))
}

disclosure_field_types <- c(
  person_id = "TEXT",
  secret_exclude = "TEXT",
  secret_schema = "TEXT",
  secret_unlisted = "TEXT",
  sex = "TEXT",
  region = "TEXT",
  age = "DOUBLE",
  day = "DATE",
  some_missing = "INTEGER",
  many_missing = "INTEGER"
)

# Write the data to a SQLite file, profile it through a DBConnection_v9, and
# write the profile. Returns the path of the YAML file.
disclosure_yaml <- function(.local_envir = parent.frame()) {
  dbconfig <- sqlite_dbconfig(.local_envir = .local_envir)
  db <- sqlite_connection(dbconfig)
  withr::defer(db$disconnect(), envir = .local_envir)
  DBI::dbWriteTable(
    db$autoconnection,
    "disclosure",
    disclosure_data(),
    field.types = c(day = "DATE")
  )
  p <- profile_query(
    db,
    "SELECT * FROM disclosure",
    columns = c(
      person_id = "exclude",
      secret_exclude = "exclude",
      secret_schema = "schema_only",
      sex = "full",
      region = "full",
      age = "coarsen",
      day = "coarsen",
      some_missing = "full",
      many_missing = "full"
    ),
    field_types = disclosure_field_types,
    k = disclosure_k,
    group_sizes = list(rows_per_id = "person_id"),
    crosstabs = list(c("sex", "region")),
    dependencies = list(c("region", "sex"))
  )
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = .local_envir)
  write_profile(p, path)
  return(path)
}

disclosure_text <- function(path) {
  return(readLines(path, warn = FALSE))
}

# Every count in a parsed profile: each element named `n` or `suppressed_n`,
# at any depth. NA marks a suppressed inner bin and is not a count.
disclosure_counts <- function(x) {
  if (!is.list(x)) {
    return(numeric(0))
  }
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep("", length(x))
  }
  out <- numeric(0)
  for (i in seq_along(x)) {
    if (nm[i] %in% c("n", "suppressed_n")) {
      out <- c(out, as.numeric(unlist(x[[i]])))
    } else {
      out <- c(out, disclosure_counts(x[[i]]))
    }
  }
  return(out[!is.na(out)])
}

# The counts of missing values and of present values that each numeric
# missing_share gives with n_rows.
disclosure_missing_counts <- function(parsed) {
  out <- numeric(0)
  for (entry in parsed$columns) {
    if (is.numeric(entry$missing_share)) {
      m <- round(entry$missing_share * parsed$n_rows)
      out <- c(out, m, parsed$n_rows - m)
    }
  }
  return(out)
}

test_that("the fixture holds what the disclosure tests need", {
  d <- disclosure_data()
  expect_equal(nrow(d), 200L)
  expect_equal(sum(d$sex == "rare_level"), disclosure_k - 1L)
  expect_equal(min(d$age), 0)
  expect_equal(max(d$age), 100)
  expect_equal(sum(is.na(d$some_missing)), 2L)
  expect_equal(sum(is.na(d$many_missing)), 10L)
  expect_true("SENTINEL_ID_42" %in% d$person_id)
})

test_that("an exclude column's values are not written", {
  path <- disclosure_yaml()
  expect_false(any(grepl(
    "SENTINEL_EXCLUDE",
    disclosure_text(path),
    fixed = TRUE
  )))
  parsed <- yaml::read_yaml(path)
  expect_null(parsed$columns$secret_exclude)
  expect_identical(parsed$field_types$secret_exclude, "TEXT")
})

test_that("a schema_only column's values are not written", {
  path <- disclosure_yaml()
  expect_false(any(grepl(
    "SENTINEL_SCHEMA",
    disclosure_text(path),
    fixed = TRUE
  )))
  parsed <- yaml::read_yaml(path)
  expect_identical(
    names(parsed$columns$secret_schema),
    c("type", "sensitivity", "missing_share")
  )
})

test_that("an unlisted column is schema_only and its values are not written", {
  path <- disclosure_yaml()
  expect_false(any(grepl(
    "SENTINEL_UNLISTED",
    disclosure_text(path),
    fixed = TRUE
  )))
  parsed <- yaml::read_yaml(path)
  expect_identical(parsed$columns$secret_unlisted$sensitivity, "schema_only")
})

test_that("a group_sizes id column's values are not written", {
  path <- disclosure_yaml()
  text <- disclosure_text(path)
  expect_false(any(grepl("SENTINEL_ID_42", text, fixed = TRUE)))
  expect_false(any(grepl("SENTINEL_ID", text, fixed = TRUE)))
  # The group sizes are written. 48 ids hold 4 rows, one id 3 rows and one
  # id 5 rows, so the sizes merge into one open range of 50 ids from 1 up.
  gs <- yaml::read_yaml(path)$group_sizes$rows_per_id
  expect_identical(gs$column, "person_id")
  expect_identical(gs$size_from, 1L)
  expect_length(gs$size_to, 0)
  expect_identical(gs$n, 50L)
})

test_that("a full TEXT level with a count of k - 1 is not written", {
  path <- disclosure_yaml()
  expect_false(any(grepl("rare_level", disclosure_text(path), fixed = TRUE)))
  sex <- yaml::read_yaml(path)$columns$sex
  expect_identical(sex$levels, c("female", "male"))
})

test_that("no count below k is written", {
  parsed <- yaml::read_yaml(disclosure_yaml())
  # The scan reaches every kind of cell: levels, weeks, bins, crosstab
  # cells and group sizes.
  expect_gt(length(disclosure_counts(parsed$columns$sex)), 0)
  expect_gt(length(disclosure_counts(parsed$columns$day)), 0)
  expect_gt(length(disclosure_counts(parsed$columns$age)), 0)
  expect_gt(length(disclosure_counts(parsed$crosstabs)), 0)
  expect_gt(length(disclosure_counts(parsed$group_sizes)), 0)
  expect_gte(parsed$n_rows, disclosure_k)
  cells <- disclosure_counts(parsed)
  expect_identical(cells[cells < disclosure_k], numeric(0))
  # A share of 0 or 1 gives a count of 0, which the contract allows.
  missing <- disclosure_missing_counts(parsed)
  expect_identical(missing[missing > 0 & missing < disclosure_k], numeric(0))
})

test_that("no stored break equals an observed extreme", {
  age <- yaml::read_yaml(disclosure_yaml())$columns$age
  breaks <- as.numeric(age$breaks)
  expect_gt(length(breaks), 2)
  expect_false(any(breaks %in% c(0, 100)))
  expect_lt(breaks[1], 0)
  expect_gt(breaks[length(breaks)], 100)
})

test_that("a small count of missing values is suppressed", {
  columns <- yaml::read_yaml(disclosure_yaml())$columns
  expect_identical(columns$some_missing$missing_share, "suppressed")
  # The positive controls: no missing value, and 10 of 200 missing.
  expect_identical(columns$sex$missing_share, 0)
  expect_identical(columns$many_missing$missing_share, 10 / 200)
})

# 400 rows. Each column needs a coarser resolution than the one before, so
# the written profile holds weeks, months, quarters, runs of quarters,
# merged bins and ranges of group sizes.
merged_data <- function() {
  rows <- function(...) {
    x <- c(...)
    return(rep(as.Date(names(x)), x))
  }
  sizes <- c(rep(1:15, each = 2), rep(20L, 8))
  return(data.frame(
    id = paste0("SENTINEL_ID_", rep(seq_along(sizes), sizes)),
    wk = as.Date("2024-01-01") + rep(0:55, length.out = 400),
    mo = rows("2024-01-01" = 98, "2024-01-31" = 2, "2024-02-15" = 100,
      "2024-03-15" = 100, "2024-04-15" = 100),
    qu = rows("2024-01-01" = 97, "2024-02-15" = 3, "2024-04-01" = 100,
      "2024-07-01" = 100, "2024-10-01" = 100),
    runs = rows("2024-01-01" = 300, "2024-07-01" = 3, "2025-01-01" = 97),
    x = c(rep(0:19, each = 10), rep(40:59, each = 10))
  ))
}

test_that("merged weeks, months, quarters and size ranges write no count below k", {
  d <- merged_data()
  expect_identical(nrow(d), 400L)
  p <- profile_data(
    d,
    c(id = "TEXT", wk = "DATE", mo = "DATE", qu = "DATE", runs = "DATE",
      x = "INTEGER"),
    columns = c(id = "exclude", wk = "coarsen", mo = "full", qu = "full",
      runs = "full", x = "coarsen"),
    group_sizes = list(rows_per_id = "id"),
    k = disclosure_k
  )
  path <- withr::local_tempfile(fileext = ".yaml")
  write_profile(p, path)
  expect_identical(read_profile(path), p)
  expect_false(any(grepl("SENTINEL_ID", disclosure_text(path), fixed = TRUE)))
  parsed <- yaml::read_yaml(path)
  columns <- parsed$columns
  # Each merged form is present, so the scan below reaches it.
  expect_true(all(c("weeks", "n") %in% names(columns$wk)))
  expect_true(all(c("months", "n") %in% names(columns$mo)))
  expect_true(all(c("quarters", "n") %in% names(columns$qu)))
  expect_true(all(c("quarters", "quarters_to", "n") %in% names(columns$runs)))
  expect_lt(length(columns$x$n), length(columns$x$breaks))
  gs <- parsed$group_sizes$rows_per_id
  expect_true(all(c("size_from", "size_to", "n") %in% names(gs)))
  expect_gt(length(gs$n), 1)
  cells <- disclosure_counts(parsed)
  expect_gt(length(cells), 20)
  expect_identical(cells[cells < disclosure_k], numeric(0))
  # Merging drops no row: each table counts every row or every id.
  for (x in c("wk", "mo", "qu", "runs", "x")) {
    expect_identical(sum(unlist(columns[[x]]$n)), 400L, label = x)
  }
  expect_identical(sum(unlist(gs$n)), 38L)
})

test_that("group-size ranges publish no exact smallest or largest size", {
  # Row counts from 3 to 25. Ten ids hold the smallest count, 3, and ten
  # ids the largest, 25, so each alone fills a range of k ids.
  sizes <- c(rep(3L, 10), rep(4:20, each = 3), rep(25L, 10))
  d <- data.frame(id = paste0("SENTINEL_ID_", rep(seq_along(sizes), sizes)))
  p <- profile_data(
    d,
    c(id = "TEXT"),
    columns = c(id = "exclude"),
    group_sizes = list(rows_per_id = "id"),
    k = disclosure_k
  )
  path <- withr::local_tempfile(fileext = ".yaml")
  write_profile(p, path)
  expect_identical(read_profile(path), p)
  gs <- yaml::read_yaml(path)$group_sizes$rows_per_id
  ends <- c(unlist(gs$size_from), unlist(gs$size_to))
  expect_gt(length(gs$n), 2)
  expect_identical(gs$size_from[[1]], 1L)
  expect_length(gs$size_to, length(gs$n) - 1L)
  expect_false(any(ends %in% c(min(sizes), max(sizes))))
  expect_identical(sum(unlist(gs$n)), length(sizes))
  # The fixture of disclosure_yaml() too: row counts 3 to 5.
  fixture <- yaml::read_yaml(disclosure_yaml())$group_sizes$rows_per_id
  fixture_ends <- c(unlist(fixture$size_from), unlist(fixture$size_to))
  expect_false(any(fixture_ends %in% c(3, 5)))
})
