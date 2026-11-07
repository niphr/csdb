# The summaries that profile_query() builds for each sensitivity level and
# each joint structure, and the YAML round trip.

test_that("schema_only holds type, sensitivity and missing_share only", {
  d <- data.frame(a = rep(c("x", "y"), 10))
  p <- profile_data(d, c(a = "TEXT"))
  expect_identical(
    p$columns$a,
    list(type = "TEXT", sensitivity = "schema_only", missing_share = 0)
  )
})

test_that("an exclude column is listed in field_types only", {
  d <- data.frame(a = rep(c("x", "y"), 10), b = 1:20)
  p <- profile_data(d, c(a = "TEXT", b = "INTEGER"), columns = c(b = "exclude"))
  expect_identical(names(p$columns), "a")
  expect_identical(p$field_types, c(a = "TEXT", b = "INTEGER"))
})

test_that("full TEXT at most max_levels publishes suppressed levels and n", {
  d <- data.frame(
    a = c(rep("x", 10), rep("y", 6), "r1", "r1", "r2", "r2", "r3", "r3"),
    b = c(rep("x", 20), "r1", "r2")
  )
  p <- profile_data(
    d,
    c(a = "TEXT", b = "TEXT"),
    columns = c(a = "full", b = "full")
  )
  expect_identical(p$columns$a$levels, c("x", "y"))
  expect_identical(p$columns$a$n, c(10L, 6L))
  # Three levels of 2 rows sum to 6, which is at least k = 5.
  expect_identical(p$columns$a$suppressed_n, 6L)
  # Two levels of 1 row sum to 2, so suppressed_n is not kept.
  expect_identical(p$columns$b$levels, "x")
  expect_false("suppressed_n" %in% names(p$columns$b))
})

test_that("full TEXT above max_levels publishes n_distinct rounded down to a multiple of k", {
  d <- data.frame(a = sprintf("v%02d", rep(1:63, 2)))
  p <- profile_data(d, c(a = "TEXT"), columns = c(a = "full"))
  expect_identical(p$columns$a$n_distinct, 60L)
  expect_null(p$columns$a$levels)
  p7 <- profile_data(d, c(a = "TEXT"), columns = c(a = "full"), k = 7L)
  expect_identical(p7$columns$a$n_distinct, 63L)
  p10 <- profile_data(d, c(a = "TEXT"), columns = c(a = "full"), k = 10L)
  expect_identical(p10$columns$a$n_distinct, 60L)
})

test_that("full BOOLEAN above max_levels publishes n_distinct", {
  d <- data.frame(flag = rep(c(TRUE, FALSE), 5))
  # 2 distinct values, above max_levels = 1. With k = 2, n_distinct is 2.
  p <- profile_data(
    d,
    c(flag = "BOOLEAN"),
    columns = c(flag = "full"),
    max_levels = 1L,
    k = 2L
  )
  expect_identical(
    p$columns$flag,
    list(
      type = "BOOLEAN",
      sensitivity = "full",
      missing_share = 0,
      n_distinct = 2L
    )
  )
})

test_that("full DATE publishes counts per day", {
  d <- data.frame(
    day = as.Date("2024-03-01") + c(rep(0, 6), rep(1, 5), rep(3, 5))
  )
  p <- profile_data(d, c(day = "DATE"), columns = c(day = "full"))
  expect_identical(
    p$columns$day$days,
    as.Date(c("2024-03-01", "2024-03-02", "2024-03-04"))
  )
  expect_identical(p$columns$day$n, c(6L, 5L, 5L))
})

test_that("a year of sparse days merges into weeks rather than disappearing", {
  # 365 days from Tuesday 2024-01-02, 3 rows a day. The first ISO week
  # holds 6 days and the last week 2 days, so every week holds 6 rows or
  # more, and no day holds k = 5.
  d <- data.frame(day = as.Date("2024-01-02") + rep(0:364, each = 3))
  p <- profile_data(d, c(day = "DATE"), columns = c(day = "full"), k = 5L)
  day <- p$columns$day
  expect_identical(
    names(day),
    c("type", "sensitivity", "missing_share", "weeks", "n")
  )
  expect_length(day$weeks, 53L)
  expect_identical(format(day$weeks, "%u"), rep("1", 53))
  expect_true(all(day$n >= 5L))
  expect_identical(sum(day$n), p$n_rows)
  expect_identical(p$n_rows, 1095L)
})

test_that("days and weeks merge into months, quarters and runs of quarters", {
  day <- function(x) as.Date(x)
  rows <- function(...) {
    x <- c(...)
    return(rep(day(names(x)), x))
  }
  # Each column holds 400 rows. The small cell decides the resolution.
  d <- data.frame(
    mo = rows(
      "2024-01-01" = 98,
      "2024-01-31" = 2,
      "2024-02-15" = 100,
      "2024-03-15" = 100,
      "2024-04-15" = 100
    ),
    qu = rows(
      "2024-01-01" = 97,
      "2024-02-15" = 3,
      "2024-04-01" = 100,
      "2024-07-01" = 100,
      "2024-10-01" = 100
    ),
    runs = rows("2024-01-01" = 300, "2024-07-01" = 3, "2025-01-01" = 97)
  )
  p <- profile_data(
    d,
    c(mo = "DATE", qu = "DATE", runs = "DATE"),
    columns = c(mo = "full", qu = "full", runs = "coarsen")
  )
  expect_identical(
    p$columns$mo$months,
    day(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"))
  )
  expect_identical(p$columns$mo$n, rep(100L, 4))
  expect_identical(
    p$columns$qu$quarters,
    day(c("2024-01-01", "2024-04-01", "2024-07-01", "2024-10-01"))
  )
  expect_null(p$columns$qu$quarters_to)
  expect_identical(p$columns$runs$quarters, day(c("2024-01-01", "2024-07-01")))
  # runs is coarsen, so its days go to weeks first. The week of 2025-01-01
  # starts on Monday 2024-12-30 and so merges into the quarter of that day.
  expect_identical(
    p$columns$runs$quarters_to,
    day(c("2024-01-01", "2024-10-01"))
  )
  expect_identical(p$columns$runs$n, c(300L, 100L))
})

test_that("full DATETIME publishes levels to the second, or counts per UTC day above max_levels", {
  t0 <- as.POSIXct("2024-03-01 23:00:00", tz = "UTC")
  d <- data.frame(t = t0 + c(rep(0.25, 5), rep(0.75, 5), rep(3600, 6)))
  p <- profile_data(d, c(t = "DATETIME"), columns = c(t = "full"))
  expect_identical(
    p$columns$t$levels,
    as.POSIXct(c("2024-03-01 23:00:00", "2024-03-02 00:00:00"), tz = "UTC")
  )
  expect_identical(p$columns$t$n, c(10L, 6L))
  d2 <- data.frame(t = t0 + c(1:10, 3600 + 1:10))
  p2 <- profile_data(
    d2,
    c(t = "DATETIME"),
    columns = c(t = "full"),
    max_levels = 5L
  )
  expect_identical(p2$columns$t$days, as.Date(c("2024-03-01", "2024-03-02")))
  expect_identical(p2$columns$t$n, c(10L, 10L))
  expect_null(p2$columns$t$levels)
})

test_that("coarsen DATE and DATETIME publish counts per ISO week keyed by its Monday", {
  # 2024-01-01 is a Monday. The days run to Sunday 2024-01-14.
  days <- as.Date("2024-01-01") + rep(0:13, each = 2)
  d <- data.frame(
    day = days,
    t = as.POSIXct(paste(days, "12:30:00"), tz = "UTC")
  )
  p <- profile_data(
    d,
    c(day = "DATE", t = "DATETIME"),
    columns = c(day = "coarsen", t = "coarsen")
  )
  mondays <- as.Date(c("2024-01-01", "2024-01-08"))
  expect_identical(p$columns$day$weeks, mondays)
  expect_identical(p$columns$day$n, c(14L, 14L))
  expect_identical(p$columns$t$weeks, mondays)
  expect_identical(p$columns$t$n, c(14L, 14L))
  expect_identical(format(p$columns$day$weeks, "%u"), c("1", "1"))
})

test_that("INTEGER and DOUBLE bins merge every bin below k with its neighbours", {
  # 0 to 59, 10 rows each, then one row of 100. pretty() gives breaks 0 to
  # 100 by 5, and the end breaks move outward to -5 and 105.
  x <- c(rep(0:59, each = 10), 100L)
  p <- profile_data(
    data.frame(x = x),
    c(x = "INTEGER"),
    columns = c(x = "coarsen")
  )
  expect_identical(p$columns$x$breaks, c(-5, seq(5, 55, by = 5), 105))
  expect_identical(p$columns$x$n, c(rep(50L, 11), 51L))
  # A gap from 20 to 39 gives inner bins of 0 rows. They merge with the
  # next bin, [40, 45). The 40 distinct values are above max_levels, so the
  # full column is binned.
  y <- c(rep(0:19, each = 10), rep(40:59, each = 10), 0L, 59L)
  q <- profile_data(
    data.frame(y = y),
    c(y = "INTEGER"),
    columns = c(y = "full"),
    max_levels = 39L
  )
  expect_identical(q$columns$y$breaks, c(-5, 5, 10, 15, 20, 45, 50, 55, 60))
  expect_identical(q$columns$y$n, c(51L, rep(50L, 6), 51L))
  expect_false(anyNA(q$columns$y$n))
  expect_false("suppressed_n" %in% names(q$columns$y))
  # A full column at most max_levels publishes its values instead.
  r <- profile_data(
    data.frame(z = rep(c(1.5, 2.5), 5)),
    c(z = "DOUBLE"),
    columns = c(z = "full")
  )
  expect_identical(r$columns$z$levels, c(1.5, 2.5))
  # A level below k is not dropped: the column is binned instead.
  s <- profile_data(
    data.frame(z = rep(c(1L, 2L, 3L), c(10, 3, 10))),
    c(z = "INTEGER"),
    columns = c(z = "full")
  )
  expect_null(s$columns$z$levels)
  expect_identical(sum(s$columns$z$n), 23L)
  expect_true(all(s$columns$z$n >= 5L))
})

test_that("a column of one value gets breaks that differ from that value", {
  p <- profile_data(
    data.frame(x = rep(5, 10)),
    c(x = "DOUBLE"),
    columns = c(x = "coarsen")
  )
  expect_false(any(p$columns$x$breaks == 5))
  expect_identical(p$columns$x$n, 10L)
})

test_that("coarsen TEXT and coarsen BOOLEAN stop with an error that names the column", {
  d <- data.frame(region = rep(c("a", "b"), 5), flag = rep(c(TRUE, FALSE), 5))
  ft <- c(region = "TEXT", flag = "BOOLEAN")
  expect_error(profile_data(d, ft, columns = c(region = "coarsen")), "region")
  expect_error(profile_data(d, ft, columns = c(flag = "coarsen")), "flag")
})

test_that("crosstabs publish suppressed cell counts of two full columns", {
  d <- data.frame(
    sex = c(rep("f", 12), rep("m", 12)),
    region = c(rep("n", 6), rep("s", 6), rep("n", 10), rep("s", 2))
  )
  ft <- c(sex = "TEXT", region = "TEXT")
  p <- profile_data(
    d,
    ft,
    columns = c(sex = "full", region = "full"),
    crosstabs = list(c("sex", "region"))
  )
  expect_identical(
    p$crosstabs,
    list(list(
      columns = c("sex", "region"),
      levels = list(sex = c("f", "f", "m"), region = c("n", "s", "n")),
      n = c(6L, 6L, 10L)
    ))
  )
  expect_error(
    profile_data(
      d,
      ft,
      columns = c(sex = "full"),
      crosstabs = list(c("sex", "region"))
    ),
    "region"
  )
})

test_that("dependencies hold one logical and store no mapping", {
  d <- data.frame(
    municipality = rep(c("m1", "m2", "m3"), 4),
    county = rep(c("c1", "c1", "c2"), 4),
    other = rep(c("a", "b"), 6)
  )
  p <- profile_data(
    d,
    c(municipality = "TEXT", county = "TEXT", other = "TEXT"),
    dependencies = list(c("municipality", "county"), c("county", "other"))
  )
  expect_identical(
    p$dependencies,
    list(
      list(columns = c("municipality", "county"), holds = TRUE),
      list(columns = c("county", "other"), holds = FALSE)
    )
  )
})

test_that("group_sizes count the values of a column per range of row counts", {
  # Row counts 2, 3, 5, 8 and 9, held by 3, 3, 5, 2 and 4 ids.
  d <- data.frame(id = rep(1:17, rep(c(2, 3, 5, 8, 9), c(3, 3, 5, 2, 4))))
  p <- profile_data(
    d,
    c(id = "INTEGER"),
    columns = c(id = "exclude"),
    group_sizes = list(rows_per_id = "id")
  )
  expect_identical(
    p$group_sizes,
    list(
      rows_per_id = list(
        column = "id",
        size_from = c(1L, 5L, 8L),
        size_to = c(4L, 7L),
        n = c(6L, 5L, 6L)
      )
    )
  )
})

test_that("group sizes merge into ranges", {
  size_ranges <- function(sizes) {
    d <- data.frame(id = rep(seq_along(sizes), sizes))
    p <- profile_data(
      d,
      c(id = "INTEGER"),
      columns = c(id = "exclude"),
      group_sizes = list(g = "id"),
      k = 5L
    )
    return(p$group_sizes$g)
  }
  withr::local_seed(5)
  fixtures <- list(
    # 80 ids: two of each size from 1 to 40 rows.
    every_size = rep(1:40, 2),
    # Sizes 2, 3, 5, 8 and 9, with gaps at 4, 6 and 7.
    gaps = rep(c(2L, 3L, 5L, 8L, 9L), c(3, 3, 5, 2, 4)),
    # A skewed draw with wide gaps between sizes.
    skewed = sample(c(1:5, 12L, 40L, 41L, 90L), 60, TRUE, prob = 9:1)
  )
  for (name in names(fixtures)) {
    sizes <- fixtures[[name]]
    g <- size_ranges(sizes)
    m <- length(g$n)
    expect_gt(m, 1, label = name)
    expect_true(all(g$n >= 5L), label = name)
    expect_identical(sum(g$n), length(sizes), label = name)
    # The ranges tile the row counts from 1: each size_to is one below the
    # next size_from, and the last range has no size_to.
    expect_identical(g$size_from[1], 1L, label = name)
    expect_length(g$size_to, m - 1L)
    for (i in seq_len(m - 1)) {
      expect_identical(
        g$size_to[i] + 1L,
        g$size_from[i + 1],
        label = paste(name, i)
      )
    }
    expect_true(all(g$size_from[-m] <= g$size_to), label = name)
  }
})

roundtrip_data <- function() {
  n <- 30L
  return(data.frame(
    day = as.Date("2024-01-01") + rep(0:2, each = 10),
    t = as.POSIXct("2024-01-01 06:00:00", tz = "UTC") + rep(c(0, 60), 15),
    count = rep(c(1L, 2L, 3L), 10),
    value = rep(c(0.1 + 0.2, 1 / 3), 15),
    flag = rep(c(TRUE, FALSE), 15),
    empty = rep(NA_real_, n),
    label = rep(c("a", "b", "c"), 10)
  ))
}

roundtrip_field_types <- c(
  day = "DATE",
  t = "DATETIME",
  count = "INTEGER",
  value = "DOUBLE",
  flag = "BOOLEAN",
  empty = "DOUBLE",
  label = "TEXT"
)

test_that("a profile survives write_profile() and read_profile() unchanged", {
  p <- profile_data(
    roundtrip_data(),
    roundtrip_field_types,
    columns = c(
      day = "full",
      t = "full",
      count = "full",
      value = "full",
      flag = "full",
      empty = "coarsen",
      label = "full"
    ),
    crosstabs = list(c("day", "flag")),
    dependencies = list(c("label", "count")),
    group_sizes = list(per_label = "label"),
    source = "round trip"
  )
  # The profile holds each type that plain yaml loses.
  expect_s3_class(p$columns$day$days, "Date")
  expect_s3_class(p$columns$t$levels, "POSIXct")
  expect_type(p$columns$count$levels, "integer")
  expect_identical(p$columns$value$levels, c(0.1 + 0.2, 1 / 3))
  expect_type(p$columns$flag$levels, "logical")
  expect_identical(p$columns$empty$breaks, numeric(0))
  expect_identical(p$columns$empty$n, integer(0))
  expect_identical(p$keys, character(0))
  expect_identical(p$notes, character(0))
  path <- withr::local_tempfile(fileext = ".yaml")
  expect_identical(write_profile(p, path), path)
  expect_identical(read_profile(path), p)
})

test_that("a profile with noise, merged bins and suppressed_n survives the round trip", {
  set.seed(20261107)
  x <- c(rep(0:19, each = 10), rep(40:59, each = 10))
  d <- data.frame(
    x = x,
    g = c(rep(c("a", "b"), 197), "r1", "r2", "r3", "r4", "r5", "r6")
  )
  p <- profile_data(
    d,
    c(x = "INTEGER", g = "TEXT"),
    columns = c(x = "coarsen", g = "full"),
    noise = list(epsilon = 2),
    k = 2L
  )
  # The gap from 20 to 39 merges into a wider bin.
  expect_lt(length(p$columns$x$n), 12L)
  expect_false(anyNA(p$columns$x$n))
  expect_type(p$columns$g$suppressed_n, "integer")
  expect_identical(p$provenance$noise, list(epsilon = 2))
  path <- withr::local_tempfile(fileext = ".yaml")
  write_profile(p, path)
  expect_identical(read_profile(path), p)
})

test_that("noise keeps every count a whole number of at least k", {
  set.seed(1)
  d <- data.frame(a = rep(letters[1:6], c(40, 30, 20, 10, 6, 3)))
  p <- profile_data(
    d,
    c(a = "TEXT"),
    columns = c(a = "full"),
    noise = list(epsilon = 0.5)
  )
  expect_type(p$columns$a$n, "integer")
  expect_true(all(p$columns$a$n >= 5L))
  expect_false(identical(p$columns$a$n, c(40L, 30L, 20L, 10L, 6L)))
})

test_that("profile_table() profiles a SQLite DBTable_v9", {
  dbconfig <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = dbconfig,
    table_name = "visits",
    field_types = c(id = "INTEGER", sex = "TEXT", day = "DATE"),
    keys = "id",
    validator_field_types = validator_field_types_blank,
    validator_field_contents = validator_field_contents_blank
  )
  withr::defer(tab$dbconnection$disconnect())
  tab$create_table()
  tab$insert_data(data.table::data.table(
    id = 1:12,
    sex = rep(c("f", "m"), 6),
    day = as.Date("2024-01-01") + rep(0:1, 6)
  ))
  p <- profile_table(
    tab,
    columns = c(sex = "full", day = "full", id = "exclude"),
    source = "visits"
  )
  expect_identical(
    p$provenance$query,
    as.character(dbplyr::sql_render(tab$tbl()))
  )
  expect_identical(p$provenance$driver, "SQLiteConnection")
  expect_identical(p$provenance$source, "visits")
  expect_identical(p$field_types, tab$field_types)
  expect_identical(p$keys, "id")
  expect_identical(p$columns$sex$n, c(6L, 6L))
  expect_identical(p$columns$day$days, as.Date(c("2024-01-01", "2024-01-02")))
})

test_that("profile_query() takes a DBConnection_v9 and infers field_types from a zero-row fetch", {
  db <- sqlite_connection(sqlite_dbconfig())
  withr::defer(db$disconnect())
  DBI::dbWriteTable(
    db$autoconnection,
    "t",
    data.frame(a = rep(c("x", "y"), 5), b = 1:10, c = seq(0.5, 5, by = 0.5))
  )
  sql <- "SELECT a, b, c FROM t WHERE b > 0"
  p <- profile_query(db, sql, columns = c(a = "full"))
  expect_identical(p$field_types, c(a = "TEXT", b = "INTEGER", c = "DOUBLE"))
  expect_identical(p$provenance$query, sql)
  expect_identical(p$provenance$format, 1L)
  expect_match(
    p$provenance$profiled_at,
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$"
  )
  expect_identical(
    names(p$provenance),
    c(
      "format",
      "source",
      "query",
      "driver",
      "profiled_at",
      "k",
      "noise",
      "max_levels",
      "csdb_version"
    )
  )
  expect_s3_class(p, "csdb_profile")
})

test_that("a query with fewer than k rows is an error", {
  d <- data.frame(a = c("x", "y", "z"))
  expect_error(profile_data(d, c(a = "TEXT")), "fewer than k rows")
})

# Item 1 of phase 7: one snapshot, and the refusal of a torn profile ----

# 100 rows that give every column kind that publishes counts. `note` holds 20
# missing values and `score` 10, so the totals include a missing count.
torn_data <- function() {
  i <- 0:99
  return(data.frame(
    sex = rep(c("f", "m"), 50),
    age = rep(c(10L, 20L), 50),
    note = ifelse(i < 20, NA, rep(c("a", "b"), 50)),
    score = ifelse(i < 10, NA, i / 3),
    on_day = as.Date("2024-01-01") + (i %% 5),
    in_week = as.Date("2024-01-01") + (i %% 14),
    in_month = as.Date("2024-01-01") + (i %% 60),
    in_quarter = as.Date("2024-01-01") + i * 9,
    id = rep(1:20, each = 5)
  ))
}

torn_field_types <- c(
  sex = "TEXT",
  age = "INTEGER",
  note = "TEXT",
  score = "DOUBLE",
  on_day = "DATE",
  in_week = "DATE",
  in_month = "DATE",
  in_quarter = "DATE",
  id = "INTEGER"
)

torn_columns <- c(
  sex = "full",
  age = "full",
  note = "full",
  score = "coarsen",
  on_day = "full",
  in_week = "coarsen",
  in_month = "full",
  in_quarter = "full",
  id = "exclude"
)

# A SQLite file that holds torn_data() as the table t.
torn_connection <- function(.local_envir = parent.frame()) {
  path <- withr::local_tempfile(
    fileext = ".sqlite",
    .local_envir = .local_envir
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), path, extended_types = TRUE)
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  DBI::dbWriteTable(con, "t", torn_data())
  return(list(con = con, path = path))
}

# Profiles the column `x` of t. With `torn = TRUE`, 10 rows are added to t
# through the same connection just after the first read, so every later read
# sees 110 rows and n_rows is 100. Returns the profile, or the error message.
torn_profile <- function(con, x, torn) {
  real <- csdb:::sql_missing_counts
  local_mocked_bindings(sql_missing_counts = function(con, sql, columns) {
    out <- real(con, sql, columns)
    if (torn) {
      DBI::dbExecute(
        con,
        "INSERT INTO t SELECT * FROM t ORDER BY rowid DESC LIMIT 10"
      )
    }
    return(out)
  })
  group_sizes <- NULL
  if (x == "id") {
    group_sizes <- list(rows_per_id = "id")
  }
  return(tryCatch(
    profile_query(
      con,
      "SELECT * FROM t",
      columns = torn_columns[x],
      field_types = torn_field_types[x],
      group_sizes = group_sizes
    ),
    error = function(e) conditionMessage(e)
  ))
}

# A field of a profile, or NULL when `p` is an error message. So a refused
# profile fails the assertions that read it, and does not error in them.
torn_field <- function(p, ...) {
  if (!inherits(p, "csdb_profile")) {
    return(NULL)
  }
  return(p[[c(...)]])
}

test_that("item 1: a consistent profile passes the torn-profile check, missing counts included", {
  db <- torn_connection()
  summaries <- c(
    sex = "levels",
    age = "levels",
    note = "levels",
    score = "breaks",
    on_day = "days",
    in_week = "weeks",
    in_month = "months",
    in_quarter = "quarters"
  )
  for (x in names(summaries)) {
    p <- torn_profile(db$con, x, torn = FALSE)
    expect_s3_class(p, "csdb_profile")
    expect_true(
      summaries[[x]] %in% names(torn_field(p, "columns", x)),
      label = x
    )
  }
  p <- torn_profile(db$con, "id", torn = FALSE)
  expect_s3_class(p, "csdb_profile")
  expect_identical(sum(torn_field(p, "group_sizes", "rows_per_id", "n")), 20L)
})

test_that("item 1: a torn profile is refused, naming the column and both totals", {
  db <- torn_connection()
  # The 10 added rows are the last 10 of t, so note and score hold a value
  # in each, and the cells gain 10 while the missing count stays.
  expected <- c(
    sex = "110 \\(110 in cells and 0 missing\\)",
    age = "110 \\(110 in cells and 0 missing\\)",
    note = "110 \\(90 in cells and 20 missing\\)",
    score = "110 \\(100 in cells and 10 missing\\)",
    on_day = "110 \\(110 in cells and 0 missing\\)",
    in_week = "110 \\(110 in cells and 0 missing\\)",
    in_month = "110 \\(110 in cells and 0 missing\\)",
    in_quarter = "110 \\(110 in cells and 0 missing\\)",
    id = "110 \\(110 in cells and 0 missing\\)"
  )
  for (x in names(expected)) {
    res <- torn_profile(db$con, x, torn = TRUE)
    expect_type(res, "character")
    expect_match(
      paste0(res, collapse = " "),
      paste0(
        "Torn profile: the counts of column ",
        x,
        " sum to ",
        expected[[x]],
        ", but n_rows is 100\\."
      ),
      label = x
    )
  }
  # The refusal rolled the transaction back, and the added rows with it.
  expect_identical(
    DBI::dbGetQuery(db$con, "SELECT COUNT(*) AS n FROM t")$n,
    100L
  )
})

test_that("item 1: a profile is read in one snapshot while another connection writes", {
  db <- torn_connection()
  DBI::dbGetQuery(db$con, "PRAGMA journal_mode = WAL")
  writer <- DBI::dbConnect(RSQLite::SQLite(), db$path)
  withr::defer(DBI::dbDisconnect(writer))
  real <- csdb:::sql_value_counts
  wrote <- FALSE
  local_mocked_bindings(sql_value_counts = function(con, sql, columns) {
    if (!wrote) {
      DBI::dbExecute(writer, "INSERT INTO t SELECT * FROM t LIMIT 10")
      wrote <<- TRUE
    }
    return(real(con, sql, columns))
  })
  p <- tryCatch(
    profile_query(
      db$con,
      "SELECT sex, age FROM t",
      columns = c(sex = "full", age = "full")
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(wrote)
  expect_identical(
    DBI::dbGetQuery(writer, "SELECT COUNT(*) AS n FROM t")$n,
    110L
  )
  expect_s3_class(p, "csdb_profile")
  expect_identical(torn_field(p, "n_rows"), 100L)
  expect_identical(sum(torn_field(p, "columns", "sex", "n")), 100L)
  expect_identical(sum(torn_field(p, "columns", "age", "n")), 100L)
})

test_that("item 1: the snapshot sends only transaction-control statements", {
  control <- "^SET TRANSACTION ISOLATION LEVEL [A-Z ,]+$"
  sqlite <- csdb:::profile_snapshot_statements("SQLite")
  expect_identical(lengths(sqlite), c(before = 0L, inside = 0L, after = 0L))
  pg <- csdb:::profile_snapshot_statements("PostgreSQL")
  expect_identical(
    pg$inside,
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY"
  )
  snap <- csdb:::profile_snapshot_statements(
    "Microsoft SQL Server",
    mssql_snapshot = TRUE,
    mssql_level = "READ COMMITTED"
  )
  expect_identical(snap$before, "SET TRANSACTION ISOLATION LEVEL SNAPSHOT")
  expect_identical(snap$after, "SET TRANSACTION ISOLATION LEVEL READ COMMITTED")
  rr <- csdb:::profile_snapshot_statements(
    "Microsoft SQL Server",
    mssql_snapshot = FALSE,
    mssql_level = "SERIALIZABLE"
  )
  expect_identical(rr$before, "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ")
  expect_identical(rr$after, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
  for (s in unlist(list(sqlite, pg, snap, rr))) {
    expect_match(s, control)
  }
  # A connection class with no known snapshot gets none, and a message.
  fake <- structure(list(), class = "UnknownConnection")
  expect_message(
    expect_null(csdb:::profile_snapshot_begin(fake)),
    "runs without a snapshot"
  )
  # SQLite profiles in a transaction and leaves none open.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "t", data.frame(a = rep(c("x", "y"), 5)))
  real <- csdb:::sql_missing_counts
  transacting <- NA
  local_mocked_bindings(sql_missing_counts = function(con, sql, columns) {
    transacting <<- RSQLite::sqliteIsTransacting(con)
    return(real(con, sql, columns))
  })
  profile_query(con, "SELECT * FROM t", columns = c(a = "full"))
  expect_true(transacting)
  expect_false(RSQLite::sqliteIsTransacting(con))
  # A transaction that the caller opened gets no snapshot, and stays open.
  DBI::dbBegin(con)
  expect_message(
    profile_query(con, "SELECT * FROM t", columns = c(a = "full")),
    "runs without a snapshot"
  )
  expect_true(RSQLite::sqliteIsTransacting(con))
  DBI::dbRollback(con)
})

# Item 2 of phase 7: one distinct value, and double breaks ----

test_that("item 2: a full numeric column of one value publishes levels, and breaks are double", {
  d <- data.frame(
    border = rep(2024L, 10),
    x = rep(c(1L, 9L), 5),
    few = c(rep(7, 3), rep(NA, 7))
  )
  ft <- c(border = "INTEGER", x = "INTEGER", few = "DOUBLE")
  p <- profile_data(
    d,
    ft,
    columns = c(border = "full", x = "coarsen", few = "full")
  )
  expect_identical(p$columns$border$levels, 2024L)
  expect_identical(p$columns$border$n, 10L)
  expect_null(p$columns$border$breaks)
  # 3 rows of one value are below k, so the one cell is removed.
  expect_identical(p$columns$few$levels, numeric(0))
  expect_identical(p$columns$few$n, integer(0))
  expect_null(p$columns$few$breaks)
  # The repro of phase 5: pretty(c(2024, 2024)) is integer.
  q <- profile_data(d, ft, columns = c(border = "coarsen", x = "coarsen"))
  expect_type(q$columns$border$breaks, "double")
  expect_type(q$columns$x$breaks, "double")
  for (profile in list(p, q)) {
    path <- withr::local_tempfile(fileext = ".yaml")
    write_profile(profile, path)
    expect_identical(read_profile(path), profile)
  }
})
