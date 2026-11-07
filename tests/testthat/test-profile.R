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
  d <- data.frame(day = as.Date("2024-03-01") + c(rep(0, 6), rep(1, 5), 2))
  p <- profile_data(d, c(day = "DATE"), columns = c(day = "full"))
  expect_identical(p$columns$day$days, as.Date(c("2024-03-01", "2024-03-02")))
  expect_identical(p$columns$day$n, c(6L, 5L))
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

test_that("INTEGER and DOUBLE bins merge small edge bins inward and mark small inner bins NA", {
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
  # A gap from 20 to 39 gives inner bins of 0 rows. The 40 distinct values
  # are above max_levels, so the full column is binned.
  y <- c(rep(0:19, each = 10), rep(40:59, each = 10), 0L, 59L)
  q <- profile_data(
    data.frame(y = y),
    c(y = "INTEGER"),
    columns = c(y = "full"),
    max_levels = 39L
  )
  expect_identical(q$columns$y$breaks, c(-5, seq(5, 60, by = 5)))
  expect_identical(
    q$columns$y$n,
    c(51L, 50L, 50L, 50L, NA, NA, NA, NA, 50L, 50L, 50L, 51L)
  )
  expect_false("suppressed_n" %in% names(q$columns$y))
  # A full column at most max_levels publishes its values instead.
  r <- profile_data(
    data.frame(z = rep(c(1.5, 2.5), 5)),
    c(z = "DOUBLE"),
    columns = c(z = "full")
  )
  expect_identical(r$columns$z$levels, c(1.5, 2.5))
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

test_that("group_sizes count the values of a column per row count, suppressed", {
  d <- data.frame(id = rep(1:8, c(2, 2, 2, 2, 2, 3, 3, 9)))
  p <- profile_data(
    d,
    c(id = "INTEGER"),
    columns = c(id = "exclude"),
    group_sizes = list(rows_per_id = "id")
  )
  expect_identical(
    p$group_sizes,
    list(rows_per_id = list(column = "id", size = 2L, n = 5L))
  )
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

test_that("a profile with noise, NA bins and suppressed_n survives the round trip", {
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
  expect_true(anyNA(p$columns$x$n))
  expect_type(p$columns$x$suppressed_n, "integer")
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
