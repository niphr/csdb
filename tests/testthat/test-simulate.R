# simulate_from_profile() and profile_schema(): types, keys, seeds and the
# joint structure of a profile.

# One column of every field type at every sensitivity it accepts.
simulate_all_types <- function() {
  n <- 60
  i <- seq_len(n)
  days <- as.Date("2024-01-01") + (i %% 20)
  times <- as.POSIXct("2024-01-01", tz = "UTC") + i * 7919
  d <- data.frame(
    text_full = rep(c("a", "b", "c"), 20),
    text_schema = paste0("s", i),
    text_exclude = paste0("e", i),
    int_full = rep(1:4, 15),
    int_coarsen = i,
    int_schema = i,
    int_exclude = i,
    dbl_full = rep(c(0.5, 1.5), 30),
    dbl_coarsen = i / 7,
    dbl_schema = i / 7,
    dbl_exclude = i / 7,
    bool_full = rep(c(TRUE, FALSE), 30),
    bool_schema = rep(c(TRUE, FALSE), 30),
    bool_exclude = rep(c(TRUE, FALSE), 30),
    date_full = days,
    date_coarsen = days,
    date_schema = days,
    date_exclude = days,
    dt_full = times,
    dt_coarsen = times,
    dt_schema = times,
    dt_exclude = times
  )
  prefix <- c(
    text = "TEXT",
    int = "INTEGER",
    dbl = "DOUBLE",
    bool = "BOOLEAN",
    date = "DATE",
    dt = "DATETIME"
  )
  field_types <- prefix[sub("_.*", "", names(d))]
  names(field_types) <- names(d)
  sensitivity <- sub("^[^_]+_", "", names(d))
  sensitivity[sensitivity == "schema"] <- "schema_only"
  names(sensitivity) <- names(d)
  return(list(d = d, field_types = field_types, columns = sensitivity))
}

simulate_r_class <- c(
  TEXT = "character",
  INTEGER = "integer",
  DOUBLE = "numeric",
  BOOLEAN = "logical",
  DATE = "Date",
  DATETIME = "POSIXct"
)

test_that("simulated data has n rows with the columns and R types of field_types", {
  x <- simulate_all_types()
  p <- profile_data(x$d, x$field_types, columns = x$columns)
  expect_setequal(
    unique(x$columns),
    c("full", "coarsen", "schema_only", "exclude")
  )
  for (n in c(1L, 7L, 1000L)) {
    sim <- simulate_from_profile(p, n = n)
    expect_s3_class(sim, "data.table")
    expect_identical(nrow(sim), n)
    expect_identical(names(sim), names(x$field_types))
    for (column in names(x$field_types)) {
      expect_true(
        inherits(sim[[column]], simulate_r_class[[x$field_types[[column]]]]),
        label = column
      )
    }
    for (column in names(x$field_types)[x$field_types == "DATETIME"]) {
      expect_identical(attr(sim[[column]], "tzone"), "UTC")
    }
    expect_identical(typeof(sim$date_full), "double")
  }
})

test_that("profile_schema gives schema_only columns with missing_share 0", {
  p <- profile_schema(c(id = "INTEGER", sex = "TEXT"), keys = "id")
  expect_s3_class(p, "csdb_profile")
  expect_identical(p$keys, "id")
  expect_identical(
    p$columns$sex,
    list(type = "TEXT", sensitivity = "schema_only", missing_share = 0)
  )
  expect_error(profile_schema(c(id = "NUMBER")), "Unknown field type")
})

test_that("schema-only columns follow the schema generation rules", {
  ft <- c(
    tx = "TEXT",
    it = "INTEGER",
    db = "DOUBLE",
    bl = "BOOLEAN",
    dy = "DATE",
    tm = "DATETIME"
  )
  sim <- simulate_from_profile(profile_schema(ft), n = 1000)
  expect_true(all(sim$tx %in% paste0("tx_", 1:10)))
  expect_true(all(sim$it %in% 0:100))
  expect_true(all(sim$db >= 0 & sim$db <= 1))
  expect_setequal(sim$bl, c(TRUE, FALSE))
  span <- as.Date(c("2020-01-01", "2024-12-31"))
  expect_true(all(sim$dy >= span[1] & sim$dy <= span[2]))
  expect_true(all(as.Date(sim$tm, tz = "UTC") >= span[1]))
  expect_true(all(as.Date(sim$tm, tz = "UTC") <= span[2]))
})

test_that("simulated rows are unique on keys", {
  # Single schema-only keys at n = 1000, TEXT and INTEGER.
  p1 <- profile_schema(c(id = "TEXT", v = "INTEGER"), keys = "id")
  s1 <- simulate_from_profile(p1, n = 1000)
  expect_identical(s1$id, paste0("id_", 1:1000))
  p2 <- profile_schema(c(id = "INTEGER", v = "TEXT"), keys = "id")
  s2 <- simulate_from_profile(p2, n = 1000)
  expect_identical(s2$id, 1:1000)
  # A composite key of two full columns with 50 levels each: 2500 pairs, so
  # 1000 independent draws repeat a pair about 180 times before redraws.
  grid <- expand.grid(a = sprintf("a%02d", 1:50), b = sprintf("b%02d", 1:50))
  p3 <- profile_data(
    grid,
    c(a = "TEXT", b = "TEXT"),
    columns = c(a = "full", b = "full"),
    keys = c("a", "b"),
    k = 1L
  )
  s3 <- simulate_from_profile(p3, n = 1000)
  expect_identical(nrow(s3), 1000L)
  expect_false(anyDuplicated(s3, by = c("a", "b")) > 0)
  # A composite key with a schema-only part is unique at n = 1000.
  p4 <- profile_schema(c(day = "DATE", id = "INTEGER"), keys = c("day", "id"))
  s4 <- simulate_from_profile(p4, n = 1000)
  expect_false(anyDuplicated(s4, by = c("day", "id")) > 0)
})

test_that("a key that cannot be unique stops with an error naming keys and n", {
  d <- data.frame(a = rep(c("x", "y"), 10))
  p <- profile_data(d, c(a = "TEXT"), columns = c(a = "full"), keys = "a")
  expect_error(
    simulate_from_profile(p, n = 10),
    "could not draw 10 rows unique on the keys a in 20 redraws"
  )
})

test_that("the same profile and seed give identical data", {
  x <- simulate_all_types()
  p <- profile_data(x$d, x$field_types, columns = x$columns)
  a <- simulate_from_profile(p, n = 200, seed = 3)
  b <- simulate_from_profile(p, n = 200, seed = 3)
  expect_identical(a, b)
  expect_false(identical(a, simulate_from_profile(p, n = 200, seed = 4)))
})

test_that("simulate_from_profile restores the caller's random state", {
  p <- profile_schema(c(v = "DOUBLE"))
  set.seed(99)
  before <- .Random.seed
  simulate_from_profile(p, n = 10, seed = 5)
  expect_identical(.Random.seed, before)
  old <- .Random.seed
  rm(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  simulate_from_profile(p, n = 10, seed = 5)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("a dependency that holds in the profile holds in the simulation", {
  grp <- rep(letters[1:10], each = 10)
  region <- rep(c("r1", "r2", "r3", "r4", "r5"), each = 20)
  d <- data.frame(grp = grp, region = region)
  ft <- c(grp = "TEXT", region = "TEXT")
  cols <- c(grp = "full", region = "full")
  p <- profile_data(
    d,
    ft,
    columns = cols,
    dependencies = list(c("grp", "region"))
  )
  expect_true(p$dependencies[[1]]$holds)
  sim <- simulate_from_profile(p, n = 1000)
  expect_identical(
    data.table::uniqueN(sim, by = "grp"),
    data.table::uniqueN(sim, by = c("grp", "region"))
  )
  s <- profile_data(
    sim,
    ft,
    columns = cols,
    dependencies = list(c("grp", "region"))
  )
  expect_true(s$dependencies[[1]]$holds)
})

test_that("a crosstab draws its two columns jointly", {
  # sex and age_group go together: f with young, m with old.
  d <- data.frame(
    sex = rep(c("f", "m"), each = 20),
    age_group = rep(c("young", "old"), each = 20)
  )
  p <- profile_data(
    d,
    c(sex = "TEXT", age_group = "TEXT"),
    columns = c(sex = "full", age_group = "full"),
    crosstabs = list(c("sex", "age_group"))
  )
  sim <- simulate_from_profile(p, n = 500)
  pairs <- unique(paste(sim$sex, sim$age_group))
  expect_setequal(pairs, c("f young", "m old"))
})

test_that("group sizes give fresh ids with the published row counts", {
  # Row counts 2, 3, 5, 8 and 9 publish the ranges 1 to 4, 5 to 7, and 8 up.
  d <- data.frame(id = rep(1:17, rep(c(2, 3, 5, 8, 9), c(3, 3, 5, 2, 4))))
  p <- profile_data(
    d,
    c(id = "INTEGER"),
    columns = c(id = "exclude"),
    group_sizes = list(rows_per_id = "id")
  )
  g <- p$group_sizes$rows_per_id
  expect_identical(g$size_from, c(1L, 5L, 8L))
  expect_identical(g$size_to, c(4L, 7L))
  sim <- simulate_from_profile(p, n = 1000)
  sizes <- table(sim$id)
  # The last id can be truncated. The open range 8 up draws 8 to 16.
  expect_true(all(sizes[-length(sizes)] %in% 1:16))
  # Each range draws uniformly within itself, gaps included.
  expect_true(all(c(4L, 6L, 7L) %in% sizes))
  expect_true(any(sizes[-length(sizes)] > 9))
  expect_identical(sort(unique(sim$id)), seq_len(length(sizes)))
})

test_that("summaries generate values within their published support", {
  d <- data.frame(
    lv = rep(c("x", "y", "z"), c(30, 20, 10)),
    age = rep(c(3L, 18L, 44L, 61L, 87L, 99L), 10),
    day = as.Date("2024-01-01") + rep(c(0, 1, 2), 20),
    wk = as.Date("2024-01-03") + rep(c(0, 7, 14), 20),
    many = sprintf("v%02d", rep(1:60, 1))
  )
  d$miss <- ifelse(seq_len(60) %% 4 == 0, NA, 1.5)
  ft <- c(
    lv = "TEXT",
    age = "INTEGER",
    day = "DATE",
    wk = "DATE",
    many = "TEXT",
    miss = "DOUBLE"
  )
  cols <- c(
    lv = "full",
    age = "coarsen",
    day = "full",
    wk = "coarsen",
    many = "full",
    miss = "full"
  )
  p <- profile_data(d, ft, columns = cols, k = 1L)
  sim <- simulate_from_profile(p, n = 2000)
  expect_setequal(sim$lv, c("x", "y", "z"))
  expect_equal(mean(sim$lv == "x"), 0.5, tolerance = 0.05)
  b <- p$columns$age$breaks
  expect_true(all(sim$age >= b[1] & sim$age <= b[length(b)]))
  expect_setequal(sim$day, as.Date("2024-01-01") + 0:2)
  mondays <- p$columns$wk$weeks
  expect_true(all(as.numeric(sim$wk - mondays[1]) %in% 0:20))
  expect_true(all(sim$many %in% paste0("many_", 1:p$columns$many$n_distinct)))
  expect_equal(mean(is.na(sim$miss)), 0.25, tolerance = 0.05)
})

test_that("world replaces the support of the named columns only", {
  d <- data.frame(
    sex = rep(c("f", "m"), 30),
    county = rep(c("c1", "c2", "c3"), c(30, 20, 10))
  )
  ft <- c(sex = "TEXT", county = "TEXT")
  p <- profile_data(d, ft, columns = c(sex = "full", county = "full"))
  sim <- simulate_from_profile(
    p,
    n = 3000,
    world = list(county = c("c1", "c9"))
  )
  expect_setequal(sim$county, c("c1", "c9"))
  expect_setequal(sim$sex, c("f", "m"))
  # c1 keeps its published weight of 30; c9 gets the mean of the shared
  # weights, which is also 30.
  expect_equal(mean(sim$county == "c1"), 0.5, tolerance = 0.05)
  expect_error(
    simulate_from_profile(p, world = list(nope = "x")),
    "world names unknown column"
  )
})

test_that("months, quarters and size ranges generate values within their cells", {
  rows <- function(...) {
    x <- c(...)
    return(rep(as.Date(names(x)), x))
  }
  sizes <- c(rep(1:15, each = 2), rep(20L, 8))
  d <- data.frame(
    id = rep(seq_along(sizes), sizes),
    mo = rows("2024-01-01" = 98, "2024-01-31" = 2, "2024-02-15" = 100,
      "2024-03-15" = 100, "2024-04-15" = 100),
    runs = rows("2024-01-01" = 300, "2024-07-01" = 3, "2025-01-01" = 97)
  )
  p <- profile_data(
    d,
    c(id = "INTEGER", mo = "DATE", runs = "DATE"),
    columns = c(id = "exclude", mo = "full", runs = "full"),
    group_sizes = list(g = "id")
  )
  expect_false(is.null(p$columns$mo$months))
  expect_false(is.null(p$columns$runs$quarters_to))
  sim <- simulate_from_profile(p, n = 4000)
  # A month draws every day of the month, not only its first day.
  expect_true(all(format(sim$mo, "%Y-%m") %in% c("2024-01", "2024-02",
    "2024-03", "2024-04")))
  expect_gt(length(unique(sim$mo)), 100)
  # The second run covers 2024-07-01 to 2025-03-31.
  runs <- p$columns$runs
  expect_identical(runs$quarters_to[2], as.Date("2025-01-01"))
  in_first <- sim$runs >= as.Date("2024-01-01") & sim$runs <= as.Date("2024-03-31")
  in_second <- sim$runs >= as.Date("2024-07-01") & sim$runs <= as.Date("2025-03-31")
  expect_true(all(in_first | in_second))
  expect_true(any(sim$runs >= as.Date("2024-10-01") & in_second))
  # Every group but the last, which can be truncated, has a size within a
  # published range.
  g <- p$group_sizes$g
  counts <- as.integer(table(sim$id))
  counts <- counts[-length(counts)]
  # The last range has no size_to, and draws up to twice its size_from.
  to <- c(g$size_to, 2L * g$size_from[length(g$size_from)])
  in_range <- vapply(counts, function(x) any(x >= g$size_from & x <= to), logical(1))
  expect_true(all(in_range))
  expect_gt(length(unique(counts)), length(g$n))
})

test_that("a nonnegative column is never simulated below 0", {
  withr::local_seed(11)
  n <- 2000
  d <- data.frame(
    count = rep(0:4, length.out = n),
    skewed = stats::rgeom(n, 0.05),
    seconds = stats::rexp(n, 1 / 60),
    unlisted = rep(0:4, length.out = n),
    signed = stats::rnorm(n)
  )
  ft <- c(
    count = "INTEGER",
    skewed = "INTEGER",
    seconds = "DOUBLE",
    unlisted = "INTEGER",
    signed = "DOUBLE"
  )
  p <- profile_data(
    d,
    ft,
    columns = c(count = "coarsen", skewed = "coarsen", seconds = "coarsen",
      signed = "coarsen")
  )
  for (x in c("count", "skewed", "seconds", "unlisted")) {
    expect_true(p$columns[[x]]$nonnegative, label = x)
  }
  expect_false(p$columns$signed$nonnegative)
  # The first bin of skewed starts below 0, so a uniform draw within it
  # would reach negative values.
  expect_lt(p$columns$skewed$breaks[1], 0)
  for (seed in 1:5) {
    sim <- simulate_from_profile(p, n = 1000, seed = seed)
    for (x in c("count", "skewed", "seconds", "unlisted")) {
      expect_gte(min(sim[[x]]), 0, label = paste(x, "seed", seed))
    }
    expect_true(any(sim$skewed < 5))
    expect_lt(min(sim$signed), 0)
  }
})
