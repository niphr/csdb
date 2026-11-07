# profile_compare(): a known structure survives profile, simulate and profile
# again, and a simulation from another profile does not pass.

compare_settings <- list(
  field_types = c(
    sex = "TEXT",
    grp = "TEXT",
    region = "TEXT",
    age = "INTEGER",
    day = "DATE",
    score = "DOUBLE"
  ),
  columns = c(
    sex = "full",
    grp = "full",
    region = "full",
    age = "coarsen",
    day = "full",
    score = "coarsen"
  ),
  crosstabs = list(c("sex", "grp")),
  dependencies = list(c("grp", "region"))
)

compare_data <- function(seed, sex_prob, age_max) {
  withr::local_seed(seed)
  n <- 3000
  grp <- sample(letters[1:6], n, TRUE, prob = 6:1)
  d <- data.frame(
    sex = sample(c("f", "m"), n, TRUE, prob = c(sex_prob, 1 - sex_prob)),
    grp = grp,
    region = c(a = "r1", b = "r1", c = "r2", d = "r2", e = "r3", f = "r3")[grp],
    age = sample(0:age_max, n, TRUE),
    day = as.Date("2024-01-01") + sample(0:6, n, TRUE),
    score = stats::rnorm(n)
  )
  d$score[sample(n, 600)] <- NA
  return(d)
}

# Real and synthetic data are profiled at the same k, as the
# profile_compare() help page asks.
compare_profile <- function(d, k = 5L) {
  return(profile_data(
    d,
    compare_settings$field_types,
    columns = compare_settings$columns,
    crosstabs = compare_settings$crosstabs,
    dependencies = compare_settings$dependencies,
    k = k
  ))
}

compare_columns <- c("column", "property", "distance", "threshold", "pass")

test_that("a simulation from the same profile passes the comparison", {
  real <- compare_profile(compare_data(1, 0.5, 99))
  synth <- compare_profile(simulate_from_profile(real))
  result <- profile_compare(real, synth)
  expect_identical(names(result), compare_columns)
  expect_setequal(
    unique(result$property),
    c("missing_share", "levels", "bins", "days", "crosstab", "dependency")
  )
  expect_true(
    all(result$pass),
    label = paste(capture.output(print(result)), collapse = "
")
  )
})

test_that("a simulation from a different profile fails the comparison", {
  real <- compare_profile(compare_data(1, 0.5, 99))
  other_data <- compare_data(2, 0.85, 30)
  other_data$region <- sample(c("r1", "r2", "r3"), nrow(other_data), TRUE)
  other <- compare_profile(other_data)
  synth <- compare_profile(simulate_from_profile(other))
  result <- profile_compare(real, synth)
  failed <- result[result$pass == FALSE, ]
  expect_true(all(c("sex", "age", "grp,region") %in% failed$column))
  expect_identical(failed$property[failed$column == "grp,region"], "dependency")
})

test_that("profile_compare gives distance 1 to a summary that synth lacks", {
  d <- data.frame(a = rep(c("x", "y"), 10))
  real <- profile_data(d, c(a = "TEXT"), columns = c(a = "full"))
  synth <- profile_data(d, c(a = "TEXT"))
  result <- profile_compare(real, synth)
  expect_identical(result$property, c("missing_share", "levels"))
  expect_identical(result$distance, c(0, 1))
  expect_identical(result$threshold, c(0.05, 0.05))
  expect_identical(result$pass, c(TRUE, FALSE))
})

test_that("profile_compare skips a suppressed missing_share", {
  d <- data.frame(a = c(rep("x", 18), NA, NA))
  real <- profile_data(d, c(a = "TEXT"))
  expect_identical(real$columns$a$missing_share, "suppressed")
  result <- profile_compare(real, real)
  expect_identical(nrow(result), 0L)
  expect_identical(names(result), compare_columns)
})

test_that("the bootstrap threshold and the margin decide pass", {
  half <- profile_data(
    data.frame(v = rep(c("x", "y"), c(500, 500))),
    c(v = "TEXT"),
    columns = c(v = "full")
  )
  skewed <- profile_data(
    data.frame(v = rep(c("x", "y"), c(700, 300))),
    c(v = "TEXT"),
    columns = c(v = "full")
  )
  levels_row <- function(...) {
    r <- profile_compare(half, skewed, ...)
    return(r[r$property == "levels", ])
  }
  default <- levels_row()
  expect_equal(default$distance, 0.2)
  # 1000 draws from a 50/50 table rarely move a share by more than 0.03.
  expect_lt(default$threshold, 0.1)
  expect_gt(default$threshold, 0.05)
  expect_false(default$pass)
  wide <- levels_row(margin = 0.5)
  expect_equal(wide$threshold - default$threshold, 0.45)
  expect_true(wide$pass)
  # The same seed gives the same threshold.
  expect_identical(levels_row(seed = 3)$threshold, levels_row(seed = 3)$threshold)
})

test_that("profile_compare restores the caller's random state", {
  p <- profile_data(
    data.frame(v = rep(c("x", "y"), 10)),
    c(v = "TEXT"),
    columns = c(v = "full")
  )
  set.seed(99)
  before <- .Random.seed
  profile_compare(p, p)
  expect_identical(.Random.seed, before)
})


test_that("n_distinct is compared with what the sample can reach", {
  withr::local_seed(7)
  ft <- c(a = "TEXT")
  cols <- c(a = "full")
  nd_profile <- function(n_values) {
    d <- data.frame(a = sample(sprintf("v%05d", seq_len(n_values)), 20000, TRUE))
    return(profile_data(d, ft, columns = cols))
  }
  nd_row <- function(real, synth) {
    r <- profile_compare(real, synth)
    return(r[r$property == "n_distinct", ])
  }
  # About 8600 of 10000 values, far above n = 1000 rows.
  real <- nd_profile(10000)
  expect_gt(real$columns$a$n_distinct, 8000)
  synth <- profile_data(simulate_from_profile(real), ft, columns = cols)
  expect_lt(synth$columns$a$n_distinct, 1000)
  expect_true(nd_row(real, synth)$pass)
  # 1000 values at n = 1000: uniform draws reach about 632 of them.
  real_1000 <- nd_profile(1000)
  synth_1000 <- profile_data(simulate_from_profile(real_1000), ft, columns = cols)
  expect_true(nd_row(real_1000, synth_1000)$pass)
  # A simulation with 100 values reaches far fewer than it could.
  few <- profile_data(simulate_from_profile(nd_profile(100)), ft, columns = cols)
  expect_identical(few$columns$a$n_distinct, 100L)
  expect_false(nd_row(real, few)$pass)
  expect_gt(nd_row(real, few)$distance, 0.8)
})

# Long and wide tables, from 20000 real rows. Each element gives the data,
# field_types, columns and the joint structure to profile.
compare_shapes <- function() {
  withr::local_seed(42)
  n <- 20000
  day0 <- as.Date("2024-01-01")
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  ids <- function(sizes) rep(seq_along(sizes), sizes)[seq_len(n)]
  one <- function(x, type, sensitivity, ...) {
    return(list(
      d = data.frame(a = x),
      ft = c(a = type),
      cols = c(a = sensitivity),
      ...
    ))
  }
  pair <- function(levels) {
    return(list(
      d = data.frame(
        a = sample(paste0("a", seq_len(levels)), n, TRUE),
        b = sample(paste0("b", seq_len(levels)), n, TRUE)
      ),
      ft = c(a = "TEXT", b = "TEXT"),
      cols = c(a = "full", b = "full"),
      crosstabs = list(c("a", "b"))
    ))
  }
  levels_50 <- sprintf("l%02d", 1:50)
  return(list(
    text_50_uniform = one(sample(levels_50, n, TRUE), "TEXT", "full"),
    text_50_skewed = one(
      sample(levels_50, n, TRUE, prob = 1 / (1:50)),
      "TEXT",
      "full"
    ),
    text_1000_distinct = one(
      sample(sprintf("v%04d", 1:1000), n, TRUE),
      "TEXT",
      "full"
    ),
    date_60_days = one(day0 + sample(0:59, n, TRUE), "DATE", "full"),
    date_365_days = one(day0 + sample(0:364, n, TRUE), "DATE", "full"),
    datetime_1_year = one(
      t0 + sample(0:(365 * 86400), n, TRUE),
      "DATETIME",
      "full"
    ),
    weeks_1_year = one(day0 + sample(0:364, n, TRUE), "DATE", "coarsen"),
    weeks_5_years = one(day0 + sample(0:1825, n, TRUE), "DATE", "coarsen"),
    integer_0_to_99 = one(sample(0:99, n, TRUE), "INTEGER", "coarsen"),
    crosstab_10_x_10 = pair(10),
    crosstab_30_x_30 = pair(30),
    group_sizes_1_to_3 = one(
      ids(sample(1:3, n, TRUE)),
      "INTEGER",
      "exclude",
      group_sizes = list(g = "a")
    ),
    group_sizes_1_to_40 = one(
      ids(sample(1:40, n, TRUE)),
      "INTEGER",
      "exclude",
      group_sizes = list(g = "a")
    )
  ))
}

compare_shape_profile <- function(shape, d, k = 5L) {
  return(profile_data(
    d,
    shape$ft,
    columns = shape$cols,
    crosstabs = shape$crosstabs,
    group_sizes = shape$group_sizes,
    k = k
  ))
}

# The comparison of `real` with 1000 rows simulated from `from`, profiled at
# the k of `real`.
compare_shape_result <- function(shape, real, from, seed) {
  sim <- simulate_from_profile(from, n = 1000, seed = seed)
  synth <- compare_shape_profile(shape, sim, real$provenance$k)
  return(profile_compare(real, synth))
}

compare_shape_label <- function(name, seed, result) {
  rows <- capture.output(print(result))
  return(paste(c(paste(name, "seed", seed), rows), collapse = "\n"))
}

test_that("same-profile simulations pass at n = 1000 for long and wide tables", {
  shapes <- compare_shapes()
  expect_length(shapes, 13)
  for (name in names(shapes)) {
    shape <- shapes[[name]]
    real <- compare_shape_profile(shape, shape$d)
    for (seed in 1:5) {
      result <- compare_shape_result(shape, real, real, seed)
      tables <- result[result$property != "missing_share", ]
      expect_gt(nrow(tables), 0)
      expect_true(all(result$pass), label = compare_shape_label(name, seed, result))
    }
  }
})

test_that("a shifted simulation still fails", {
  withr::local_seed(43)
  n <- 20000
  day0 <- as.Date("2024-01-01")
  dates <- list(ft = c(a = "DATE"), cols = c(a = "full"))
  year <- data.frame(a = day0 + sample(0:364, n, TRUE))
  first_quarter <- data.frame(a = day0 + sample(0:90, n, TRUE))
  real <- compare_shape_profile(dates, year)
  shifted <- compare_shape_profile(dates, first_quarter)
  a <- sample(paste0("a", 1:10), n, TRUE)
  cross <- list(
    ft = c(a = "TEXT", b = "TEXT"),
    cols = c(a = "full", b = "full"),
    crosstabs = list(c("a", "b"))
  )
  # In the real data b follows a. In the broken data b is independent of a,
  # with the same marginal.
  joint <- data.frame(a = a, b = sub("a", "b", a))
  broken <- data.frame(a = a, b = sample(paste0("b", 1:10), n, TRUE))
  real_cross <- compare_shape_profile(cross, joint)
  broken_cross <- compare_shape_profile(cross, broken)
  for (seed in 1:5) {
    r <- compare_shape_result(dates, real, shifted, seed)
    date_row <- r[r$property != "missing_share", ]
    expect_identical(nrow(date_row), 1L)
    expect_false(date_row$pass, label = compare_shape_label("dates", seed, r))
    r <- compare_shape_result(cross, real_cross, broken_cross, seed)
    expect_false(r$pass[r$property == "crosstab"])
    expect_true(all(r$pass[r$property == "levels"]))
  }
})
