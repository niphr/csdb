# SyntheticTable: the contract checks, the fixed datasets and their md5.

st_field_types <- c(
  id = "INTEGER",
  sex = "TEXT",
  weight = "DOUBLE",
  alive = "BOOLEAN",
  born = "DATE",
  seen = "DATETIME"
)

st_valid <- function() {
  return(data.table::data.table(
    id = 1:3,
    sex = c("female", "male", NA),
    weight = c(3.5, 4.25, NA),
    alive = c(TRUE, FALSE, NA),
    born = as.Date(c("2024-01-02", "2024-02-03", NA)),
    seen = as.POSIXct(
      c("2024-01-02 03:04:05", "2024-02-03 00:00:00", NA),
      tz = "UTC"
    )
  ))
}

st_new <- function(dir = NULL, ...) {
  return(SyntheticTable$new(
    name = "people",
    field_types = st_field_types,
    keys = "id",
    dir = dir,
    ...
  ))
}

# The value of `expr` and its error message, so that a refused dataset shows
# as a failed expectation and not as an error in the test.
st_try <- function(expr) {
  return(tryCatch(
    list(value = expr, error = NULL),
    error = function(e) list(value = NULL, error = conditionMessage(e))
  ))
}

# The error message of `expr`, or "no error".
st_error <- function(expr) {
  error <- st_try(expr)$error
  if (is.null(error)) {
    return("no error")
  }
  return(error)
}

test_that("a valid data.table passes validate()", {
  d <- st_valid()
  expect_identical(st_new()$validate(d), d)
})

test_that("validate() names the columns check", {
  d <- st_valid()
  expect_error(
    st_new()$validate(d[, rev(names(d)), with = FALSE]),
    "columns check"
  )
  expect_error(st_new()$validate(d[, -"seen"]), "columns check")
  expect_error(st_new()$validate(as.data.frame(d)), "columns check")
})

test_that("validate() names the types check", {
  d <- st_valid()
  d[, id := as.numeric(id)]
  expect_error(st_new()$validate(d), "types check.*id \\(INTEGER\\)")
  d <- st_valid()
  d[, born := as.character(born)]
  expect_error(st_new()$validate(d), "types check.*born \\(DATE\\)")
})

test_that("duplicate keys are refused", {
  d <- st_valid()
  d[, id := c(1L, 1L, 2L)]
  expect_error(st_new()$validate(d), "keys check")
})

test_that("validate() names the validator_field_contents check", {
  bad <- function(data) {
    out <- FALSE
    attr(out, "var") <- "sex"
    return(out)
  }
  st <- st_new(validator_field_contents = bad)
  expect_error(st$validate(st_valid()), "validator_field_contents check.*sex")
})

test_that("validate() names a failing invariant and its message", {
  st <- st_new(
    invariants = list(
      ok = function(d) TRUE,
      heavy = function(d) {
        if (all(d$weight > 4, na.rm = TRUE)) TRUE else "too light"
      }
    )
  )
  expect_error(st$validate(st_valid()), "invariant 'heavy' check. too light")
})

test_that("a generator that breaks an invariant is refused", {
  st <- st_new(
    invariants = list(no_male = function(d) !any(d$sex %in% "male")),
    generator = function(profile, n, seed, world) st_valid()
  )
  expect_error(st$random(n = 3, seed = 1), "invariant 'no_male'")
})

test_that("random() with seed = NULL reports a seed that replays it", {
  st <- st_new()
  msg <- NULL
  d1 <- withCallingHandlers(
    st$random(n = 20),
    message = function(m) {
      msg <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    }
  )
  expect_match(msg, "drew seed [0-9]+")
  seed <- as.integer(sub(".*drew seed ([0-9]+).*", "\\1", msg))
  expect_identical(st$random(n = 20, seed = seed), d1)
  expect_true(data.table::is.data.table(d1))
  expect_identical(names(d1), names(st_field_types))
})

test_that("random() passes world to the generator and validates", {
  st <- st_new(invariants = list(f = function(d) all(d$sex == "f")))
  d <- st$random(n = 10, seed = 2, world = list(sex = "f"))
  expect_identical(unique(d$sex), "f")
})

test_that("a valid handmade dataset is returned unchanged", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "handmade"))
  d <- st_valid()
  data.table::fwrite(d, file.path(dir, "handmade", "three_people.csv"))
  st <- st_new(dir)
  res <- st_try(st$dataset("three_people"))
  expect_null(res$error)
  expect_identical(res$value, d)
  expect_identical(st$list_datasets()$kind, "handmade")
})

test_that("an invalid handmade dataset is refused by name", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "handmade"))
  d <- st_valid()
  d[, id := 1L]
  data.table::fwrite(d, file.path(dir, "handmade", "twins.csv"))
  expect_match(
    st_error(st_new(dir)$dataset("twins")),
    "dataset 'twins'.*keys check"
  )
})

test_that("add_dataset() then dataset() reproduces the data and its md5", {
  dir <- withr::local_tempdir()
  st <- st_new(dir)
  d <- st$add_dataset(
    "small",
    n = 25,
    seed = 3,
    description = "Twenty-five rows.",
    overrides = list(born = as.Date("2024-05-06"))
  )
  expect_identical(st$dataset("small"), d)
  expect_identical(unique(d$born), as.Date("2024-05-06"))
  entry <- yaml::read_yaml(file.path(dir, "datasets.yaml"))[[1]]
  expect_identical(entry$md5, csdb:::synthetic_hash(d, st_field_types))
  expect_identical(
    names(entry),
    c("name", "kind", "n", "seed", "description", "overrides", "md5")
  )
  raw <- readBin(file.path(dir, "datasets.yaml"), "raw", 1e5)
  expect_false(any(raw == as.raw(13)))
  expect_identical(
    st$list_datasets(),
    data.table::data.table(
      name = "small",
      kind = "recipe",
      n = 25L,
      seed = 3L,
      description = "Twenty-five rows."
    )
  )
  expect_identical(
    st$check_hashes(),
    data.table::data.table(name = "small", pass = TRUE)
  )
})

test_that("the md5 uses LF and ignores row order", {
  d <- st_valid()
  h <- csdb:::synthetic_hash(d, st_field_types)
  expect_identical(h, csdb:::synthetic_hash(d[3:1], st_field_types))
  path <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(d, path, eol = "\r\n")
  expect_false(identical(h, unname(tools::md5sum(path))))
})

test_that("a changed generator fails the hash check by name", {
  dir <- withr::local_tempdir()
  st_new(dir)$add_dataset("small", n = 25, seed = 3, description = "x")
  changed <- function(profile, n, seed, world) {
    d <- simulate_from_profile(profile, n, seed, world)
    d[, weight := weight + 1]
    return(d)
  }
  st <- st_new(dir, generator = changed)
  expect_error(st$dataset("small"), "dataset 'small'.*does not match the md5")
})

test_that("check_hashes() flags a changed generator by name", {
  dir <- withr::local_tempdir()
  st <- st_new(dir)
  st$add_dataset("small", n = 25, seed = 3, description = "x")
  st$add_dataset("other", n = 10, seed = 5, description = "y")
  changed <- function(profile, n, seed, world) {
    d <- simulate_from_profile(profile, n, seed, world)
    if (n == 25L) {
      d[, weight := weight + 1]
    }
    return(d)
  }
  expect_identical(
    st_new(dir, generator = changed)$check_hashes(),
    data.table::data.table(name = c("small", "other"), pass = c(FALSE, TRUE))
  )
})

test_that("add_dataset() refuses an existing name", {
  dir <- withr::local_tempdir()
  st <- st_new(dir)
  st$add_dataset("small", n = 5, seed = 1, description = "x")
  expect_error(
    st$add_dataset("small", n = 6, seed = 2, description = "y"),
    "already has a dataset named small"
  )
  dir.create(file.path(dir, "handmade"))
  data.table::fwrite(st_valid(), file.path(dir, "handmade", "hand.csv"))
  expect_error(
    st$add_dataset("hand", n = 6, seed = 2, description = "y"),
    "already has a dataset named hand"
  )
  expect_error(
    st$add_dataset("Bad-Name", n = 5, seed = 1, description = "x"),
    "lower snake case"
  )
  expect_error(
    st$add_dataset("default", n = 5, seed = 1, description = "x"),
    "n = 1000, seed = 1"
  )
})

test_that("world validates without the hash check and says so", {
  dir <- withr::local_tempdir()
  st_new(dir)$add_dataset("small", n = 25, seed = 3, description = "x")
  changed <- function(profile, n, seed, world) {
    d <- simulate_from_profile(profile, n, seed, world)
    d[, weight := weight + 1]
    return(d)
  }
  st <- st_new(
    dir,
    generator = changed,
    invariants = list(f = function(d) {
      if (all(d$sex == "f")) TRUE else "sex is not f"
    })
  )
  expect_message(
    d <- st$dataset("small", world = list(sex = "f")),
    "world is not NULL, so the hash is not checked"
  )
  expect_identical(unique(d$sex), "f")
  expect_error(
    suppressMessages(st$dataset("small", world = list(sex = "m"))),
    "invariant 'f' check. sex is not f"
  )
})

test_that("synthetic_table_from_dir() reads profile, recipes and handmade CSVs", {
  dir <- withr::local_tempdir()
  p <- profile_data(
    data.frame(sex = rep(c("female", "male"), 20), id = 1:40),
    c(sex = "TEXT", id = "INTEGER"),
    columns = c(sex = "full"),
    keys = "id"
  )
  write_profile(p, file.path(dir, "profile.yaml"))
  ft <- c(id = "INTEGER", sex = "TEXT", born = "DATE")
  ft_p <- c(sex = "TEXT", id = "INTEGER")
  st <- synthetic_table_from_dir(dir, "people", ft_p, keys = "id")
  st$add_dataset("small", n = 30, seed = 4, description = "x")
  st2 <- synthetic_table_from_dir(dir, "people", ft_p, keys = "id")
  d <- st2$dataset("small")
  expect_true(all(d$sex %in% c("female", "male")))
  expect_identical(st2$list_datasets()$name, "small")
})

test_that("synthetic_table_from_dir() reads handmade CSV columns as their field types", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "handmade"))
  path <- file.path(dir, "handmade", "two.csv")
  writeLines(c("id,sex,born", "1,female,2024-01-02", "2,,"), path)
  ft <- c(id = "INTEGER", sex = "TEXT", born = "DATE")
  st <- synthetic_table_from_dir(dir, "births", ft, keys = "id")
  # The reader that $dataset() uses, before validation.
  read <- csdb:::synthetic_read_csv(path, st$field_types)
  expect_s3_class(read$born, "Date")
  expect_type(read$id, "integer")
  # $dataset() returns the same typed columns, validated.
  res <- st_try(st$dataset("two"))
  expect_null(res$error)
  expect_s3_class(res$value$born, "Date")
  expect_identical(res$value$born, as.Date(c("2024-01-02", NA)))
  expect_identical(res$value$id, 1:2)
  expect_identical(res$value$sex, c("female", NA))
  expect_identical(st$list_datasets()$kind, "handmade")
})
