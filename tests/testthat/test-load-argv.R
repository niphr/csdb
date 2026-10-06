# psql and bcp MUST receive in their arguments exactly the elements that csdb
# built. system2() pastes the arguments into one command line, and on Linux a
# shell runs it. An unquoted space, quote, `;`, `*` or `$(...)` then splits an
# argument or runs a command. run_load_tool() quotes every element.
#
# psql reads its password from PGPASSWORD. run_load_tool() sets it for the
# client only, and restores the old value, or its absence, afterwards.
#
# The client is Rscript itself, so these tests run on every OS. It runs a
# script that saves its arguments and the value of PGPASSWORD to a file, and
# exits with the status it is given.

argv_hostile <- c(
  "\\copy \"s q\".\"t\"\"x\" (\"a b\") from 'it''s.csv' (FORMAT CSV, DELIMITER '\t')",
  "$(touch PWNED); \"'\\`",
  "`touch PWNED`",
  "; touch PWNED",
  "two words",
  "a;b",
  "ORDER(id ASC, x ASC)",
  "*"
)

argv_password <- "s3cret $(touch PWNED); '\"`"

# Write the recording script to a new temporary directory, and make it the
# working directory. A command that a shell runs by mistake then leaves its
# file there.
local_argv_recorder <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(dir, .local_envir = .local_envir)
  script <- file.path(dir, "record.R")
  writeLines(
    c(
      "a <- commandArgs(trailingOnly = TRUE)",
      "seen <- list(args = a[-(1:2)], pgpassword = Sys.getenv('PGPASSWORD', NA))",
      "saveRDS(seen, a[[1]])",
      "quit(status = as.integer(a[[2]]))"
    ),
    script
  )
  return(list(dir = dir, script = script, out = file.path(dir, "seen.rds")))
}

# Run the recording script through run_load_tool().
run_argv_recorder <- function(recorder, args, status = 0L, env = NULL) {
  return(run_load_tool(
    file.path(R.home("bin"), "Rscript"),
    c(recorder$script, recorder$out, as.character(status), args),
    env = env
  ))
}

test_that("run_load_tool() passes every argument to the client unchanged", {
  recorder <- local_argv_recorder()
  res <- run_argv_recorder(recorder, argv_hostile)
  expect_identical(res$status, 0L)
  seen <- readRDS(recorder$out)
  expect_identical(seen$args, argv_hostile)
  expect_false(file.exists(file.path(recorder$dir, "PWNED")))
})

test_that("run_load_tool() runs no shell command that an argument holds", {
  # Without quotes, a shell would parse these two elements as valid commands.
  recorder <- local_argv_recorder()
  args <- c("x; touch PWNED", "$(touch PWNED)")
  res <- run_argv_recorder(recorder, args)
  expect_false(file.exists(file.path(recorder$dir, "PWNED")))
  expect_identical(readRDS(recorder$out)$args, args)
})

test_that("run_load_tool() sets PGPASSWORD for the client and restores it", {
  recorder <- local_argv_recorder()
  withr::local_envvar(PGPASSWORD = "outer-value")
  res <- run_argv_recorder(
    recorder,
    "x",
    env = c(PGPASSWORD = argv_password)
  )
  expect_identical(res$status, 0L)
  seen <- readRDS(recorder$out)
  expect_identical(seen$pgpassword, argv_password)
  expect_identical(seen$args, "x")
  expect_identical(Sys.getenv("PGPASSWORD", NA), "outer-value")
  expect_false(file.exists(file.path(recorder$dir, "PWNED")))
})

test_that("run_load_tool() unsets PGPASSWORD afterwards when it was not set", {
  recorder <- local_argv_recorder()
  withr::local_envvar(PGPASSWORD = NA)
  res <- run_argv_recorder(
    recorder,
    "x",
    env = c(PGPASSWORD = argv_password)
  )
  expect_identical(res$status, 0L)
  expect_identical(readRDS(recorder$out)$pgpassword, argv_password)
  expect_identical(Sys.getenv("PGPASSWORD", NA), NA_character_)
})

test_that("run_load_tool() restores PGPASSWORD when the client fails", {
  recorder <- local_argv_recorder()
  withr::local_envvar(PGPASSWORD = "outer-value")
  res <- run_argv_recorder(
    recorder,
    argv_hostile,
    status = 3L,
    env = c(PGPASSWORD = argv_password)
  )
  expect_identical(res$status, 3L)
  seen <- readRDS(recorder$out)
  expect_identical(seen$pgpassword, argv_password)
  expect_identical(seen$args, argv_hostile)
  expect_identical(Sys.getenv("PGPASSWORD", NA), "outer-value")

  # A client that cannot start.
  res <- run_load_tool(
    file.path(recorder$dir, "no-such-client"),
    "x",
    env = c(PGPASSWORD = argv_password)
  )
  expect_false(identical(res$status, 0L))
  expect_identical(Sys.getenv("PGPASSWORD", NA), "outer-value")

  withr::local_envvar(PGPASSWORD = NA)
  res <- run_argv_recorder(
    recorder,
    "x",
    status = 3L,
    env = c(PGPASSWORD = argv_password)
  )
  expect_identical(res$status, 3L)
  expect_identical(Sys.getenv("PGPASSWORD", NA), NA_character_)
})
