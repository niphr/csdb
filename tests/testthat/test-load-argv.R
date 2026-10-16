# bcp MUST receive in its arguments exactly the elements that csdb built.
# system2() pastes the arguments into one command line, and on Linux a shell
# runs it. An unquoted space, quote, `;`, `*` or `$(...)` then splits an
# argument or runs a command. run_load_tool() quotes every element.
#
# The client is Rscript itself, so these tests run on every OS. It runs a
# script that saves its arguments to a file, and exits with the status it is
# given.

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
      "seen <- list(args = a[-(1:2)])",
      "saveRDS(seen, a[[1]])",
      "quit(status = as.integer(a[[2]]))"
    ),
    script
  )
  return(list(dir = dir, script = script, out = file.path(dir, "seen.rds")))
}

# Run the recording script through run_load_tool().
run_argv_recorder <- function(recorder, args, status = 0L) {
  return(run_load_tool(
    file.path(R.home("bin"), "Rscript"),
    c(recorder$script, recorder$out, as.character(status), args)
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
