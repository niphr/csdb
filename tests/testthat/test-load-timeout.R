# A psql or bcp call that runs longer than `load_timeout` seconds is killed.
# The load then stops with class csdb_load_ambiguous after one attempt,
# because the server may or may not have committed the rows.
#
# The slow client is Rscript running Sys.sleep(30). It exists on every OS
# that runs the tests. run_load_tool() quotes every argument, so the "(" in
# the expression reaches Rscript unchanged.

timeout_rscript <- function() {
  return(file.path(R.home("bin"), "Rscript"))
}

timeout_sleep_args <- function() {
  return(c("-e", "Sys.sleep(30)"))
}

# Record every wait between attempts instead of sleeping, as
# local_recorded_waits() in test-load-tools.R does. A regression to retrying
# then shows in the record and costs no wait.
timeout_recorded_waits <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$waits <- numeric()
  local_mocked_bindings(
    load_retry_wait = function(seconds) {
      rec$waits <- c(rec$waits, seconds)
      return(invisible(NULL))
    },
    .env = .local_envir
  )
  return(rec)
}

test_that("run_load_tool() kills a client that runs longer than timeout", {
  elapsed <- system.time(
    res <- run_load_tool(timeout_rscript(), timeout_sleep_args(), timeout = 2)
  )[["elapsed"]]
  expect_identical(res$status, 124L)
  expect_lt(elapsed, 15)
})

test_that("run_checked_load() stops a timed-out client as ambiguous after one attempt", {
  rec <- timeout_recorded_waits()
  err <- tryCatch(
    suppressMessages(run_checked_load(
      command = timeout_rscript(),
      args = timeout_sleep_args(),
      tool = "Rscript",
      table = "tab",
      timeout = 2
    )),
    error = function(e) e
  )
  expect_s3_class(err, "csdb_load_ambiguous")
  expect_identical(err$attempts, 1L)
  expect_identical(rec$waits, numeric())
  expect_match(conditionMessage(err), "timed out after 2 s", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "If table tab has no primary key and you run the load again by hand, the table can get duplicate rows.",
    fixed = TRUE
  )
})

# The methods call Sys.which() before they run a client. Put an executable of
# each name first on PATH, as local_stand_in_tools() in test-load-tools.R
# does. It never runs, because run_load_tool() is mocked.
timeout_stand_in_tools <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  for (name in c("psql", "bcp")) {
    if (.Platform$OS.type == "windows") {
      file.copy(
        file.path(R.home("bin"), "Rscript.exe"),
        file.path(dir, paste0(name, ".exe"))
      )
    } else {
      writeLines(c("#!/bin/sh", "exit 99"), file.path(dir, name))
      Sys.chmod(file.path(dir, name), "0755")
    }
  }
  withr::local_envvar(
    PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  return(invisible(dir))
}

# Record the arguments of every client call, and answer each call as a load
# that stored both rows.
timeout_recorded_calls <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      rec$calls[[length(rec$calls) + 1L]] <- list(
        command = command,
        args = as.character(args),
        timeout = timeout,
        env = env
      )
      if (command == "psql") {
        return(list(status = 0L, output = "COPY 2"))
      }
      if (args[[2]] == "format") {
        return(list(status = 0L, output = character()))
      }
      return(list(status = 0L, output = "2 rows copied."))
    },
    .env = .local_envir
  )
  return(rec)
}

timeout_dbconfig <- function() {
  return(list(
    server = "fakehost",
    port = 5432,
    db = "fakedb",
    user = "fakeuser",
    password = "s3cret-Pw",
    trusted_connection = "no"
  ))
}

timeout_connection <- function(.local_envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  DBI::dbWriteTable(con, "tab", data.frame(id = integer(), x = character()))
  return(con)
}

# TRUE when "-l" is followed by "30" in `args`.
has_login_timeout <- function(args) {
  at <- which(args == "-l")
  return(length(at) == 1L && identical(args[at + 1L], "30"))
}

test_that("bcp format and bcp in pass a login timeout of 30 s", {
  timeout_stand_in_tools()
  rec <- timeout_recorded_calls()
  con <- timeout_connection()
  S7::method(load_data_infile, db_mssql)(
    connection = con,
    dbconfig = timeout_dbconfig(),
    table = "tab",
    dt = data.table::data.table(id = 1:2, x = c("a", "b")),
    file = tempfile(),
    load_timeout = 7
  )
  kinds <- vapply(rec$calls, function(x) x$args[[2]], character(1))
  expect_identical(kinds, c("format", "in"))
  for (call in rec$calls) {
    expect_true(has_login_timeout(call$args), label = call$args[[2]])
    expect_identical(call$timeout, 7)
  }
})

test_that("the psql URI holds connect_timeout=30 and no password", {
  timeout_stand_in_tools()
  rec <- timeout_recorded_calls()
  con <- timeout_connection()
  with_mocked_bindings(
    S7::method(load_data_infile, db_postgres)(
      connection = con,
      dbconfig = timeout_dbconfig(),
      table = DBI::Id(table = "tab"),
      dt = data.table::data.table(id = 1:2, x = c("a", "b")),
      file = tempfile(),
      load_timeout = 7
    ),
    dbQuoteIdentifier = function(conn, x, ...) {
      if (is.character(x)) {
        return(DBI::SQL(paste0("\"", x, "\"")))
      }
      return(DBI::SQL(paste0("\"", x@name, "\"", collapse = ".")))
    },
    .package = "DBI"
  )
  expect_length(rec$calls, 1L)
  args <- rec$calls[[1]]$args
  expect_identical(
    args[[length(args)]],
    "postgresql://fakeuser@fakehost:5432/fakedb?connect_timeout=30"
  )
  expect_false(any(grepl("s3cret-Pw", args, fixed = TRUE)))
  # psql reads the password from PGPASSWORD.
  expect_identical(rec$calls[[1]]$env, c(PGPASSWORD = "s3cret-Pw"))
  expect_identical(rec$calls[[1]]$timeout, 7)
})
