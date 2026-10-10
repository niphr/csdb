# csdb_set_password_hook() registers a function that returns the password.
# csdb calls it for each new PostgreSQL connection and for each psql load, so
# a token that expires is fetched again each time. Other drivers keep the
# configured password.
#
# No test needs a database server. A mocked DBI::dbConnect() records its
# arguments and returns an in-memory SQLite connection. A mocked
# run_load_tool() records the environment that psql would get. The hook
# values are fake: "tok-1", "tok-2" and "tok-3".

hook_tokens <- c("tok-1", "tok-2", "tok-3")

# Register a hook that returns `values` in order, one per call, and repeats the
# last one. The returned environment counts the calls in `calls`.
local_password_hook <- function(values, .local_envir = parent.frame()) {
  state <- new.env()
  state$calls <- 0L
  previous <- csdb_set_password_hook(function() {
    state$calls <- state$calls + 1L
    return(values[[min(state$calls, length(values))]])
  })
  withr::defer(csdb_set_password_hook(previous), envir = .local_envir)
  withr::local_options(csdb.auth_hook = NULL, .local_envir = .local_envir)
  return(state)
}

local_no_password_hook <- function(.local_envir = parent.frame()) {
  withr::local_options(
    csdb.password_hook = NULL,
    csdb.auth_hook = NULL,
    .local_envir = .local_envir
  )
}

# Replace DBI::dbConnect(). Each call records its arguments after the driver.
# With fail = TRUE the call stops with a message that holds every argument, as
# the connection string that odbc builds would.
local_recorded_connect <- function(
  fail = FALSE,
  .local_envir = parent.frame()
) {
  rec <- new.env()
  rec$calls <- list()
  real_connect <- DBI::dbConnect
  local_mocked_bindings(
    dbConnect = function(drv, ...) {
      args <- list(...)
      rec$calls[[length(rec$calls) + 1L]] <- args
      if (fail) {
        stop(
          "connection refused for ",
          paste(names(args), args, sep = "=", collapse = ";"),
          call. = FALSE
        )
      }
      return(real_connect(RSQLite::SQLite(), ":memory:"))
    },
    .package = "DBI",
    .env = .local_envir
  )
  return(rec)
}

hook_test_connection <- function(
  driver = "PostgreSQL Unicode",
  db = "d",
  trusted_connection = NULL,
  sslmode = NULL
) {
  return(DBConnection_v9$new(
    driver = driver,
    server = "h",
    port = 5432L,
    db = db,
    user = "u",
    password = "cfg-pw",
    trusted_connection = trusted_connection,
    sslmode = sslmode
  ))
}

# Run `expr`. Return its error, or NULL, and every message it emitted.
collect_conditions <- function(expr) {
  msgs <- character()
  err <- withCallingHandlers(
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) {
        return(e)
      }
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  return(list(error = err, messages = msgs))
}

# (e2) The public API ----

test_that("csdb_set_password_hook() stores the hook that csdb_get_password_hook() returns", {
  withr::local_options(csdb.password_hook = NULL)
  expect_null(csdb_get_password_hook())
  hook <- function() {
    return("tok-1")
  }
  expect_null(csdb_set_password_hook(hook))
  expect_identical(csdb_get_password_hook(), hook)
  expect_identical(getOption("csdb.password_hook"), hook)
})

test_that("csdb_set_password_hook(NULL) clears the hook to NULL", {
  hook <- function() {
    return("tok-1")
  }
  withr::local_options(csdb.password_hook = hook)
  expect_identical(csdb_set_password_hook(NULL), hook)
  expect_null(csdb_get_password_hook())
  expect_null(getOption("csdb.password_hook"))
})

test_that("csdb_set_password_hook() rejects an argument that is not a function or NULL", {
  withr::local_options(csdb.password_hook = NULL)
  expect_error(csdb_set_password_hook("tok-1"), "must be a function or NULL")
  expect_null(csdb_get_password_hook())
})

# (e) The hook value ----

test_that("a hook value that is not one non-empty string is an error that does not show it", {
  bad <- list(
    NULL,
    character(0),
    c("tok-1", "tok-2"),
    NA_character_,
    "",
    1L,
    list("tok-1")
  )
  for (value in bad) {
    local({
      local_password_hook(list(value))
      err <- expect_error(password_hook_value(), class = "error")
      text <- conditionMessage(err)
      expect_match(text, "csdb password hook", fixed = TRUE)
      expect_match(text, "single non-empty string", fixed = TRUE)
      expect_false(grepl("tok-", text, fixed = TRUE))
    })
  }
})

test_that("a hook that fails is an error that names the hook", {
  local_password_hook(list())
  csdb_set_password_hook(function() {
    stop("no token cache", call. = FALSE)
  })
  expect_error(
    password_hook_value(),
    "csdb password hook (csdb_set_password_hook()) failed: no token cache",
    fixed = TRUE
  )
})

# (a) Each new PostgreSQL connection asks the hook again ----

test_that("each new PostgreSQL connection gets the current hook value", {
  hook <- local_password_hook(hook_tokens)
  rec <- local_recorded_connect()
  db <- hook_test_connection()
  db$connect()
  db$disconnect()
  db$connect()
  db$disconnect()
  # The odbc_connect_args() branch for sslmode = "require".
  db_ssl <- hook_test_connection(sslmode = "require")
  db_ssl$connect()
  db_ssl$disconnect()
  expect_length(rec$calls, 3L)
  expect_identical(rec$calls[[1]]$password, I("{tok-1}"))
  expect_identical(rec$calls[[2]]$password, I("{tok-2}"))
  expect_identical(rec$calls[[3]]$password, I("{tok-3}"))
  expect_identical(rec$calls[[3]]$sslmode, "require")
  expect_identical(hook$calls, 3L)
  # The settings keep the configured password.
  expect_identical(db$config$password, "cfg-pw")
})

# (f) The hook value never reaches another kind of server ----

test_that("a SQL Server or other connection with a password keeps the configured password when a hook is set", {
  hook <- local_password_hook(hook_tokens)
  rec <- local_recorded_connect()
  for (driver in c(
    "ODBC Driver 17 for SQL Server",
    "MySQL ODBC 8.0 Unicode Driver"
  )) {
    db <- hook_test_connection(driver = driver, db = NULL)
    db$connect()
    db$disconnect()
    expect_identical(
      rec$calls[[length(rec$calls)]],
      odbc_connect_args(db$config),
      label = driver
    )
  }
  expect_length(rec$calls, 2L)
  expect_identical(rec$calls[[1]]$pwd, I("{cfg-pw}"))
  expect_identical(rec$calls[[2]]$password, I("{cfg-pw}"))
  expect_identical(hook$calls, 0L)
})

test_that("SQLite and a trusted SQL Server connection do not call the hook", {
  hook <- local_password_hook(hook_tokens)
  rec <- local_recorded_connect()
  db <- hook_test_connection(
    driver = "ODBC Driver 17 for SQL Server",
    db = NULL,
    trusted_connection = "yes"
  )
  db$connect()
  db$disconnect()
  expect_identical(
    rec$calls[[1]],
    list(
      driver = "ODBC Driver 17 for SQL Server",
      server = "h",
      port = 5432L,
      trusted_connection = "yes"
    )
  )

  sqlite <- DBConnection_v9$new(
    driver = "SQLite",
    db = withr::local_tempfile(fileext = ".sqlite")
  )
  sqlite$connect()
  sqlite$disconnect()
  expect_length(rec$calls, 2L)
  expect_false("password" %in% names(rec$calls[[2]]))
  expect_identical(hook$calls, 0L)
})

# (b) psql gets the hook value ----

# The methods call Sys.which("psql") before they run the client. Put an
# executable of that name first on PATH, as test-load-timeout.R does. It never
# runs, because run_load_tool() is mocked.
hook_stand_in_psql <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  if (.Platform$OS.type == "windows") {
    file.copy(
      file.path(R.home("bin"), "Rscript.exe"),
      file.path(dir, "psql.exe")
    )
  } else {
    writeLines(c("#!/bin/sh", "exit 99"), file.path(dir, "psql"))
    Sys.chmod(file.path(dir, "psql"), "0755")
  }
  withr::local_envvar(
    PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  return(invisible(dir))
}

pg_hook_load <- S7::method(load_data_infile, db_postgres)
pg_hook_upsert <- S7::method(upsert_load_data_infile, db_postgres)

# Record the `env` of every psql call. With fail = TRUE, psql fails and prints
# the PGPASSWORD it received; otherwise it reports that it stored both rows.
# The upsert reaches the PostgreSQL load method, and the DBI calls that need a
# server do nothing.
local_recorded_psql <- function(fail = FALSE, .local_envir = parent.frame()) {
  hook_stand_in_psql(.local_envir = .local_envir)
  rec <- new.env()
  rec$env <- list()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      rec$env[[length(rec$env) + 1L]] <- env
      if (fail) {
        return(list(
          status = 2L,
          output = paste0(
            "psql: error: password \"",
            env[["PGPASSWORD"]],
            "\" rejected"
          )
        ))
      }
      return(list(status = 0L, output = "COPY 2"))
    },
    load_data_infile = function(connection, ...) {
      return(pg_hook_load(connection, ...))
    },
    add_index = function(...) {
      return(invisible(NULL))
    },
    .env = .local_envir
  )
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      return(0L)
    },
    dbListFields = function(conn, name, ...) {
      return(c("a", "b"))
    },
    dbRemoveTable = function(conn, name, ...) {
      return(TRUE)
    },
    .package = "DBI",
    .env = .local_envir
  )
  return(rec)
}

hook_sqlite_con <- function(.local_envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  return(con)
}

hook_dbconfig <- function() {
  return(list(
    server = "h",
    port = 5432L,
    db = "d",
    user = "u",
    password = "cfg-pw"
  ))
}

hook_rows <- function() {
  return(data.table::data.table(a = 1:2, b = c("x", "y")))
}

run_hook_load <- function(con) {
  return(pg_hook_load(
    connection = con,
    dbconfig = hook_dbconfig(),
    table = "tab",
    dt = hook_rows()
  ))
}

run_hook_upsert <- function(con) {
  return(pg_hook_upsert(
    connection = con,
    dbconfig = hook_dbconfig(),
    table = DBI::Id(schema = "public", table = "tab"),
    dt = hook_rows(),
    fields = c("a", "b"),
    keys = "a"
  ))
}

test_that("the PostgreSQL load and upsert give psql the current hook value as PGPASSWORD", {
  hook <- local_password_hook(hook_tokens)
  rec <- local_recorded_psql()
  con <- hook_sqlite_con()
  run_hook_load(con)
  run_hook_upsert(con)
  expect_identical(
    rec$env,
    list(c(PGPASSWORD = "tok-1"), c(PGPASSWORD = "tok-2"))
  )
  expect_identical(hook$calls, 2L)
})

test_that("the PostgreSQL load and upsert redact the hook value in their messages", {
  local_password_hook(hook_tokens)
  local_recorded_psql(fail = TRUE)
  con <- hook_sqlite_con()
  for (run in list(run_hook_load, run_hook_upsert)) {
    got <- collect_conditions(run(con))
    expect_s3_class(got$error, "csdb_load_error")
    text <- c(got$messages, conditionMessage(got$error))
    # psql printed the password, and the message shows *** in its place.
    expect_true(any(grepl("password \"***\" rejected", text, fixed = TRUE)))
    for (tok in hook_tokens) {
      expect_false(any(grepl(tok, text, fixed = TRUE)), label = tok)
    }
  }
})

test_that("a PostgreSQL load stops before psql when the hook value is not a string", {
  local_password_hook(list(NULL))
  rec <- local_recorded_psql()
  con <- hook_sqlite_con()
  expect_error(run_hook_load(con), "csdb password hook", fixed = TRUE)
  expect_length(rec$env, 0L)
})

# (c) A failed connection does not show the hook value ----

test_that("a failed connection does not show the hook value in any message", {
  hook <- local_password_hook(hook_tokens)
  rec <- local_recorded_connect(fail = TRUE)
  db <- hook_test_connection()
  got <- collect_conditions(db$connect(attempts = 2))
  expect_match(
    conditionMessage(got$error),
    "Failed to connect to database after 2 attempts"
  )
  expect_length(rec$calls, 2L)
  expect_identical(hook$calls, 2L)
  text <- c(got$messages, conditionMessage(got$error))
  # The driver error reached the messages, with *** in place of the password.
  expect_identical(
    sum(grepl("connection refused for ", text, fixed = TRUE)),
    2L
  )
  expect_identical(sum(grepl("password=***;", text, fixed = TRUE)), 2L)
  for (tok in hook_tokens) {
    expect_false(any(grepl(tok, text, fixed = TRUE)), label = tok)
  }
})

test_that("a failed connection redacts a hook value that holds a brace", {
  local_password_hook("tok}x")
  local_recorded_connect(fail = TRUE)
  db <- hook_test_connection()
  got <- collect_conditions(db$connect(attempts = 1))
  text <- c(got$messages, conditionMessage(got$error))
  expect_true(any(grepl("password=***;", text, fixed = TRUE)))
  expect_false(any(grepl("tok", text, fixed = TRUE)))
})

# (d) With no hook, nothing changes ----

test_that("with no hook, a connection gets the configured password as in 2026.10.7", {
  local_no_password_hook()
  rec <- local_recorded_connect()
  db <- hook_test_connection()
  db$connect()
  db$disconnect()
  expect_identical(
    rec$calls[[1]],
    list(
      driver = "PostgreSQL Unicode",
      server = "h",
      port = 5432L,
      uid = "u",
      password = I("{cfg-pw}"),
      database = "d"
    )
  )
  expect_identical(rec$calls[[1]], odbc_connect_args(db$config))
})

test_that("with no hook, a failed connection message is as in 2026.10.7", {
  local_no_password_hook()
  local_recorded_connect(fail = TRUE)
  db <- hook_test_connection()
  got <- collect_conditions(db$connect(attempts = 1))
  expect_match(
    got$messages[[1]],
    paste0(
      "Could not connect to database server 'h'\nOriginal error: ",
      "connection refused for driver=PostgreSQL Unicode;server=h;port=5432;",
      "uid=u;password={cfg-pw};database=d"
    ),
    fixed = TRUE
  )
})

test_that("with no hook, psql gets the configured password as in 2026.10.7", {
  local_no_password_hook()
  rec <- local_recorded_psql()
  con <- hook_sqlite_con()
  run_hook_load(con)
  run_hook_upsert(con)
  expect_identical(
    rec$env,
    list(c(PGPASSWORD = "cfg-pw"), c(PGPASSWORD = "cfg-pw"))
  )
})
