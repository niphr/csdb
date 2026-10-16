# csdb_set_password_hook() registers a function that returns the password.
# csdb calls it for each new PostgreSQL connection, so a token that expires is
# fetched again each time. Other drivers keep the configured password.
#
# No test needs a database server. A mocked DBI::dbConnect() records its
# arguments and returns an in-memory SQLite connection. The hook values are
# fake: "tok-1", "tok-2" and "tok-3".

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
  # The SQLite stand-in has no lc_messages. A PostgreSQL connection reads it
  # once, and an unknown value would give a warning.
  local_mocked_bindings(
    json_load_lc_messages = function(connection) "C",
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

# (b) A load uses the open connection and does not call the hook ----

test_that("a PostgreSQL load and upsert do not call the hook", {
  hook <- local_password_hook(hook_tokens)
  local_mocked_bindings(
    json_load_execute = function(connection, sql, json = NULL) {
      if (is.null(json)) {
        return(0)
      }
      return(nrow(yyjsonr::read_json_str(json)))
    },
    add_index = function(...) {
      return(invisible(NULL))
    }
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
    .package = "DBI"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  rows <- data.table::data.table(a = 1:2, b = c("x", "y"))
  cfg <- list(
    server = "h",
    port = 5432L,
    db = "d",
    user = "u",
    password = "cfg-pw"
  )
  S7::method(load_data_infile, db_postgres)(
    connection = con,
    dbconfig = cfg,
    table = "tab",
    dt = rows
  )
  S7::method(upsert_load_data_infile, db_postgres)(
    connection = con,
    dbconfig = cfg,
    table = DBI::Id(schema = "public", table = "tab"),
    dt = rows,
    fields = c("a", "b"),
    keys = "a"
  )
  expect_identical(hook$calls, 0L)
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
