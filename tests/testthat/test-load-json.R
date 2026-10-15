# The JSON load of PostgreSQL, pg_load = "json". csdb sends the rows as JSON
# text over the connection that it holds, and starts no psql.
#
# SQLite stands in for PostgreSQL here. json_load_execute() is mocked: it
# records each statement, and for an INSERT it reads the JSON text back with
# yyjsonr and appends the rows to the SQLite table. BEGIN, COMMIT and
# ROLLBACK are the real ones of the SQLite connection, so a rollback really
# removes the rows. run_load_tool() is mocked to fail, so a test fails when
# csdb starts psql.
#
# The server texts below are the error messages that odbc 1.7.0 and psqlODBC
# 13.02 gave against PostgreSQL 18.6 on 2026-10-15.

json_text_duplicate <- paste(
  "nanodbc/nanodbc.cpp:1867: 00000",
  "ERROR: duplicate key value violates unique constraint \"k_pkey\"",
  "DETAIL: Key (id)=(1) already exists.;",
  "Error while executing the query",
  sep = "\n"
)
json_text_type <- paste(
  "nanodbc/nanodbc.cpp:1867: 00000",
  "ERROR: invalid input syntax for type integer: \"x\";",
  "Error while executing the query",
  sep = "\n"
)
json_text_lock <- paste(
  "nanodbc/nanodbc.cpp:1867: 00000",
  "ERROR: canceling statement due to lock timeout;",
  "Error while preparing parameters",
  sep = "\n"
)
json_text_statement_timeout <- paste(
  "! ODBC failed with error 00000 from .",
  "x ERROR: canceling statement due to statement timeout;",
  sep = "\n"
)
json_text_lost <- paste(
  "nanodbc/nanodbc.cpp:1867: 00000",
  "FATAL:  terminating connection due to administrator command",
  "server closed the connection unexpectedly",
  "\tThis probably means the server terminated abnormally",
  "\tbefore or while processing the request.",
  ";",
  "The connection has been lost",
  sep = "\n"
)
json_text_lost_after <- paste(
  "nanodbc/nanodbc.cpp:1591: 00000",
  "SQLCancel unable due to the connection lost",
  sep = "\n"
)
json_text_commit <- paste(
  "nanodbc/nanodbc.cpp:1405: 00000",
  "ERROR: late failure at commit",
  sep = "\n"
)

# The duplicate-key text of a server with lc_messages = 'de_DE.UTF-8', from
# the German message catalogue of PostgreSQL. Not measured through odbc.
json_text_german <- paste(
  "nanodbc/nanodbc.cpp:1867: 00000",
  "FEHLER: doppelter Schl\u00fcsselwert verletzt Unique-Constraint \u00bbk_pkey\u00ab",
  sep = "\n"
)

json_secret <- "json-Pw-SENTINEL"

# Two SQLite tables t1 and t2 on one connection, created before the test.
json_tables <- function(pg_load = "json", .local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  conn <- sqlite_connection(cfg)
  withr::defer(conn$disconnect(), envir = .local_envir)
  tables <- lapply(c("t1", "t2"), function(name) {
    tab <- DBTable_v9$new(
      dbconfig = cfg,
      table_name = name,
      field_types = c(id = "INTEGER", x = "TEXT"),
      keys = "id",
      dbconnection = conn
    )
    suppressMessages(tab$connect())
    return(tab)
  })
  conn$config$pg_load <- pg_load
  conn$config$password <- json_secret
  return(tables)
}

json_data <- function() {
  return(list(
    data.table::data.table(id = 1:3, x = c("a1", "a2", "a3")),
    data.table::data.table(id = 1:2, x = c("b1", "b2"))
  ))
}

json_rows_of <- function(tab) {
  d <- DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    paste0("SELECT id, x FROM ", tab$table_name, " ORDER BY id")
  )
  return(paste(d$id, d$x))
}

# The mocked server. `fail` is a function(table, n) that returns NULL, an
# error text to raise, or a number of stored rows to report: `table` is the
# bare table name and `n` counts the INSERT statements of that table. The
# returned environment holds every statement in `sql`.
local_json_server <- function(fail = NULL, .local_envir = parent.frame()) {
  rec <- new.env()
  rec$sql <- character()
  rec$counts <- list()
  rec$psql <- 0L
  local_mocked_bindings(
    json_load_execute = function(connection, sql, json = NULL) {
      rec$sql <- c(rec$sql, sql)
      if (is.null(json)) {
        return(0)
      }
      name <- gsub("[`\"]", "", sub("^INSERT INTO (\\S+) .*$", "\\1", sql))
      rec$counts[[name]] <- (if (is.null(rec$counts[[name]])) {
        0L
      } else {
        rec$counts[[name]]
      }) +
        1L
      rows <- yyjsonr::read_json_str(json)
      DBI::dbAppendTable(connection, name, rows)
      outcome <- if (is.null(fail)) NULL else fail(name, rec$counts[[name]])
      if (is.character(outcome)) {
        stop(outcome, call. = FALSE)
      }
      if (is.numeric(outcome)) {
        return(outcome)
      }
      return(nrow(rows))
    },
    run_load_tool = function(...) {
      rec$psql <- rec$psql + 1L
      stop("csdb started psql", call. = FALSE)
    },
    .env = .local_envir
  )
  return(rec)
}

local_json_waits <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$waits <- numeric()
  local_mocked_bindings(
    retry_wait = function(seconds) {
      rec$waits <- c(rec$waits, seconds)
      return(invisible(NULL))
    },
    .env = .local_envir
  )
  return(rec)
}

json_conditions <- function(expr) {
  messages <- character()
  error <- withCallingHandlers(
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) e
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  return(list(
    error = error,
    message = if (is.null(error)) NA_character_ else conditionMessage(error),
    messages = messages
  ))
}

# json_load_tables() on the tables and data of the fixtures above.
json_load_fixture <- function(tables, data, ...) {
  conn <- tables[[1]]$dbconnection
  return(json_load_tables(
    connection = conn$autoconnection,
    dbconfig = conn$config,
    tables = lapply(tables, function(x) {
      return(x$table_name_short_for_mssql_fully_specified_for_postgres)
    }),
    dts = data,
    ...
  ))
}

insert_statements <- function(rec) {
  return(grep("^INSERT INTO ", rec$sql, value = TRUE))
}

test_that("DBConnection_v9 keeps pg_load, \"psql\" by default, and refuses any other value", {
  expect_identical(
    DBConnection_v9$new(driver = "SQLite")$config$pg_load,
    "psql"
  )
  expect_identical(
    DBConnection_v9$new(driver = "SQLite", pg_load = NULL)$config$pg_load,
    "psql"
  )
  expect_identical(
    DBConnection_v9$new(driver = "SQLite", pg_load = "json")$config$pg_load,
    "json"
  )
  for (bad in list("JSON", "copy", c("psql", "json"), 1, NA_character_)) {
    expect_error(
      DBConnection_v9$new(driver = "SQLite", pg_load = bad),
      'pg_load MUST be "psql" or "json".',
      fixed = TRUE
    )
  }
})

test_that("DBTable_v9 gives pg_load of its dbconfig to the connection that it creates", {
  cfg <- sqlite_dbconfig()
  cfg$pg_load <- "json"
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "t1",
    field_types = c(id = "INTEGER"),
    keys = "id"
  )
  expect_identical(tab$dbconnection$config$pg_load, "json")
  cfg$pg_load <- NULL
  tab2 <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "t1",
    field_types = c(id = "INTEGER"),
    keys = "id"
  )
  expect_identical(tab2$dbconnection$config$pg_load, "psql")
})

test_that("the JSON text keeps an empty string, quotes, tabs, line breaks and unicode, and NA is null", {
  s <- c(
    "",
    "a\"b",
    "\"wrapped\"",
    "a\tb",
    "line1\nline2",
    "a\rb",
    "a\\b",
    "\\.",
    "æøå €",
    NA
  )
  dt <- data.table::data.table(s = s)
  json <- json_load_text(data.table::copy(dt))
  expect_match(json, '^\\[\\{"s":""\\},', perl = TRUE)
  expect_match(json, '\\{"s":null\\}\\]$', perl = TRUE)
  back <- yyjsonr::read_json_str(json)
  expect_identical(back$s, s)
})

test_that("the JSON text writes a logical as 1 and 0, a whole double as an integer, and Inf and NaN as null", {
  dt <- data.table::data.table(
    b = c(TRUE, FALSE, NA),
    w = c(5, 1e5, NA),
    wi = c(5, Inf, NA),
    big = c(3e9, -1, NA),
    d = c(0.5, Inf, NaN),
    i = c(1L, NA, -2147483647L),
    p = as.POSIXct(c("2020-01-06 12:00:00", "2020-01-06 12:00:00", NA), tz = "UTC") + c(0, Inf, 0)
  )
  expect_identical(
    json_load_text(dt),
    paste0(
      '[{"b":1,"w":5,"wi":5,"big":"3000000000","d":0.5,"i":1,"p":"2020-01-06 12:00:00"},',
      '{"b":0,"w":100000,"wi":null,"big":"-1","d":null,"i":null,"p":null},',
      '{"b":null,"w":null,"wi":null,"big":null,"d":null,"i":-2147483647,"p":null}]'
    )
  )
})

test_that("the JSON text writes the shortest text that reads back as the same double", {
  x <- c(0.1 + 0.2, 1 / 3, 1e-20, 1e300, -0.5, 123456.789)
  json <- json_load_text(data.table::data.table(d = x))
  expect_identical(yyjsonr::read_json_str(json)$d, x)
  expect_match(json, '"d":0.30000000000000004', fixed = TRUE)
})

test_that("the JSON text writes a Date as YYYY-MM-DD, a POSIXct as as.character(), and a factor as its labels", {
  ts <- as.POSIXct(
    c("2020-01-06 12:34:56", "2020-01-07 00:00:00", NA),
    tz = "UTC"
  )
  dt <- data.table::data.table(
    dt = as.Date(c("2020-01-06", "1900-01-01", NA)),
    idt = data.table::as.IDate(c("2099-12-31", NA, "2000-02-29")),
    ts = ts,
    f = factor(c("lv1", NA, "lv2"))
  )
  back <- yyjsonr::read_json_str(json_load_text(dt))
  expect_identical(back$dt, c("2020-01-06", "1900-01-01", NA))
  expect_identical(back$idt, c("2099-12-31", NA, "2000-02-29"))
  expect_identical(back$ts, as.character(ts))
  expect_identical(back$f, c("lv1", NA, "lv2"))
})

test_that("json_load_chunks() splits by rows, and by the estimated bytes of JSON", {
  dt <- data.table::data.table(id = 1:10, x = strrep("y", 20))
  expect_identical(
    json_load_chunks(dt, names(dt), max_rows = 4L, max_bytes = 2^28),
    list(1:4, 5:8, 9:10)
  )
  per_row <- nchar(json_load_text(data.table::copy(dt)), type = "bytes") / 10
  chunks <- json_load_chunks(
    dt,
    names(dt),
    max_rows = 500000L,
    max_bytes = 3.5 * per_row
  )
  expect_identical(lengths(chunks), c(3L, 3L, 3L, 1L))
  expect_identical(unlist(chunks), 1:10)
})

test_that("json_load_tables() loads every chunk of every table in one transaction, with json_populate_recordset", {
  rec <- local_json_server()
  tables <- json_tables()
  data <- json_data()
  got <- json_conditions(json_load_fixture(tables, data, max_rows = 2L))
  expect_null(got$error)
  ins <- insert_statements(rec)
  expect_length(ins, 3)
  expect_identical(
    ins[[1]],
    "INSERT INTO `t1` (`id`, `x`) SELECT `id`, `x` FROM json_populate_recordset(NULL::`t1`, CAST(? AS json))"
  )
  expect_identical(json_rows_of(tables[[1]]), c("1 a1", "2 a2", "3 a3"))
  expect_identical(json_rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_identical(rec$psql, 0L)
})

test_that("json_load_tables() locks every table IN ROW EXCLUSIVE MODE before the first INSERT", {
  rec <- local_json_server()
  tables <- json_tables()
  json_load_fixture(tables, json_data(), max_rows = 2L, load_timeout = 0)
  expect_identical(
    rec$sql,
    c(
      "LOCK TABLE `t1` IN ROW EXCLUSIVE MODE",
      "LOCK TABLE `t2` IN ROW EXCLUSIVE MODE",
      rep("INSERT INTO `t1` (`id`, `x`) SELECT `id`, `x` FROM json_populate_recordset(NULL::`t1`, CAST(? AS json))", 2),
      "INSERT INTO `t2` (`id`, `x`) SELECT `id`, `x` FROM json_populate_recordset(NULL::`t2`, CAST(? AS json))"
    )
  )
})

test_that("json_load_tables() locks the tables in the sorted order of their names, whatever the order of the load", {
  rec <- local_json_server()
  tables <- json_tables()
  json_load_fixture(rev(tables), rev(json_data()), load_timeout = 0)
  expect_identical(
    rec$sql[1:2],
    c("LOCK TABLE `t1` IN ROW EXCLUSIVE MODE", "LOCK TABLE `t2` IN ROW EXCLUSIVE MODE")
  )
  expect_match(insert_statements(rec)[[1]], "^INSERT INTO `t2` ")
})

test_that("json_load_tables() sets lock_timeout first when it is given, and not otherwise", {
  rec <- local_json_server()
  tables <- json_tables()
  json_load_fixture(tables, json_data(), lock_timeout = 60)
  expect_identical(rec$sql[[1]], "SET LOCAL lock_timeout = '60s'")
  rec2 <- local_json_server()
  tables2 <- json_tables()
  json_load_fixture(tables2, json_data())
  expect_false(any(grepl("lock_timeout", rec2$sql, fixed = TRUE)))
})

test_that("load_timeout sets statement_timeout to the time left before each statement", {
  rec <- local_json_server()
  tables <- json_tables()
  json_load_fixture(tables, json_data(), load_timeout = 3600)
  before <- rec$sql[which(grepl("^(INSERT|LOCK)", rec$sql)) - 1L]
  expect_length(before, 4)
  ms <- as.numeric(sub(
    "^SET LOCAL statement_timeout = ([0-9]+)$",
    "\\1",
    before
  ))
  expect_true(all(ms > 3500000 & ms <= 3600000))
  rec2 <- local_json_server()
  tables2 <- json_tables()
  json_load_fixture(tables2, json_data(), load_timeout = 0)
  expect_false(any(grepl("statement_timeout", rec2$sql, fixed = TRUE)))
})

test_that("a statement that stores fewer rows than sent rolls back every table and stops with csdb_load_other", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") 1 else NULL
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_identical(got$error$attempts, 1L)
  expect_length(waits$waits, 0)
  expect_match(
    got$message,
    "table `t2` stored 1 rows of one statement, and csdb sent 2 rows.",
    fixed = TRUE
  )
  expect_match(
    got$message,
    "so it stored no row in tables `t1`, `t2`.",
    fixed = TRUE
  )
  expect_identical(json_rows_of(tables[[1]]), character())
  expect_identical(json_rows_of(tables[[2]]), character())
})

test_that("a lock timeout rolls back and runs the whole load again", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2" && n == 1L) json_text_lock else NULL
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(
    tables,
    json_data(),
    lock_timeout = 60
  ))
  expect_null(got$error)
  expect_identical(waits$waits, 2)
  expect_identical(sum(rec$sql == "SET LOCAL lock_timeout = '60s'"), 2L)
  expect_identical(json_rows_of(tables[[1]]), c("1 a1", "2 a2", "3 a3"))
  expect_identical(json_rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_match(
    got$messages[[1]],
    "failed on attempt 1 of at most 3 (transient). Retrying in 2 s.",
    fixed = TRUE
  )
  expect_match(
    got$messages[[2]],
    "succeeded on attempt 2 of at most 3.",
    fixed = TRUE
  )
})

test_that("a lock timeout on every attempt stops after 3 attempts with csdb_load_transient", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) json_text_lock)
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_identical(got$error$attempts, 3L)
  expect_identical(waits$waits, c(2, 10))
  expect_identical(json_rows_of(tables[[1]]), character())
})

test_that("a duplicate key stops with csdb_load_duplicate_key, stores no row and is not retried", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") json_text_duplicate else NULL
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_s3_class(got$error, "csdb_load_error")
  expect_identical(got$error$tool, "json_populate_recordset")
  expect_identical(got$error$attempts, 1L)
  expect_length(waits$waits, 0)
  expect_identical(json_rows_of(tables[[1]]), character())
})

test_that("a type error stops with csdb_load_other and stores no row", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") json_text_type else NULL
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_identical(json_rows_of(tables[[1]]), character())
})

test_that("a connection that breaks during the load is ambiguous, and csdb does not retry it", {
  for (text in c(json_text_lost, json_text_lost_after)) {
    waits <- local_json_waits()
    rec <- local_json_server(fail = function(table, n) {
      if (table == "t2") text else NULL
    })
    tables <- json_tables()
    got <- json_conditions(json_load_fixture(tables, json_data()))
    expect_s3_class(got$error, "csdb_load_ambiguous")
    expect_length(waits$waits, 0)
    expect_match(
      got$message,
      "The connection broke during the load",
      fixed = TRUE
    )
    expect_match(
      got$message,
      "Count the rows in tables `t1`, `t2`",
      fixed = TRUE
    )
  }
})

test_that("an error of the COMMIT is ambiguous, and csdb does not retry it", {
  waits <- local_json_waits()
  rec <- local_json_server()
  local_mocked_bindings(json_load_commit = function(connection) {
    stop(json_text_commit, call. = FALSE)
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(waits$waits, 0)
  expect_match(
    got$message,
    "The COMMIT failed, so the server may or may not have committed",
    fixed = TRUE
  )
  expect_match(got$message, "late failure at commit", fixed = TRUE)
})

test_that("classify_json_failure() sorts the server texts into the failure classes", {
  cls <- function(text, rolled_back = TRUE) {
    e <- simpleError(text)
    e$csdb_rolled_back <- rolled_back
    return(classify_json_failure(e))
  }
  expect_identical(cls(json_text_duplicate), "duplicate key")
  expect_identical(cls(json_text_lock), "transient")
  expect_identical(cls("ERROR: deadlock detected"), "transient")
  expect_identical(
    cls("ERROR: could not serialize access due to concurrent update"),
    "transient"
  )
  expect_identical(cls(json_text_lost), "ambiguous")
  expect_identical(cls(json_text_lost_after), "ambiguous")
  expect_identical(cls(json_text_type), "other")
  expect_identical(cls(json_text_statement_timeout), "other")
  expect_identical(cls(json_text_german), "other")
  for (text in c(json_text_duplicate, json_text_lock, json_text_type, json_text_german)) {
    expect_identical(cls(text, rolled_back = FALSE), "ambiguous")
  }
  expect_identical(classify_json_failure(simpleError(json_text_lock)), "ambiguous")
  commit <- simpleError(json_text_duplicate)
  commit$csdb_phase <- "commit"
  commit$csdb_rolled_back <- TRUE
  expect_identical(classify_json_failure(commit), "ambiguous")
})

test_that("a chunk whose JSON text is longer than max_bytes goes as two statements", {
  rec <- local_json_server()
  tables <- json_tables()
  data <- list(data.table::data.table(
    id = 1:4,
    x = c("a", "b", "c", strrep("z", 1000))
  ))
  json_load_fixture(tables[1], data, max_bytes = 600)
  expect_length(insert_statements(rec), 3)
  expect_identical(json_rows_of(tables[[1]])[1:3], c("1 a", "2 b", "3 c"))
  expect_length(json_rows_of(tables[[1]]), 4)
})

test_that("json_load_tables() leaves the caller's data.table unchanged", {
  rec <- local_json_server()
  tables <- json_tables()
  d <- data.table::data.table(id = 1:2, x = c(TRUE, NA))
  before <- data.table::copy(d)
  json_load_fixture(tables[1], list(d))
  expect_identical(d, before)
  expect_identical(json_rows_of(tables[[1]]), c("1 1", "2 NA"))
})

test_that("the PostgreSQL load_data_infile() with pg_load = \"json\" starts no psql", {
  rec <- local_json_server()
  tables <- json_tables()
  pg <- S7::method(load_data_infile, db_postgres)
  conn <- tables[[1]]$dbconnection
  got <- json_conditions(pg(
    connection = conn$autoconnection,
    dbconfig = conn$config,
    table = "t1",
    dt = json_data()[[1]]
  ))
  expect_null(got$error)
  expect_identical(rec$psql, 0L)
  expect_length(insert_statements(rec), 1)
  expect_identical(json_rows_of(tables[[1]]), c("1 a1", "2 a2", "3 a3"))
})

test_that("the PostgreSQL load_data_infile() with pg_load = \"psql\" runs psql", {
  rec <- local_json_server()
  tables <- json_tables(pg_load = "psql")
  pg <- S7::method(load_data_infile, db_postgres)
  conn <- tables[[1]]$dbconnection
  got <- json_conditions(pg(
    connection = conn$autoconnection,
    dbconfig = conn$config,
    table = "t1",
    dt = json_data()[[1]]
  ))
  # Without psql on PATH, csdb stops before it starts the client.
  expect_match(got$message, "csdb started psql|psql command not found")
  expect_length(insert_statements(rec), 0)
})

test_that("insert_data_many() with pg_load = \"json\" loads every table in one transaction with lock_timeout and starts no psql", {
  rec <- local_json_server()
  tables <- json_tables()
  got <- json_conditions(with_mocked_bindings(
    insert_data_many(tables, json_data()),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_identical(rec$psql, 0L)
  expect_identical(rec$sql[[1]], "SET LOCAL lock_timeout = '60s'")
  expect_length(insert_statements(rec), 2)
  expect_identical(json_rows_of(tables[[1]]), c("1 a1", "2 a2", "3 a3"))
  expect_identical(json_rows_of(tables[[2]]), c("1 b1", "2 b2"))
})

test_that("insert_data_many() with pg_load = \"json\" stores no row in any table after a failure in table 2", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") json_text_type else NULL
  })
  tables <- json_tables()
  got <- json_conditions(with_mocked_bindings(
    insert_data_many(tables, json_data()),
    loads_through_psql = function(connection) TRUE
  ))
  expect_s3_class(got$error, "csdb_load_other")
  expect_identical(json_rows_of(tables[[1]]), character())
  expect_identical(json_rows_of(tables[[2]]), character())
})

test_that("no message or error of the JSON load holds the password", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    paste("ERROR: canceling statement due to lock timeout", json_secret)
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_false(any(grepl(
    json_secret,
    c(got$message, got$messages),
    fixed = TRUE
  )))
  expect_match(got$message, "lock timeout ***", fixed = TRUE)
})

# Round 2 ----

# Replace DBI::dbConnect() with an in-memory SQLite connection, as
# local_recorded_connect() in test-password-hook.R does.
json_recorded_connect <- function(.local_envir = parent.frame()) {
  real_connect <- DBI::dbConnect
  local_mocked_bindings(
    dbConnect = function(drv, ...) real_connect(RSQLite::SQLite(), ":memory:"),
    .package = "DBI",
    .env = .local_envir
  )
  return(invisible(NULL))
}

test_that("a row whose JSON text is longer than max_row_bytes stops the load before csdb sends it", {
  waits <- local_json_waits()
  rec <- local_json_server()
  tables <- json_tables()
  data <- list(data.table::data.table(id = 1:3, x = c("a", "b", strrep("z", 1000))))
  got <- json_conditions(json_load_fixture(tables[1], data, max_rows = 1L, max_row_bytes = 500))
  expect_s3_class(got$error, "csdb_load_other")
  expect_length(insert_statements(rec), 2)
  expect_match(got$message, "row 3 of the data for table `t1` is ", fixed = TRUE)
  expect_match(got$message, "csdb did not send it.", fixed = TRUE)
  expect_length(waits$waits, 0)
  expect_identical(json_rows_of(tables[[1]]), character())
})

test_that("json_load_row_max_bytes is 900 MB, below the 1 GB limit of PostgreSQL", {
  expect_identical(json_load_row_max_bytes, 900 * 2^20)
  expect_identical(formals(json_load_tables)$max_row_bytes, quote(json_load_row_max_bytes))
})

test_that("a ROLLBACK that fails after a completed chunk is ambiguous, is not retried, and claims no rollback", {
  for (text in c(json_text_lock, json_text_type, json_text_duplicate)) {
    waits <- local_json_waits()
    rec <- local_json_server(fail = function(table, n) {
      if (table == "t2") text else NULL
    })
    local_mocked_bindings(json_load_rollback_once = function(connection) {
      DBI::dbRollback(connection)
      stop("could not send ROLLBACK: connection dead", call. = FALSE)
    })
    tables <- json_tables()
    got <- json_conditions(json_load_fixture(tables, json_data()))
    expect_s3_class(got$error, "csdb_load_ambiguous")
    expect_identical(got$error$attempts, 1L)
    expect_length(waits$waits, 0)
    expect_length(insert_statements(rec), 2)
    expect_match(got$message, "csdb could not confirm the ROLLBACK", fixed = TRUE)
    expect_false(grepl("stored no row", got$message, fixed = TRUE))
  }
})

test_that("a connection that does not answer SELECT 1 after the ROLLBACK is ambiguous", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") json_text_lock else NULL
  })
  local_mocked_bindings(json_load_alive = function(connection) FALSE)
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(waits$waits, 0)
})

test_that("an error text in another language is csdb_load_other when the connection answers", {
  waits <- local_json_waits()
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") json_text_german else NULL
  })
  tables <- json_tables()
  got <- json_conditions(json_load_fixture(tables, json_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_length(waits$waits, 0)
  expect_identical(json_rows_of(tables[[1]]), character())
})

test_that("json_load_locale_text() accepts C, POSIX and English, and warns for any other lc_messages", {
  for (lc in c("C", "POSIX", "C.UTF-8", "en_US.UTF-8", "en_GB.utf8", "en")) {
    expect_null(json_load_locale_text(lc), label = lc)
  }
  for (lc in c("de_DE.UTF-8", "nb_NO.UTF-8", "", NA_character_)) {
    expect_match(json_load_locale_text(lc), "csdb_load_other or csdb_load_ambiguous", fixed = TRUE, label = lc)
  }
})

test_that("a new PostgreSQL connection with pg_load = \"json\" warns once when lc_messages is not English", {
  json_recorded_connect()
  seen <- new.env()
  seen$n <- 0L
  local_mocked_bindings(json_load_lc_messages = function(connection) {
    seen$n <- seen$n + 1L
    return("de_DE.UTF-8")
  })
  db <- DBConnection_v9$new(driver = "PostgreSQL Unicode", server = "h", port = 5432L, db = "d", user = "u", password = "pw", pg_load = "json")
  expect_warning(db$connect(), "lc_messages = 'de_DE.UTF-8'", fixed = TRUE)
  expect_no_warning(db$autoconnection)
  expect_identical(seen$n, 1L)
  db$disconnect()
  psql <- DBConnection_v9$new(driver = "PostgreSQL Unicode", server = "h", port = 5432L, db = "d", user = "u", password = "pw")
  expect_no_warning(psql$connect())
  expect_identical(seen$n, 1L)
  psql$disconnect()
})

test_that("an English lc_messages gives no warning", {
  json_recorded_connect()
  local_mocked_bindings(json_load_lc_messages = function(connection) "en_US.UTF-8")
  db <- DBConnection_v9$new(driver = "PostgreSQL Unicode", server = "h", port = 5432L, db = "d", user = "u", password = "pw", pg_load = "json")
  expect_no_warning(db$connect())
  db$disconnect()
})

# The tables of json_tables(), with a connection that can no longer open: the
# SQLite file is under a path that is a file.
json_broken_tables <- function(pg_load = "json", .local_envir = parent.frame()) {
  tables <- json_tables(pg_load = pg_load, .local_envir = .local_envir)
  conn <- tables[[1]]$dbconnection
  conn$disconnect()
  blocker <- withr::local_tempfile(.local_envir = .local_envir)
  writeLines("x", blocker)
  conn$config$db <- file.path(blocker, "sub", "x.sqlite")
  return(tables)
}

test_that("a connection that cannot open for a JSON load stops with csdb_load_transient and the original message", {
  waits <- local_json_waits()
  rec <- local_json_server()
  tables <- json_broken_tables()
  got <- json_conditions(tables[[1]]$insert_data(json_data()[[1]], verbose = FALSE))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_s3_class(got$error, "csdb_load_error")
  expect_identical(got$error$tool, "json_populate_recordset")
  expect_match(got$message, "failed before csdb sent a row: csdb could not connect.", fixed = TRUE)
  expect_match(got$message, "Failed to connect to database after 3 attempts", fixed = TRUE)
  expect_match(got$message, "The last attempt said: Attempt 3 of 3 to connect failed:", fixed = TRUE)
  expect_match(got$message, "unable to open database file", fixed = TRUE)
  expect_s3_class(got$error$parent, "error")
  expect_length(insert_statements(rec), 0)
  got2 <- json_conditions(insert_data_many(tables, json_data()))
  expect_s3_class(got2$error, "csdb_load_transient")
  got3 <- json_conditions(tables[[1]]$upsert_data(json_data()[[1]], verbose = FALSE))
  expect_s3_class(got3$error, "csdb_load_transient")
})

test_that("with pg_load = \"psql\" a connection that cannot open keeps its plain error", {
  waits <- local_json_waits()
  rec <- local_json_server()
  tables <- json_broken_tables(pg_load = "psql")
  got <- json_conditions(tables[[1]]$insert_data(json_data()[[1]], verbose = FALSE))
  expect_false(inherits(got$error, "csdb_load_error"))
  expect_match(got$message, "Failed to connect to database after 3 attempts", fixed = TRUE)
})
