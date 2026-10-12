# insert_data_many() loads several tables. On PostgreSQL one psql session
# loads all of them in one transaction, so a failure stores no row in any
# table. Before 2026.10.13 a caller called $insert_data() once for each table,
# and each call started its own psql process and its own login.
#
# The tests put a fake psql first on PATH. It records its arguments and the
# script that -f names, and prints one "COPY <n>" for each \copy line. SQLite
# tables stand in for the PostgreSQL tables: a SQLite connection answers
# dbListFields(), and loads_through_psql() is mocked to TRUE. The fake is a
# POSIX shell script, so these tests skip on Windows and run in CI on Linux.

# FAKE_MODE is a comma-separated list: the first psql run uses the first
# entry, the second run the second, and the last entry repeats.
#
#   ok       one "COPY <rows>" line for each \copy line.
#   short2   as ok, but the count for the second \copy is one row short.
#   dupkey2  "COPY <rows>" for the first \copy, then the duplicate-key error
#            that psql 14.24 printed for the second \copy of a session
#            against PostgreSQL 14.24 on 2026-10-12, and exit status 3.
#   lock2    as dupkey2, with the error of a lock wait longer than
#            lock_timeout, SQLSTATE 55P03.
#   proxy    the error line of a login that a token proxy refuses.
#
# The fake prints "SET" for a line that starts with "SET ", as psql does. A -c
# argument is read as a script of one line, so the fake also serves the
# single-table load of load_data_infile().
fake_psql_session <- c(
  "#!/bin/sh",
  "{ printf '%s\\n' psql; for a in \"$@\"; do printf '%s\\n' \"$a\"; done; printf '%s\\n' '---'; } >> \"$FAKE_LOG\"",
  "n=$(( $(cat \"$FAKE_LOG.n\" 2>/dev/null || echo 0) + 1 )); echo \"$n\" > \"$FAKE_LOG.n\"",
  "mode=$(printf '%s\\n' \"$FAKE_MODE\" | cut -s -d, -f\"$n\")",
  "if [ -z \"$mode\" ]; then mode=$(printf '%s\\n' \"$FAKE_MODE\" | awk -F, '{print $NF}'); fi",
  "script=\"$FAKE_LOG.script.$n\"; : > \"$script\"; prev=''",
  "for a in \"$@\"; do",
  "  if [ \"$prev\" = '-f' ]; then cat \"$a\" > \"$script\"; fi",
  "  if [ \"$prev\" = '-c' ]; then printf '%s\\n' \"$a\" > \"$script\"; fi",
  "  prev=$a",
  "done",
  "if [ \"$mode\" = proxy ]; then",
  "  echo 'psql: error: connection to server at \"pg-auth\" (10.0.0.9), port 5432 failed: ERROR:  internal proxy error' >&2",
  "  exit 2",
  "fi",
  "i=0",
  "while IFS= read -r line; do",
  "  if [ \"${line#SET }\" != \"$line\" ]; then echo SET; continue; fi",
  "  i=$((i + 1))",
  "  file=$(printf '%s' \"$line\" | sed -n \"s/.* from '\\(.*\\)' (FORMAT .*/\\1/p\" | sed \"s/''/'/g\")",
  "  rows=$(wc -l < \"$file\" | tr -d ' ')",
  "  if [ \"$mode\" = short2 ] && [ $i -eq 2 ]; then rows=$((rows - 1)); fi",
  "  if [ \"$mode\" = dupkey2 ] && [ $i -eq 2 ]; then",
  "    echo \"psql:$script:$((i + 1)): ERROR:  23505: duplicate key value violates unique constraint \\\"pk_t2\\\"\" >&2",
  "    echo 'DETAIL:  Key (id)=(1) already exists.' >&2",
  "    exit 3",
  "  fi",
  "  if [ \"$mode\" = lock2 ] && [ $i -eq 2 ]; then",
  "    echo \"psql:$script:$((i + 1)): ERROR:  55P03: canceling statement due to lock timeout\" >&2",
  "    exit 3",
  "  fi",
  "  echo \"COPY $rows\"",
  "done < \"$script\"",
  "exit 0"
)

local_fake_psql <- function(mode, .local_envir = parent.frame()) {
  skip_on_os("windows")
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  path <- file.path(dir, "psql")
  writeLines(fake_psql_session, path)
  Sys.chmod(path, "0755")
  log <- file.path(dir, "argv.log")
  file.create(log)
  withr::local_envvar(
    PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
    FAKE_MODE = mode,
    FAKE_LOG = log,
    .local_envir = .local_envir
  )
  return(log)
}

# The arguments of every psql run, one character vector each, without the
# leading "psql".
fake_psql_runs <- function(log) {
  lines <- readLines(log)
  ends <- which(lines == "---")
  starts <- c(1, utils::head(ends, -1) + 1)
  return(Map(function(s, e) lines[(s + 1):(e - 1)], starts, ends))
}

# The script that psql run `n` received.
fake_psql_script <- function(log, n) {
  return(readLines(paste0(log, ".script.", n)))
}

local_session_waits <- function(.local_envir = parent.frame()) {
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

capture_conditions <- function(expr) {
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

session_secret <- "s3cret-Pw"

# Three SQLite tables t1, t2 and t3 on one shared connection. Each table
# exists before the test starts, and the connection carries the settings that
# the psql command line reads.
three_tables <- function(.local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  conn <- sqlite_connection(cfg)
  withr::defer(conn$disconnect(), envir = .local_envir)
  tables <- lapply(c("t1", "t2", "t3"), function(name) {
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
  conn$config$user <- "fakeuser"
  conn$config$server <- "fakehost"
  conn$config$port <- 5432
  conn$config$db <- "fakedb"
  conn$config$password <- session_secret
  return(tables)
}

three_data <- function() {
  return(list(
    data.table::data.table(id = 1:2, x = c("a1", "a2")),
    data.table::data.table(id = 1:2, x = c("b1", "b2")),
    data.table::data.table(id = 1:2, x = c("c1", "c2"))
  ))
}

rows_of <- function(tab) {
  d <- DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    paste0("SELECT id, x FROM ", tab$table_name, " ORDER BY id")
  )
  return(paste(d$id, d$x))
}

# Run insert_data_many() as if the tables were on PostgreSQL. A
# load_data_infile() call for one table, which the per-table path makes,
# also goes to the PostgreSQL method and so to the fake psql. RSQLite quotes
# each identifier with backticks, where PostgreSQL uses double quotes.
insert_as_postgres <- function(tables, data, ...) {
  pg <- S7::method(load_data_infile, db_postgres)
  return(with_mocked_bindings(
    insert_data_many(tables, data, ...),
    loads_through_psql = function(connection) TRUE,
    load_data_infile = function(...) pg(...)
  ))
}

test_that("insert_data_many() loads 3 tables with one psql run, one \\copy each, in order", {
  log <- local_fake_psql("ok")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)

  runs <- fake_psql_runs(log)
  expect_length(runs, 1)
  args <- runs[[1]]
  expect_true("--single-transaction" %in% args || "-1" %in% args)
  at <- match(c("ON_ERROR_STOP=1", "VERBOSITY=verbose"), args)
  expect_false(anyNA(at))
  expect_identical(args[at - 1L], c("-v", "-v"))
  expect_true("-f" %in% args)
  expect_false("-c" %in% args)
  expect_false(any(grepl(session_secret, args, fixed = TRUE)))

  script <- fake_psql_script(log, 1)
  expect_length(script, 4)
  expect_match(script[[2]], "^\\\\copy `t1` \\(`id`, `x`\\) from '")
  expect_match(script[[3]], "^\\\\copy `t2` \\(`id`, `x`\\) from '")
  expect_match(script[[4]], "^\\\\copy `t3` \\(`id`, `x`\\) from '")
  expect_length(waits$waits, 0)
})

test_that("the session script sets lock_timeout to 60 s on its first line", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  expect_identical(load_lock_timeout_seconds, 60)
  expect_identical(fake_psql_script(log, 1)[[1]], "SET lock_timeout = '60s';")
})

test_that("insert_data_many() runs the whole session again after a lock timeout", {
  log <- local_fake_psql("lock2,ok")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 2)
  expect_identical(waits$waits, 2)
  expect_identical(fake_psql_script(log, 1), fake_psql_script(log, 2))
  expect_match(
    got$messages,
    "failed on attempt 1 of at most 3 (transient). Retrying in 2 s.",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    got$messages,
    "55P03: canceling statement due to lock timeout",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data_many() stops when psql reports a wrong count for table 2", {
  log <- local_fake_psql("short2")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_match(
    got$message,
    "stored 1 rows into table `t2`, and csdb sent 2 rows",
    fixed = TRUE
  )
  expect_match(
    got$message,
    "Loading data into tables `t1`, `t2`, `t3` failed.",
    fixed = TRUE
  )
  expect_length(fake_psql_runs(log), 1)
  expect_length(waits$waits, 0)
})

test_that("insert_data_many() runs the whole session again after a proxy refusal", {
  log <- local_fake_psql("proxy,ok")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 2)
  expect_identical(waits$waits, 2)
  expect_length(fake_psql_script(log, 1), 4)
  expect_identical(fake_psql_script(log, 1), fake_psql_script(log, 2))
  expect_match(
    got$messages,
    "succeeded on attempt 2 of at most 3",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data_many() skips a table whose data has no rows", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  data <- three_data()
  data[[2]] <- data[[2]][0]
  got <- capture_conditions(insert_as_postgres(tables, data))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  script <- fake_psql_script(log, 1)
  expect_length(script, 3)
  expect_match(script[[2]], "^\\\\copy `t1` ")
  expect_match(script[[3]], "^\\\\copy `t3` ")
})

test_that("insert_data_many() stops on a duplicate key without confirm_insert_via_nrow", {
  log <- local_fake_psql("dupkey2")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_length(fake_psql_runs(log), 1)
  expect_length(waits$waits, 0)
})

test_that("insert_data_many(confirm_insert_via_nrow = TRUE) inserts table by table after a duplicate key", {
  log <- local_fake_psql("dupkey2")
  tables <- three_tables()
  suppressMessages(
    tables[[2]]$insert_data(data.table::data.table(id = 1L, x = "old1"))
  )
  data <- three_data()
  data[[2]] <- data.table::data.table(id = c(1L, 3L), x = c("new1", "new3"))

  # The session goes to the fake psql. The per-table loads after it are the
  # real SQLite loads, except the first load of t2, which fails as the
  # PostgreSQL load fails on a duplicate key.
  real <- load_data_infile
  per_table <- new.env()
  per_table$tables <- character()
  local_mocked_bindings(
    load_data_infile = function(connection, dbconfig = NULL, table, ...) {
      name <- if (methods::is(table, "Id")) table@name[["table"]] else table
      per_table$tables <- c(per_table$tables, name)
      if (identical(name, "t2") && sum(per_table$tables == "t2") == 1L) {
        stop(errorCondition(
          "Loading data into table t2 failed (mocked).",
          class = c("csdb_load_duplicate_key", "csdb_load_error"),
          table = "t2",
          tool = "psql \\copy",
          attempts = 1L,
          failure_class = "duplicate key"
        ))
      }
      return(real(
        connection = connection,
        dbconfig = dbconfig,
        table = table,
        ...
      ))
    }
  )
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, data, confirm_insert_via_nrow = TRUE),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  # t1, then t2, then the staging table of the upsert of t2, then t3.
  expect_length(per_table$tables, 4)
  expect_identical(per_table$tables[c(1, 2, 4)], c("t1", "t2", "t3"))
  expect_match(per_table$tables[[3]], "^tmp")
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 new1", "3 new3"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
  expect_match(
    got$messages,
    "stored no row in any of the 3 tables",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    got$messages,
    "on table t2: psql \\copy failed on attempt 1 (duplicate key)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data_many(confirm_insert_via_nrow = TRUE) upserts each table that holds fewer rows after the session", {
  # The fake psql stores nothing, so each table holds 0 rows after a session
  # that succeeded. Each upsert is a real SQLite upsert.
  log <- local_fake_psql("ok")
  tables <- three_tables()
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, three_data(), confirm_insert_via_nrow = TRUE),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
  expect_identical(
    sum(grepl(
      "After insert have 0 rows. Tried to insert 2. Now trying upsert.",
      got$messages,
      fixed = TRUE
    )),
    3L
  )
})

test_that("insert_data_many() inserts table by table on a driver without psql", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  got <- capture_conditions(insert_data_many(tables, three_data()))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 0)
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
})

test_that("insert_data_many() inserts table by table when the tables differ in configuration", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  other <- DBTable_v9$new(
    dbconfig = sqlite_dbconfig(),
    table_name = "t4",
    field_types = c(id = "INTEGER", x = "TEXT"),
    keys = "id"
  )
  withr::defer(other$disconnect())
  data <- c(three_data(), list(data.table::data.table(id = 1L, x = "d1")))
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(c(tables, list(other)), data),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 0)
  expect_identical(rows_of(other), "1 d1")
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
})

# The remaining tests need no fake psql, so they also run on Windows.

test_that("insert_data_many() passes ... to insert_data() of each table", {
  Recording <- R6::R6Class(
    "Recording",
    inherit = DBTable_v9,
    public = list(
      seen = NULL,
      insert_data = function(newdata, ..., marker = "unset") {
        self$seen <- marker
        return(super$insert_data(newdata, ...))
      }
    )
  )
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)
  withr::defer(conn$disconnect())
  tables <- lapply(c("r1", "r2"), function(name) {
    return(Recording$new(
      dbconfig = cfg,
      table_name = name,
      field_types = c(id = "INTEGER", x = "TEXT"),
      keys = "id",
      dbconnection = conn
    ))
  })
  data <- three_data()[1:2]
  suppressMessages(insert_data_many(tables, data, marker = "passed"))
  expect_identical(tables[[1]]$seen, "passed")
  expect_identical(tables[[2]]$seen, "passed")
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
})

test_that("insert_data_many() rejects tables and newdata that do not pair up", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "t1",
    field_types = c(id = "INTEGER", x = "TEXT"),
    keys = "id"
  )
  withr::defer(tab$disconnect())
  d <- three_data()[[1]]
  expect_error(insert_data_many(tab, list(d)), "tables MUST be a list")
  expect_error(insert_data_many(list(tab), d), "newdata MUST be a list")
  expect_error(
    insert_data_many(list(tab), list(d, d)),
    "one data.table for each table"
  )
  expect_null(insert_data_many(list(), list()))
})

test_that("loads_through_psql() is TRUE for the PostgreSQL class and FALSE for SQLite", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  expect_false(loads_through_psql(con))
  pg <- if (methods::is(db_postgres, "classRepresentation")) {
    methods::new(db_postgres@className)
  } else {
    structure(list(), class = "PostgreSQL")
  }
  expect_true(loads_through_psql(pg))
})

test_that("classify_load_failure() reads the SQLSTATE after the prefix of a psql script", {
  # psql 14.24 printed the first two lines against PostgreSQL 14.24 on
  # 2026-10-12, for a duplicate key on line 2 of a script that -f named.
  dup <- c(
    "COPY 2",
    "psql:/tmp/Rtmp03RHBd/file14de25362.sql:2: ERROR:  23505: duplicate key value violates unique constraint \"pk_anon_p20_week_xxpxx_p2_pk_912168375c5d5b73\"",
    "DETAIL:  Key (id)=(1) already exists."
  )
  expect_identical(classify_load_failure("psql", dup), "duplicate key")
  deadlock <- "psql:C:/Users/A B/AppData/x 1.sql:3: ERROR:  40P01: deadlock detected"
  expect_identical(classify_load_failure("psql", deadlock), "transient")
  lost <- "psql:/tmp/x.sql:1: FATAL:  57P01: terminating connection due to administrator command"
  expect_identical(classify_load_failure("psql", lost), "ambiguous")
  refused <- "psql: error: connection to server at \"h\" (10.0.0.9), port 5432 failed: Connection refused"
  expect_identical(classify_load_failure("psql", refused), "transient")
})
