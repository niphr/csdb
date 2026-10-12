# insert_data_many() loads several tables. On PostgreSQL one psql session
# loads all of them in one transaction, so a failure stores no row in any
# table. Before 2026.10.13 a caller called $insert_data() once for each table,
# and each call started its own psql process and its own login.
#
# The tests put a fake psql first on PATH. It records its arguments, the
# script that -f names and the value of PGPASSWORD, and it plays the part of
# psql and the server for the lines of the script. SQLite tables stand in for
# the PostgreSQL tables: a SQLite connection answers dbListFields(), and
# loads_through_psql() is mocked to TRUE. The fake is a POSIX shell script,
# so these tests skip on Windows and run in CI on Linux.

# FAKE_MODE is a comma-separated list: the first psql run uses the first
# entry, the second run the second, and the last entry repeats.
#
#   ok           ROW_COUNT and the "COPY <n>" line equal the lines of the
#                load file of each \copy.
#   short2       the second \copy stores one row less: ROW_COUNT and the
#                "COPY <n>" line are both one short.
#   tagshort2    ROW_COUNT is right, but the "COPY <n>" line of the second
#                \copy is one short.
#   dupkey2      "COPY <rows>" for the first \copy, then the duplicate-key
#                error that psql 14.24 printed for the second \copy of a
#                session against PostgreSQL 14.24 on 2026-10-12, and exit 3.
#   lock2        as dupkey2, with the error of a lock wait longer than
#                lock_timeout, SQLSTATE 55P03.
#   lost2        "COPY <rows>" for the first \copy, then the lines of a
#                connection that broke after the load started, as psql 14.24
#                printed them in test-load-tools.R, and exit 2. They have no
#                "psql: error: " prefix, so the class is ambiguous.
#   proxy        the error line of a login that a token proxy refuses.
#   echo_secret  prints every argument and PGPASSWORD as connection errors.
#   sleep        waits 5 s, then runs as ok.
#
# The fake runs the row count check of the script as psql and the server do.
# After a "SELECT :ROW_COUNT ... = <n> ..." line whose <n> differs from the
# rows of the \copy before it, it prints the \echo line of the \else branch
# with :csdb_rows replaced, then the RAISE EXCEPTION text of the DO line as a
# P0001 error, and exits 3. It ignores the other lines of the check. It
# prints "SET" for a line that starts with "SET ". A -c argument is read as a
# script of one line, so the fake also serves load_data_infile().
fake_psql_session <- c(
  "#!/bin/sh",
  "{ printf '%s\\n' psql; for a in \"$@\"; do printf '%s\\n' \"$a\"; done; printf '%s\\n' '---'; } >> \"$FAKE_LOG\"",
  "n=$(( $(cat \"$FAKE_LOG.n\" 2>/dev/null || echo 0) + 1 )); echo \"$n\" > \"$FAKE_LOG.n\"",
  "printf '%s\\n' \"$PGPASSWORD\" > \"$FAKE_LOG.pw.$n\"",
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
  "if [ \"$mode\" = echo_secret ]; then",
  "  for a in \"$@\"; do echo \"psql: error: $a\" >&2; done",
  "  echo \"psql: error: PGPASSWORD=$PGPASSWORD\" >&2",
  "  exit 2",
  "fi",
  "if [ \"$mode\" = sleep ]; then sleep 5; fi",
  "i=0; rc=0; failing=0",
  "while IFS= read -r line; do",
  "  if [ \"${line#SET }\" != \"$line\" ]; then echo SET; continue; fi",
  "  if [ \"${line#SELECT :ROW_COUNT}\" != \"$line\" ]; then",
  "    want=$(printf '%s' \"$line\" | sed -n 's/.*:ROW_COUNT = \\([0-9]*\\) AS csdb_rows_ok.*/\\1/p')",
  "    if [ \"$rc\" != \"$want\" ]; then failing=1; fi",
  "    continue",
  "  fi",
  "  if [ \"$failing\" = 1 ]; then",
  "    case \"$line\" in",
  "      *'echo csdb: '*) printf '%s\\n' \"$line\" | sed -e 's/^.echo //' -e \"s/:csdb_rows/$rc/\" ;;",
  "      'DO '*)",
  "        msg=$(printf '%s' \"$line\" | sed -n \"s/.*RAISE EXCEPTION '\\(.*\\)'; END.*/\\1/p\")",
  "        echo \"psql:$script:9: ERROR:  P0001: $msg\" >&2",
  "        exit 3 ;;",
  "    esac",
  "    continue",
  "  fi",
  "  case \"$line\" in",
  "    *'(FORMAT CSV'*) ;;",
  "    *) continue ;;",
  "  esac",
  "  i=$((i + 1))",
  "  file=$(printf '%s' \"$line\" | sed -n \"s/.* from '\\(.*\\)' (FORMAT .*/\\1/p\" | sed \"s/''/'/g\")",
  "  rows=$(wc -l < \"$file\" | tr -d ' ')",
  "  rc=$rows",
  "  if [ \"$mode\" = short2 ] && [ $i -eq 2 ]; then rc=$((rows - 1)); fi",
  "  tag=$rc",
  "  if [ \"$mode\" = tagshort2 ] && [ $i -eq 2 ]; then tag=$((rows - 1)); fi",
  "  if [ \"$mode\" = dupkey2 ] && [ $i -eq 2 ]; then",
  "    echo \"psql:$script:9: ERROR:  23505: duplicate key value violates unique constraint \\\"pk_t2\\\"\" >&2",
  "    echo 'DETAIL:  Key (id)=(1) already exists.' >&2",
  "    exit 3",
  "  fi",
  "  if [ \"$mode\" = lock2 ] && [ $i -eq 2 ]; then",
  "    echo \"psql:$script:9: ERROR:  55P03: canceling statement due to lock timeout\" >&2",
  "    exit 3",
  "  fi",
  "  if [ \"$mode\" = lost2 ] && [ $i -eq 2 ]; then",
  "    echo 'server closed the connection unexpectedly' >&2",
  "    echo '        This probably means the server terminated abnormally' >&2",
  "    echo '        before or while processing the request.' >&2",
  "    echo 'connection to server was lost' >&2",
  "    exit 2",
  "  fi",
  "  echo \"COPY $tag\"",
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

# The arguments of psql run `n`, or character(0) when there was no such run.
fake_psql_args <- function(log, n) {
  runs <- fake_psql_runs(log)
  if (length(runs) < n) {
    return(character(0))
  }
  return(runs[[n]])
}

# The script that psql run `n` received, or character(0).
fake_psql_script <- function(log, n) {
  f <- paste0(log, ".script.", n)
  if (!file.exists(f)) {
    return(character(0))
  }
  return(readLines(f))
}

# The \copy lines of a script, in order.
copy_lines <- function(script) {
  return(grep("(FORMAT CSV", script, fixed = TRUE, value = TRUE))
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

session_secret <- "s3cret-Pw-SENTINEL"

# A subclass whose $insert_data() and $upsert_data() take update_last_updated,
# as the cs9 tables do. Each call records the value it received. A call with
# update_last_updated = TRUE writes one last-updated record: here a count in
# `records`, in cs9 a row of config_tables_last_updated. The methods call
# their parent with the arguments that the parent takes.
Recording <- R6::R6Class(
  "Recording",
  inherit = DBTable_v9,
  public = list(
    seen_insert = character(),
    seen_upsert = character(),
    records = 0L,
    insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE,
      load_timeout = 3600,
      update_last_updated = TRUE
    ) {
      self$seen_insert <- c(self$seen_insert, as.character(update_last_updated))
      super$insert_data(
        newdata,
        confirm_insert_via_nrow = confirm_insert_via_nrow,
        verbose = verbose,
        load_timeout = load_timeout
      )
      if (update_last_updated) {
        self$records <- self$records + 1L
      }
      return(invisible(NULL))
    },
    upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE,
      load_timeout = 3600,
      update_last_updated = TRUE
    ) {
      self$seen_upsert <- c(self$seen_upsert, as.character(update_last_updated))
      super$upsert_data(
        newdata,
        drop_indexes = drop_indexes,
        verbose = verbose,
        load_timeout = load_timeout
      )
      if (update_last_updated) {
        self$records <- self$records + 1L
      }
      return(invisible(NULL))
    }
  )
)

# The number of last-updated records that each table wrote.
records_of <- function(tables) {
  return(vapply(tables, function(t) t$records, integer(1)))
}

# Three SQLite tables t1, t2 and t3 on one shared connection. Each table
# exists before the test starts, and the connection carries the settings that
# the psql command line reads.
three_tables <- function(class = DBTable_v9, .local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  conn <- sqlite_connection(cfg)
  withr::defer(conn$disconnect(), envir = .local_envir)
  tables <- lapply(c("t1", "t2", "t3"), function(name) {
    tab <- class$new(
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

# The first load of table t2 fails as the PostgreSQL load fails on a
# duplicate key. Every other load is the real SQLite load. Returns the
# environment whose `tables` lists every table that a load reached.
local_t2_duplicate_once <- function(.local_envir = parent.frame()) {
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
    },
    .env = .local_envir
  )
  return(per_table)
}

test_that("insert_data_many() loads 3 tables with one psql run, one \\copy each, in order", {
  log <- local_fake_psql("ok")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)

  expect_length(fake_psql_runs(log), 1)
  args <- fake_psql_args(log, 1)
  expect_true("--single-transaction" %in% args || "-1" %in% args)
  at <- match(c("ON_ERROR_STOP=1", "VERBOSITY=verbose"), args)
  expect_false(anyNA(at))
  expect_identical(args[at - 1L], c("-v", "-v"))
  expect_true("-f" %in% args)
  expect_false("-c" %in% args)

  copies <- copy_lines(fake_psql_script(log, 1))
  expect_length(copies, 3)
  expect_match(copies, "^\\\\copy `t[123]` \\(`id`, `x`\\) from '")
  expect_identical(substr(copies, 8, 9), c("t1", "t2", "t3"))
  expect_length(waits$waits, 0)
})

test_that("the session script checks the row count of each table inside the transaction", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  data <- three_data()
  data[[3]] <- data.table::data.table(id = 1:3, x = c("c1", "c2", "c3"))
  got <- capture_conditions(insert_as_postgres(tables, data))
  expect_null(got$error)
  script <- fake_psql_script(log, 1)
  at <- grep("(FORMAT CSV", script, fixed = TRUE)
  expect_length(at, 3)
  check <- function(n, k) {
    return(c(
      sprintf(
        "SELECT :ROW_COUNT AS csdb_rows, :ROW_COUNT = %d AS csdb_rows_ok \\gset",
        n
      ),
      "\\if :csdb_rows_ok",
      "\\else",
      sprintf(
        "\\echo csdb: table number %d of the session stored :csdb_rows rows, and csdb sent %d rows.",
        k,
        n
      ),
      sprintf(
        "DO $csdb$ BEGIN RAISE EXCEPTION 'csdb: table `t%d` did not store the %d rows that csdb sent. The server stored no row in any table.'; END $csdb$;",
        k,
        n
      ),
      "\\endif"
    ))
  }
  # at[k] is NA when a \copy line is missing, so the check fails and does not
  # error.
  expect_identical(script[at[1] + 1:6], check(2L, 1L))
  expect_identical(script[at[2] + 1:6], check(2L, 2L))
  expect_identical(script[at[3] + 1:6], check(3L, 3L))
  expect_length(script, 1 + 3 * 7)
})

test_that("the session script sets lock_timeout to 60 s on its first line", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  expect_identical(load_lock_timeout_seconds, 60)
  expect_identical(
    utils::head(fake_psql_script(log, 1), 1),
    "SET lock_timeout = '60s';"
  )
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

test_that("a short row count of table 2 stops the session inside the transaction", {
  log <- local_fake_psql("short2")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_match(
    got$message,
    "ERROR:  P0001: csdb: table `t2` did not store the 2 rows that csdb sent. The server stored no row in any table.",
    fixed = TRUE
  )
  expect_match(
    got$message,
    "csdb: table number 2 of the session stored 1 rows, and csdb sent 2 rows.",
    fixed = TRUE
  )
  expect_length(fake_psql_runs(log), 1)
  expect_length(waits$waits, 0)
})

# psql exited with 0, so the session committed. A COPY line that then shows
# a different count is ambiguous: the rows may be stored.
test_that("a short COPY line of table 2 after psql exits is ambiguous, and csdb does not retry it", {
  log <- local_fake_psql("tagshort2")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_match(
    got$message,
    "psql \\copy exited without an error, so the server may have committed rows to tables `t1`, `t2`, `t3`. Count the rows in tables `t1`, `t2`, `t3` before you load these rows again.",
    fixed = TRUE
  )
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
  expect_length(copy_lines(fake_psql_script(log, 1)), 3)
  expect_identical(fake_psql_script(log, 1), fake_psql_script(log, 2))
  expect_match(
    got$messages,
    "succeeded on attempt 2 of at most 3",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("the password reaches psql through PGPASSWORD only, and no message or script holds it", {
  log <- local_fake_psql("echo_secret")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  # The echoed lines are connection errors that csdb does not retry.
  expect_s3_class(got$error, "csdb_load_other")
  expect_length(waits$waits, 0)
  runs <- fake_psql_runs(log)
  expect_length(runs, 1)
  for (n in seq_along(runs)) {
    expect_identical(readLines(paste0(log, ".pw.", n)), session_secret)
    expect_false(any(grepl(session_secret, runs[[n]], fixed = TRUE)))
    expect_false(any(grepl(
      session_secret,
      fake_psql_script(log, n),
      fixed = TRUE
    )))
  }
  expect_false(grepl(session_secret, got$message, fixed = TRUE))
  expect_false(any(grepl(session_secret, got$messages, fixed = TRUE)))
  # The fake printed the password, and the message holds its redacted line.
  expect_match(got$message, "PGPASSWORD=***", fixed = TRUE)
})

test_that("load_timeout bounds the psql session", {
  log <- local_fake_psql("sleep")
  waits <- local_session_waits()
  tables <- three_tables()
  t0 <- Sys.time()
  got <- capture_conditions(
    insert_as_postgres(tables, three_data(), load_timeout = 1)
  )
  seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_match(got$message, "timed out after 1 s", fixed = TRUE)
  expect_length(fake_psql_runs(log), 1)
  expect_length(waits$waits, 0)
  expect_lt(seconds, 4)
})

test_that("insert_data_many() skips a table whose data has no rows", {
  log <- local_fake_psql("ok")
  tables <- three_tables()
  data <- three_data()
  data[[2]] <- data[[2]][0]
  got <- capture_conditions(insert_as_postgres(tables, data))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  copies <- copy_lines(fake_psql_script(log, 1))
  expect_length(copies, 2)
  expect_identical(substr(copies, 8, 9), c("t1", "t3"))
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
  # real SQLite loads, except the first load of t2.
  per_table <- local_t2_duplicate_once()
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, data, confirm_insert_via_nrow = TRUE),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  # t1, then t2, then the staging table of the upsert of t2, then t3.
  expect_length(per_table$tables, 4)
  expect_identical(per_table$tables[c(1, 2, 4)], c("t1", "t2", "t3"))
  expect_match(per_table$tables[3], "^tmp")
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

test_that("csdb writes no last-updated record after a session that commits", {
  log <- local_fake_psql("ok")
  tables <- three_tables(Recording)
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, three_data()),
    loads_through_psql = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(fake_psql_runs(log), 1)
  expect_identical(records_of(tables), c(0L, 0L, 0L))
  for (tab in tables) {
    expect_identical(tab$seen_insert, character(0))
    expect_identical(tab$seen_upsert, character(0))
  }
})

test_that("a session that loses its connection after the load starts is not retried", {
  log <- local_fake_psql("lost2")
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_psql_runs(log), 1)
  expect_length(waits$waits, 0)
  expect_match(
    got$message,
    "The failure class is ambiguous, after 1 attempt.",
    fixed = TRUE
  )
  expect_match(
    got$message,
    "server closed the connection unexpectedly",
    fixed = TRUE
  )
})

# The phrases that the installed help and NEWS.md MUST hold. The help is read
# from man/insert_data_many.Rd in the source tree, or from the installed
# package when the tests run from a built package.
rendered_help <- function() {
  src <- test_path("..", "..", "man", "insert_data_many.Rd")
  rd <- if (file.exists(src)) {
    tools::parse_Rd(src)
  } else {
    tools::Rd_db("csdb")[["insert_data_many.Rd"]]
  }
  withr::local_options(useFancyQuotes = FALSE)
  out <- utils::capture.output(tools::Rd2txt(
    rd,
    options = list(underline_titles = FALSE)
  ))
  return(gsub("\\s+", " ", paste(out, collapse = " ")))
}

news_text <- function() {
  src <- test_path("..", "..", "NEWS.md")
  f <- if (file.exists(src)) src else system.file("NEWS.md", package = "csdb")
  return(gsub("\\s+", " ", paste(readLines(f), collapse = " ")))
}

test_that("the help of insert_data_many() states the rules of the session", {
  help <- rendered_help()
  phrases <- c(
    lock_timeout = "The session waits at most 60 s for one lock.",
    empty_skip = "table whose data.table is 'NULL' or has no rows gets no '\\copy'.",
    fallback = "With another driver, or with tables that have different connection settings, csdb calls '$insert_data()' on each table in turn.",
    row_count = "A difference raises an error before the commit, so the server stores no row in any table.",
    exception = "The one exception is a duplicate key with 'confirm_insert_via_nrow = TRUE': csdb then inserts table by table."
  )
  for (p in names(phrases)) {
    expect_true(
      grepl(phrases[[p]], help, fixed = TRUE),
      label = paste0("the help holds the ", p, " phrase \"", phrases[[p]], "\"")
    )
  }
})

test_that("NEWS.md states the rules of the session", {
  news <- news_text()
  phrases <- c(
    lock_timeout = "It waits at most 60 s for one lock",
    empty_skip = "The session skips a table with no rows",
    fallback = "With another driver, or with tables that have different connection settings, it calls `$insert_data()` on each table in turn.",
    row_count = "checks the row count of each table before the commit",
    exception = "The one exception is a duplicate key with `confirm_insert_via_nrow = TRUE`: csdb then inserts table by table."
  )
  for (p in names(phrases)) {
    expect_true(
      grepl(phrases[[p]], news, fixed = TRUE),
      label = paste0("NEWS.md holds the ", p, " phrase \"", phrases[[p]], "\"")
    )
  }
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

test_that("csdb exports insert_data_many()", {
  ns <- readLines(system.file("NAMESPACE", package = "csdb"))
  expect_true("export(insert_data_many)" %in% ns)
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
  # psql 14.24 against PostgreSQL 14.24, the row count check of a session.
  check <- "psql:/tmp/s.sql:10: ERROR:  P0001: csdb: count check failed"
  expect_identical(classify_load_failure("psql", check), "other")
})
