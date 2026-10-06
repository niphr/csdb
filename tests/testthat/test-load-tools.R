# The PostgreSQL and SQL Server load methods hand the data to an external
# client: psql for PostgreSQL, bcp for SQL Server. Until 2026.10.3 both methods
# ignored what the client returned, so a load the server refused lost its rows
# and raised nothing. From 2026.10.3 a failure is classified: a transient one
# is retried, at most 3 attempts in total, and every other one stops.
#
# The tests in the first part put a fake psql and a fake bcp first on PATH and
# call the real S7 methods. A SQLite connection answers dbListFields() and
# dbQuoteIdentifier(), so no database server is needed. The fakes are POSIX
# shell scripts. On Windows Sys.which() finds psql.exe in Rtools before any
# script, so those tests skip there and run in CI on Linux.
#
# The tests in the second part replace run_load_tool() or load_data_infile()
# with a mock, so they also run on Windows.

pg_method <- function() {
  return(S7::method(load_data_infile, db_postgres))
}

ms_method <- function() {
  return(S7::method(load_data_infile, db_mssql))
}

load_tools_secret <- "s3cret-Pw"

load_tools_dbconfig <- function(password = load_tools_secret) {
  return(list(
    server = "fakehost",
    port = 5432,
    db = "fakedb",
    user = "fakeuser",
    password = password,
    trusted_connection = "no"
  ))
}

# A SQLite database with one empty table `tab (id, x)`.
load_tools_connection <- function(.local_envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  DBI::dbWriteTable(con, "tab", data.frame(id = integer(), x = character()))
  return(con)
}

load_tools_dt <- function() {
  return(data.table::data.table(id = 1:2, x = c("a", "b")))
}

# Each fake reads its mode from a comma-separated list: the first call uses
# the first entry, the second call the second, and the last entry repeats.
# FAKE_MODE drives psql and the bcp `in` call. FAKE_FORMAT_MODE drives the
# bcp `format` call. The error texts are the ones psql 14.24 and bcp 17.11
# printed against PostgreSQL 16 and SQL Server 2022 on 2026-10-03. The
# deadlock, the query timeout, the lock timeout and the communication link
# failure are the documented SQL Server messages instead. A real bcp never
# returned after its connection broke mid-transfer, so no real text exists
# for that case. Like psql 14.24, the fake psql prints the SQLSTATE only when
# its arguments hold VERBOSITY=verbose.
fake_tool_script <- c(
  "#!/bin/sh",
  "name=$(basename \"$0\")",
  "{ printf '%s\\n' \"$name\"; for a in \"$@\"; do printf '%s\\n' \"$a\"; done; printf '%s\\n' '---'; } >> \"$FAKE_LOG\"",
  "pick() {",
  "  n=$(( $(cat \"$2\" 2>/dev/null || echo 0) + 1 )); echo \"$n\" > \"$2\"",
  "  m=$(printf '%s\\n' \"$1\" | cut -s -d, -f\"$n\")",
  "  if [ -z \"$m\" ]; then m=$(printf '%s\\n' \"$1\" | awk -F, '{print $NF}'); fi",
  "  printf '%s' \"$m\"",
  "}",
  "bcp_refused() {",
  "  echo 'SQLState = S1T00, NativeError = 0'",
  "  echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Login timeout expired'",
  "  echo 'SQLState = 08001, NativeError = 10057'",
  "  echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]TCP Provider: Error code 0x2749'",
  "}",
  "if [ \"$name\" = psql ]; then",
  "  mode=$(pick \"$FAKE_MODE\" \"$FAKE_LOG.psql.n\")",
  "  sql=''; prev=''",
  "  for a in \"$@\"; do if [ \"$prev\" = '-c' ]; then sql=$a; fi; prev=$a; done",
  "  file=$(printf '%s' \"$sql\" | sed -n \"s/.* from '\\(.*\\)' (FORMAT .*/\\1/p\" | sed \"s/''/'/g\")",
  "  n=$(wc -l < \"$file\" | tr -d ' ')",
  "  verbose=no",
  "  for a in \"$@\"; do if [ \"$a\" = 'VERBOSITY=verbose' ]; then verbose=yes; fi; done",
  "  err() {",
  "    if [ \"$verbose\" = yes ]; then echo \"$1\" >&2",
  "    else echo \"$1\" | sed -E 's/^([A-Z]+:  )[0-9A-Z]{5}: /\\1/' >&2; fi",
  "  }",
  "  case \"$mode\" in",
  "    ok) echo \"COPY $n\"; exit 0 ;;",
  "    dupkey)",
  "      err 'ERROR:  23505: duplicate key value violates unique constraint \"pk_tab\"'",
  "      echo 'DETAIL:  Key (id)=(1) already exists.' >&2",
  "      echo 'CONTEXT:  COPY tab, line 1' >&2",
  "      echo 'LOCATION:  _bt_check_unique, nbtinsert.c:666' >&2",
  "      exit 1 ;;",
  "    dupkey_no) err 'FEIL:  23505: duplisert nokkelverdi bryter med unik-skranke \"pk_tab\"'; exit 1 ;;",
  "    refused)",
  "      echo 'psql: error: connection to server at \"fakehost\" (10.0.0.9), port 5432 failed: Connection refused' >&2",
  "      echo '        Is the server running on that host and accepting TCP/IP connections?' >&2",
  "      exit 2 ;;",
  "    lock) err 'ERROR:  55P03: canceling statement due to lock timeout'; exit 1 ;;",
  "    lost)",
  "      echo 'server closed the connection unexpectedly' >&2",
  "      echo '        This probably means the server terminated abnormally' >&2",
  "      echo '        before or while processing the request.' >&2",
  "      err 'FATAL:  57P01: terminating connection due to administrator command'",
  "      echo 'CONTEXT:  COPY tab, line 1' >&2",
  "      echo 'connection to server was lost' >&2",
  "      exit 2 ;;",
  "    admin_code) err 'FATAL:  57P01: avslutter tilkobling etter kommando fra administrator'; exit 2 ;;",
  "    lost_plain)",
  "      echo 'could not receive data from server: Connection reset by peer' >&2",
  "      echo 'connection to server was lost' >&2",
  "      exit 2 ;;",
  "    connclosed)",
  "      echo 'psql: error: connection to server at \"fakehost\" (10.0.0.9), port 5432 failed: server closed the connection unexpectedly' >&2",
  "      exit 2 ;;",
  "    toomany_code) err 'FATAL:  53300: beklager, for mange klienter allerede'; exit 2 ;;",
  "    toomany)",
  "      echo 'psql: error: connection to server at \"fakehost\" (10.0.0.9), port 5432 failed: FATAL:  sorry, too many clients already' >&2",
  "      exit 2 ;;",
  "    badpw)",
  "      echo 'psql: error: connection to server at \"fakehost\" (10.0.0.9), port 5432 failed: FATAL:  password authentication failed for user \"fakeuser\"' >&2",
  "      exit 2 ;;",
  "    other) err 'ERROR:  22P02: invalid input syntax for type integer: \"x\"'; exit 1 ;;",
  "    short) echo \"COPY $((n - 1))\"; exit 0 ;;",
  "    silent) exit 0 ;;",
  "    echo_secret)",
  "      for a in \"$@\"; do echo \"psql: error: $a\" >&2; done",
  "      echo \"psql: error: PGPASSWORD=$PGPASSWORD\" >&2",
  "      exit 2 ;;",
  "  esac",
  "  exit 99",
  "fi",
  "fmt=''; prev=''",
  "for a in \"$@\"; do if [ \"$prev\" = '-f' ]; then fmt=$a; fi; prev=$a; done",
  "if [ \"$2\" = format ]; then",
  "  mode=$(pick \"${FAKE_FORMAT_MODE:-ok}\" \"$FAKE_LOG.format.n\")",
  "  case \"$mode\" in",
  "    killed) exit 137 ;;",
  "    refused) bcp_refused; exit 1 ;;",
  "    fail)",
  "      echo 'SQLState = S0002, NativeError = 208'",
  "      echo \"Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Invalid object name 'tab'.\"",
  "      exit 1 ;;",
  "  esac",
  "  case \"$3\" in",
  "    /*) : > \"$3\"; echo '14.0' > \"$fmt\"; exit 0 ;;",
  "    *)",
  "      echo 'SQLState = S1000, NativeError = 0'",
  "      echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Unable to open BCP host data-file'",
  "      exit 1 ;;",
  "  esac",
  "fi",
  "if [ ! -f \"$fmt\" ]; then",
  "  echo 'SQLState = S1000, NativeError = 0'",
  "  echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Format file could not be opened'",
  "  exit 1",
  "fi",
  "mode=$(pick \"$FAKE_MODE\" \"$FAKE_LOG.in.n\")",
  "n=$(wc -l < \"$3\" | tr -d ' ')",
  "case \"$mode\" in",
  "  ok) echo; echo 'Starting copy...'; echo; echo \"$n rows copied.\"; echo \"$n\" >> \"$FAKE_STORE\"; exit 0 ;;",
  "  refused) bcp_refused; exit 1 ;;",
  "  dupkey)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 23000, NativeError = 2627'",
  "    echo \"Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Violation of PRIMARY KEY constraint 'PK_tab'. Cannot insert duplicate key in object 'dbo.tab'. The duplicate key value is (1).\"",
  "    echo 'SQLState = 01000, NativeError = 3621'",
  "    echo 'Warning = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]The statement has been terminated.'",
  "    echo; echo 'BCP copy in failed'",
  "    exit 1 ;;",
  "  deadlock)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 40001, NativeError = 1205'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Transaction (Process ID 55) was deadlocked on lock resources with another process and has been chosen as the deadlock victim. Rerun the transaction.'",
  "    echo; echo 'BCP copy in failed'",
  "    exit 1 ;;",
  "  querytimeout)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = HYT00, NativeError = 0'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Query timeout expired'",
  "    exit 1 ;;",
  "  linkfail)",
  "    echo; echo 'Starting copy...'",
  "    echo '1000 rows sent to SQL Server. Total sent: 1000'",
  "    echo 'SQLState = 08S01, NativeError = 10054'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]TCP Provider: Error code 0x2746'",
  "    echo 'SQLState = 08S01, NativeError = 10054'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Communication link failure'",
  "    echo; echo 'BCP copy in failed'",
  "    exit 1 ;;",
  "  killstate)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = S1000, NativeError = 596'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Cannot continue the execution because the session is in the kill state.'",
  "    echo 'SQLState = S1000, NativeError = 0'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Unspecified error occurred on SQL Server. Connection may have been terminated by the server.'",
  "    echo; echo 'BCP copy in failed'",
  "    exit 1 ;;",
  "  snapshot)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 40001, NativeError = 3960'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Snapshot isolation transaction aborted due to update conflict.'",
  "    exit 1 ;;",
  "  deadlock_link)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 40001, NativeError = 1205'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Transaction (Process ID 55) was deadlocked on lock resources with another process and has been chosen as the deadlock victim. Rerun the transaction.'",
  "    echo 'SQLState = 08S01, NativeError = 10054'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]Communication link failure'",
  "    exit 1 ;;",
  "  locktimeout)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 37000, NativeError = 1222'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Lock request time out period exceeded.'",
  "    exit 1 ;;",
  "  badpw)",
  "    echo 'SQLState = 28000, NativeError = 18456'",
  "    echo \"Error = [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Login failed for user 'fakeuser'.\"",
  "    exit 1 ;;",
  "  partial)",
  "    echo; echo 'Starting copy...'",
  "    echo 'SQLState = 22001, NativeError = 0'",
  "    echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]String data, right truncation'",
  "    echo; echo \"$n rows copied.\"; exit 0 ;;",
  "  short) echo; echo 'Starting copy...'; echo; echo \"$((n - 1)) rows copied.\"; exit 0 ;;",
  "  silent) exit 0 ;;",
  "  echo_secret) for a in \"$@\"; do echo \"Error = $a\"; done; exit 1 ;;",
  "  progress)",
  "    echo; echo 'Starting copy...'",
  "    i=1",
  "    while [ $i -le 3000 ]; do",
  "      echo \"1000 rows sent to SQL Server. Total sent: $((i * 1000))\"",
  "      if [ $i -eq 1500 ]; then",
  "        echo 'SQLState = 22001, NativeError = 0'",
  "        echo 'Error = [Microsoft][ODBC Driver 17 for SQL Server]String data, right truncation'",
  "      fi",
  "      i=$((i + 1))",
  "    done",
  "    echo; echo 'BCP copy in failed'",
  "    exit 1 ;;",
  "esac",
  "exit 99"
)

# Put a fake psql and a fake bcp first on PATH. Returns the paths of the
# argument log and of the file where the fake bcp records the rows it stored.
local_fake_tools <- function(
  mode,
  format = "ok",
  .local_envir = parent.frame()
) {
  skip_on_os("windows")
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  for (name in c("psql", "bcp")) {
    path <- file.path(dir, name)
    writeLines(fake_tool_script, path)
    Sys.chmod(path, "0755")
  }
  log <- file.path(dir, "argv.log")
  store <- file.path(dir, "stored.log")
  file.create(c(log, store))
  withr::local_envvar(
    PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
    FAKE_MODE = mode,
    FAKE_FORMAT_MODE = format,
    FAKE_LOG = log,
    FAKE_STORE = store,
    .local_envir = .local_envir
  )
  return(list(log = log, store = store))
}

# The arguments of every call the fakes received, one character vector each.
fake_tool_calls <- function(log) {
  lines <- readLines(log)
  ends <- which(lines == "---")
  starts <- c(1, utils::head(ends, -1) + 1)
  return(Map(function(s, e) lines[s:(e - 1)], starts, ends))
}

# The calls of one kind: "psql", "format" or "in".
fake_calls_of <- function(log, kind) {
  calls <- fake_tool_calls(log)
  keep <- vapply(
    calls,
    function(x) {
      if (kind == "psql") {
        return(x[[1]] == "psql")
      }
      return(x[[1]] == "bcp" && length(x) >= 3 && x[[3]] == kind)
    },
    logical(1)
  )
  return(calls[keep])
}

# Record every wait between attempts instead of sleeping. A csdb without
# retries has no load_retry_wait(), and there the mock is left out, so a test
# fails on the missing retry and not on a missing function.
local_recorded_waits <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$waits <- numeric()
  ns <- asNamespace("csdb")
  if (exists("load_retry_wait", envir = ns, inherits = FALSE)) {
    local_mocked_bindings(
      load_retry_wait = function(seconds) {
        rec$waits <- c(rec$waits, seconds)
        return(invisible(NULL))
      },
      .env = .local_envir
    )
  }
  return(rec)
}

# Run `expr`, and return its error, its message text, and every message and
# warning it emitted.
capture_load_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  error <- withCallingHandlers(
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  return(list(
    error = error,
    message = if (is.null(error)) NA_character_ else conditionMessage(error),
    warnings = warnings,
    messages = messages
  ))
}

# RSQLite quotes an identifier with backticks. PostgreSQL quotes with double
# quotes, so the mock below does too. It quotes a DBI::Id as one name, and a
# character vector one element at a time.
pg_quote_identifier <- function(conn, x, ...) {
  if (is.character(x)) {
    return(DBI::SQL(paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")))
  }
  return(DBI::SQL(paste0("\"", x@name, "\"", collapse = ".")))
}

run_pg_load <- function(con, password = load_tools_secret, file = tempfile()) {
  return(with_mocked_bindings(
    pg_method()(
      connection = con,
      dbconfig = load_tools_dbconfig(password),
      table = DBI::Id(table = "tab"),
      dt = load_tools_dt(),
      file = file
    ),
    dbQuoteIdentifier = pg_quote_identifier,
    .package = "DBI"
  ))
}

run_ms_load <- function(
  con,
  password = load_tools_secret,
  dt = load_tools_dt(),
  file = tempfile()
) {
  return(ms_method()(
    connection = con,
    dbconfig = load_tools_dbconfig(password),
    table = "tab",
    dt = dt,
    file = file
  ))
}

# Part 1: fake clients on PATH, real methods ---------------------------------

test_that("psql: a COPY the server refuses stops with psql's own error", {
  fake <- local_fake_tools("dupkey")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "table \"tab\"", fixed = TRUE)
  expect_match(got$message, "psql \\copy", fixed = TRUE)
  expect_match(got$message, "exit status was 1", fixed = TRUE)
  expect_match(
    got$message,
    "duplicate key value violates unique constraint \"pk_tab\"",
    fixed = TRUE
  )
  expect_match(got$message, "Key (id)=(1) already exists.", fixed = TRUE)
})

test_that("psql: a duplicate key stops after 1 attempt, classed as such", {
  fake <- local_fake_tools("dupkey")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_s3_class(got$error, "csdb_load_error")
  expect_match(
    got$message,
    "The failure class is duplicate key, after 1 attempt.",
    fixed = TRUE
  )
  expect_length(fake_calls_of(fake$log, "psql"), 1)
  expect_identical(rec$waits, numeric())
  expect_match(
    got$messages,
    "table \"tab\" with psql \\copy failed on attempt 1 of at most 3 (duplicate key)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("psql: SQLSTATE 23505 is a duplicate key in any server language", {
  fake <- local_fake_tools("dupkey_no")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: a refused connection is retried, and attempt 2 succeeds", {
  fake <- local_fake_tools("refused,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "psql"), 2)
  expect_identical(rec$waits, 1)
  expect_match(
    got$messages,
    "table \"tab\" with psql \\copy failed on attempt 1 of at most 3 (transient). Retrying in 1 s.",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(got$messages, "Connection refused", fixed = TRUE, all = FALSE)
  expect_match(
    got$messages,
    "table \"tab\" with psql \\copy succeeded on attempt 2 of at most 3.",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("psql: three transient failures stop after exactly 3 attempts", {
  fake <- local_fake_tools("refused")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_match(
    got$message,
    "The failure class is transient, after 3 attempts.",
    fixed = TRUE
  )
  expect_match(got$message, "Connection refused", fixed = TRUE)
  expect_length(fake_calls_of(fake$log, "psql"), 3)
  expect_identical(rec$waits, c(1, 3))
  expect_identical(sum(rec$waits), 4)
  expect_length(grep("failed on attempt", got$messages, fixed = TRUE), 3)
})

test_that("psql: a lock timeout (SQLSTATE 55P03) is transient", {
  fake <- local_fake_tools("lock,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "psql"), 2)
  expect_identical(rec$waits, 1)
})

test_that("psql: too many connections (SQLSTATE 53300) is transient", {
  # A server language other than English: only the SQLSTATE shows the class.
  fake <- local_fake_tools("toomany_code")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_length(fake_calls_of(fake$log, "psql"), 3)
  expect_identical(rec$waits, c(1, 3))
})

test_that("psql: 'sorry, too many clients already' at connection is transient", {
  # psql 14.24 printed this line, with no SQLSTATE, against PostgreSQL 16 with
  # every connection slot taken (2026-10-03).
  fake <- local_fake_tools("toomany")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_length(fake_calls_of(fake$log, "psql"), 3)
  expect_match(got$message, "sorry, too many clients already", fixed = TRUE)
})

test_that("psql: a connection lost mid-COPY stops after 1 attempt, ambiguous", {
  # The text psql 14.24 printed when pg_terminate_backend() ended a running
  # COPY on PostgreSQL 16 (2026-10-03).
  fake <- local_fake_tools("lost,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "psql"), 1)
  expect_identical(rec$waits, numeric())
  expect_match(
    got$message,
    "may or may not have committed the rows to table \"tab\".",
    fixed = TRUE
  )
  expect_match(
    got$messages,
    "with psql \\copy failed on attempt 1 of at most 3 (ambiguous)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("psql: a connection lost mid-COPY without a SQLSTATE is ambiguous", {
  fake <- local_fake_tools("lost_plain,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: SQLSTATE 57P01 alone is ambiguous", {
  # A server language other than English, and no lost-connection text.
  fake <- local_fake_tools("admin_code,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: a password equal to the SQLSTATE does not hide the class", {
  fake <- local_fake_tools("dupkey")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con, password = "23505"))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
})

test_that("psql: a connection closed while it starts is retried", {
  fake <- local_fake_tools("connclosed,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "psql"), 2)
  expect_identical(rec$waits, 1)
})

test_that("psql: a password equal to the row count does not hide the count", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con, password = "2"))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: another error stops after 1 attempt", {
  fake <- local_fake_tools("other")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_other")
  expect_match(got$message, "22P02: invalid input syntax", fixed = TRUE)
  expect_length(fake_calls_of(fake$log, "psql"), 1)
  expect_identical(rec$waits, numeric())
})

test_that("psql: a rejected password is not retried", {
  fake <- local_fake_tools("badpw")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_s3_class(got$error, "csdb_load_other")
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: a COPY that reports fewer rows than csdb sent stops", {
  fake <- local_fake_tools("short")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "stored 1 rows, and csdb sent 2 rows", fixed = TRUE)
  expect_length(fake_calls_of(fake$log, "psql"), 1)
})

test_that("psql: a COPY that reports no row count stops", {
  fake <- local_fake_tools("silent")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "did not report how many rows", fixed = TRUE)
})

test_that("psql: the password reaches neither the error nor a warning", {
  fake <- local_fake_tools("echo_secret")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_false(is.na(got$message))
  # The fake echoed every argument and PGPASSWORD. The URI holds no
  # password, and the value of PGPASSWORD is redacted.
  expect_match(
    got$message,
    "postgresql://fakeuser@fakehost:5432/fakedb?connect_timeout=30",
    fixed = TRUE
  )
  expect_match(got$message, "PGPASSWORD=***", fixed = TRUE)
  expect_false(grepl(load_tools_secret, got$message, fixed = TRUE))
  expect_false(any(grepl(load_tools_secret, got$warnings, fixed = TRUE)))
  expect_false(any(grepl(load_tools_secret, got$messages, fixed = TRUE)))
})

test_that("psql: the arguments hold the copy command as one element and no password", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  withr::local_envvar(PGPASSWORD = "outer-value")
  file <- file.path(withr::local_tempdir(), "it's a file.tsv")
  got <- capture_load_conditions(run_pg_load(con, file = file))
  expect_null(got$error)
  calls <- fake_calls_of(fake$log, "psql")
  expect_length(calls, 1)
  copy <- sprintf(
    "\\copy \"tab\" (\"id\", \"x\") from '%s' (FORMAT CSV, DELIMITER '\t')",
    gsub("'", "''", file, fixed = TRUE)
  )
  expect_identical(
    calls[[1]],
    c(
      "psql",
      "-v",
      "VERBOSITY=verbose",
      "-U",
      "fakeuser",
      "-c",
      copy,
      "postgresql://fakeuser@fakehost:5432/fakedb?connect_timeout=30"
    )
  )
  expect_false(any(grepl(load_tools_secret, calls[[1]], fixed = TRUE)))
  # The load set PGPASSWORD for psql only.
  expect_identical(Sys.getenv("PGPASSWORD"), "outer-value")
})

test_that("psql: the user, the host and the database are URL-encoded", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  dbconfig <- load_tools_dbconfig()
  dbconfig$user <- "u@x:y/z"
  dbconfig$server <- "fake host"
  dbconfig$db <- "db/1?x"
  got <- capture_load_conditions(with_mocked_bindings(
    pg_method()(
      connection = con,
      dbconfig = dbconfig,
      table = DBI::Id(table = "tab"),
      dt = load_tools_dt(),
      file = tempfile()
    ),
    dbQuoteIdentifier = pg_quote_identifier,
    .package = "DBI"
  ))
  expect_null(got$error)
  call <- fake_calls_of(fake$log, "psql")[[1]]
  expect_identical(
    call[[length(call)]],
    "postgresql://u%40x%3Ay%2Fz@fake%20host:5432/db%2F1%3Fx?connect_timeout=30"
  )
  expect_identical(call[[5]], "u@x:y/z")
})

test_that("psql: a COPY that stores every row returns without an error", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_pg_load(con))
  expect_identical(got$message, NA_character_)
  expect_identical(got$warnings, character())
  expect_identical(got$messages, character())
  expect_length(fake_tool_calls(fake$log), 1)
})

test_that("bcp: the format call writes nothing to the working directory", {
  # The fake bcp answers a relative data-file name the way bcp on Linux does
  # in a working directory it cannot write: exit 1, no format file. Given an
  # absolute name, it creates that file empty, as bcp on Linux does.
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_identical(got$message, NA_character_)
  expect_identical(readLines(fake$store), "2")

  format_call <- fake_calls_of(fake$log, "format")
  expect_length(format_call, 1)
  data_file <- format_call[[1]][[4]]
  expect_false(identical(data_file, "nul"))
  expect_true(startsWith(
    normalizePath(dirname(data_file), mustWork = FALSE),
    normalizePath(tempdir())
  ))
  # The data-file name is deleted after the load.
  expect_false(file.exists(data_file))
})

test_that("bcp: a format call that fails stops before the load", {
  fake <- local_fake_tools("ok", format = "fail")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "bcp format", fixed = TRUE)
  expect_match(got$message, "exit status was 1", fixed = TRUE)
  expect_match(got$message, "Invalid object name 'tab'", fixed = TRUE)
  # The load itself never started.
  calls <- fake_tool_calls(fake$log)
  expect_length(calls, 1)
  expect_identical(calls[[1]][[3]], "format")
})

test_that("bcp: a format call killed with no output stops on its status", {
  # Exit status 137 is SIGKILL. Only the status shows the failure here: there
  # is no error line, and the format call reports no row count.
  fake <- local_fake_tools("ok", format = "killed")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_match(
    got$message,
    "bcp format returned a non-zero exit status. Its exit status was 137.",
    fixed = TRUE
  )
  expect_length(fake_tool_calls(fake$log), 1)
})

test_that("bcp: a refused connection on the format call is retried", {
  fake <- local_fake_tools("ok", format = "refused,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "format"), 2)
  expect_length(fake_calls_of(fake$log, "in"), 1)
  expect_identical(readLines(fake$store), "2")
  expect_identical(rec$waits, 1)
  expect_match(
    got$messages,
    "table tab with bcp format failed on attempt 1 of at most 3 (transient)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("bcp: a refused connection on the load is retried, and attempt 2 succeeds", {
  fake <- local_fake_tools("refused,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "in"), 2)
  expect_identical(readLines(fake$store), "2")
  expect_identical(rec$waits, 1)
  expect_match(
    got$messages,
    "table tab with bcp in succeeded on attempt 2 of at most 3.",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("bcp: three deadlocks stop after exactly 3 attempts", {
  fake <- local_fake_tools("deadlock")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_transient")
  expect_match(
    got$message,
    "The failure class is transient, after 3 attempts.",
    fixed = TRUE
  )
  expect_match(got$message, "deadlocked on lock resources", fixed = TRUE)
  expect_length(fake_calls_of(fake$log, "in"), 3)
  expect_identical(rec$waits, c(1, 3))
})

test_that("bcp: a query timeout (HYT00) after Starting copy is ambiguous", {
  fake <- local_fake_tools("querytimeout,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "in"), 1)
  expect_identical(rec$waits, numeric())
})

test_that("bcp: a communication link failure after Starting copy is ambiguous", {
  fake <- local_fake_tools("linkfail,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "in"), 1)
  expect_identical(rec$waits, numeric())
  expect_match(
    got$message,
    "may or may not have committed the rows to table tab.",
    fixed = TRUE
  )
  expect_match(got$message, "Communication link failure", fixed = TRUE)
  expect_match(
    got$message,
    "If table tab has no primary key and you run the load again by hand, the table can get duplicate rows.",
    fixed = TRUE
  )
  expect_match(
    got$messages,
    "table tab with bcp in failed on attempt 1 of at most 3 (ambiguous)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("bcp: a session killed after Starting copy (NativeError 596) is ambiguous", {
  # The text bcp 17.11 printed when KILL ended its session on SQL Server 2022
  # during a 3,000,000-row load (2026-10-03).
  fake <- local_fake_tools("killstate,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "in"), 1)
  expect_match(got$message, "session is in the kill state", fixed = TRUE)
})

test_that("bcp: a serialization failure (SQLState 40001) is transient", {
  fake <- local_fake_tools("snapshot,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "in"), 2)
})

test_that("bcp: a link failure after Starting copy is ambiguous even with a deadlock", {
  fake <- local_fake_tools("deadlock_link,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(fake_calls_of(fake$log, "in"), 1)
})

test_that("bcp: a password equal to the row count does not hide the count", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con, password = "2"))
  expect_null(got$error)
  expect_identical(readLines(fake$store), "2")
})

test_that("bcp: a password equal to 'Error' does not hide an error line", {
  fake <- local_fake_tools("partial")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con, password = "Error"))
  expect_s3_class(got$error, "csdb_load_other")
  expect_match(got$message, "bcp in reported an error", fixed = TRUE)
})

test_that("bcp: a lock timeout (NativeError 1222) is transient", {
  fake <- local_fake_tools("locktimeout,ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_null(got$error)
  expect_length(fake_calls_of(fake$log, "in"), 2)
})

test_that("bcp: a duplicate primary key stops with bcp's own message", {
  fake <- local_fake_tools("dupkey")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_match(got$message, "table tab", fixed = TRUE)
  expect_match(got$message, "bcp in", fixed = TRUE)
  expect_match(got$message, "exit status was 1", fixed = TRUE)
  expect_match(
    got$message,
    "Violation of PRIMARY KEY constraint 'PK_tab'",
    fixed = TRUE
  )
  expect_match(got$message, "BCP copy in failed", fixed = TRUE)
  expect_length(fake_calls_of(fake$log, "in"), 1)
  expect_identical(rec$waits, numeric())
})

test_that("bcp: a failed login is not retried", {
  fake <- local_fake_tools("badpw")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_other")
  expect_length(fake_calls_of(fake$log, "in"), 1)
})

test_that("bcp: an error line stops the load when bcp exits with 0", {
  fake <- local_fake_tools("partial")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "bcp in reported an error", fixed = TRUE)
  expect_match(got$message, "exit status was 0", fixed = TRUE)
  expect_match(got$message, "String data, right truncation", fixed = TRUE)
})

test_that("bcp: a row count below the rows csdb sent stops the load", {
  fake <- local_fake_tools("short")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "stored 1 rows, and csdb sent 2 rows", fixed = TRUE)
})

test_that("bcp: a load that reports no row count stops", {
  fake <- local_fake_tools("silent")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  expect_match(got$message, "did not report how many rows", fixed = TRUE)
})

test_that("bcp: the password reaches neither the error nor a warning", {
  fake <- local_fake_tools("echo_secret")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_false(is.na(got$message))
  # The fake echoed every argument, so "-P" and the password were in it.
  expect_match(got$message, "Error = -P", fixed = TRUE)
  expect_false(grepl(load_tools_secret, got$message, fixed = TRUE))
  expect_false(any(grepl(load_tools_secret, got$warnings, fixed = TRUE)))
  expect_false(any(grepl(load_tools_secret, got$messages, fixed = TRUE)))
})

test_that("bcp: every option and its value arrive as separate arguments", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  dt <- load_tools_dt()
  data.table::setkey(dt, id)
  got <- capture_load_conditions(run_ms_load(con, dt = dt))
  expect_null(got$error)
  call <- fake_calls_of(fake$log, "in")[[1]]
  expect_identical(call[match("-a", call) + 1L], "16384")
  expect_identical(call[match("-h", call) + 1L], "ORDER(id ASC)")
  expect_identical(call[match("-P", call) + 1L], load_tools_secret)
})

test_that("bcp: 3000 progress lines leave the message short, with the error line", {
  fake <- local_fake_tools("progress")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_s3_class(got$error, "csdb_load_error")
  lines <- strsplit(got$message, "\n", fixed = TRUE)[[1]]
  expect_lte(length(lines), 60)
  expect_true(any(grepl(
    "^Error = .*String data, right truncation$",
    lines
  )))
  expect_false(any(grepl("rows sent to SQL Server", lines, fixed = TRUE)))
  for (m in got$messages) {
    expect_lte(length(strsplit(m, "\n", fixed = TRUE)[[1]]), 60)
  }
})

test_that("bcp: a load that stores every row returns without an error", {
  fake <- local_fake_tools("ok")
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  got <- capture_load_conditions(run_ms_load(con))
  expect_identical(got$message, NA_character_)
  expect_identical(got$warnings, character())
  expect_identical(got$messages, character())
  expect_identical(readLines(fake$store), "2")
})

# Part 2: mocks, on every OS -------------------------------------------------

# The methods call Sys.which() before they run a client, and stop when it
# finds nothing. Put an executable of each name first on PATH so that check
# passes. It never runs, because run_load_tool() is mocked. Sys.which() on
# Windows looks for a .exe, so a copy of Rscript.exe stands in there.
local_stand_in_tools <- function(.local_envir = parent.frame()) {
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

test_that("mocked psql: a refused COPY stops, and a full COPY does not", {
  local_stand_in_tools()
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      return(list(
        status = 1L,
        output = "ERROR:  23505: duplicate key value violates unique constraint"
      ))
    }
  )
  expect_error(
    suppressMessages(run_pg_load(con)),
    "duplicate key value",
    fixed = TRUE
  )

  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      return(list(status = 0L, output = "COPY 2"))
    }
  )
  expect_no_error(run_pg_load(con))
})

test_that("mocked psql: a transient failure, then success, is retried once", {
  local_stand_in_tools()
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  n <- 0L
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      n <<- n + 1L
      if (n == 1L) {
        return(list(status = 2L, output = "psql: error: Connection refused"))
      }
      return(list(status = 0L, output = "COPY 2"))
    }
  )
  got <- capture_load_conditions(run_pg_load(con))
  expect_null(got$error)
  expect_match(
    got$messages,
    "succeeded on attempt 2",
    fixed = TRUE,
    all = FALSE
  )
  expect_identical(n, 2L)
  expect_identical(rec$waits, 1)
})

test_that("mocked bcp: a short row count stops, and a full one does not", {
  local_stand_in_tools()
  rec <- local_recorded_waits()
  con <- load_tools_connection()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      if (args[[2]] == "format") {
        return(list(status = 0L, output = character()))
      }
      return(list(status = 0L, output = "1 rows copied."))
    }
  )
  expect_error(
    suppressMessages(run_ms_load(con)),
    "stored 1 rows, and csdb sent 2",
    fixed = TRUE
  )

  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      if (args[[2]] == "format") {
        return(list(status = 0L, output = character()))
      }
      return(list(status = 0L, output = "2 rows copied."))
    }
  )
  expect_no_error(run_ms_load(con))
})

# Record the arguments that csdb builds for every client call, and answer
# each call as a load that stored both rows.
local_recorded_args <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    run_load_tool = function(command, args, timeout = 0, env = NULL) {
      rec$calls[[length(rec$calls) + 1L]] <- list(
        command = command,
        args = as.character(args),
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

test_that("mocked bcp: csdb builds each option and its value as separate elements", {
  local_stand_in_tools()
  rec <- local_recorded_args()
  con <- load_tools_connection()
  dt <- load_tools_dt()
  data.table::setkey(dt, id, x)
  run_ms_load(con, dt = dt)
  expect_length(rec$calls, 2L)
  args <- rec$calls[[2]]$args
  expect_identical(args[[2]], "in")
  expect_identical(args[which(args == "-a") + 1L], "16384")
  expect_identical(args[which(args == "-h") + 1L], "ORDER(id ASC, x ASC)")
  expect_false(any(grepl("'", args, fixed = TRUE)))
})

test_that("mocked psql: the copy command quotes the columns and the path, and the URI is encoded", {
  local_stand_in_tools()
  rec <- local_recorded_args()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(
    con,
    "tab",
    data.frame(id = integer(), `a b` = character(), check.names = FALSE)
  )
  dbconfig <- load_tools_dbconfig()
  dbconfig$user <- "u@x:y/z"
  dbconfig$server <- "fake host"
  dbconfig$db <- "db/1?x"
  file <- file.path(withr::local_tempdir(), "it's.tsv")
  with_mocked_bindings(
    pg_method()(
      connection = con,
      dbconfig = dbconfig,
      table = DBI::Id(table = "tab"),
      dt = data.table::data.table(id = 1:2, `a b` = c("p", "q")),
      file = file
    ),
    dbQuoteIdentifier = pg_quote_identifier,
    .package = "DBI"
  )
  expect_length(rec$calls, 1L)
  args <- rec$calls[[1]]$args
  copy <- args[which(args == "-c") + 1L]
  # psql MUST receive the bare command. No shell strips quotes any more.
  expect_true(startsWith(copy, "\\copy "))
  expect_false(startsWith(copy, "\""))
  expect_false(endsWith(copy, "\""))
  expect_match(copy, "(\"id\", \"a b\")", fixed = TRUE)
  expect_match(copy, "/it''s.tsv' (FORMAT CSV", fixed = TRUE)
  expect_identical(
    copy,
    sprintf(
      "\\copy \"tab\" (\"id\", \"a b\") from '%s' (FORMAT CSV, DELIMITER '\t')",
      gsub("'", "''", file, fixed = TRUE)
    )
  )
  expect_identical(
    args[[length(args)]],
    "postgresql://u%40x%3Ay%2Fz@fake%20host:5432/db%2F1%3Fx?connect_timeout=30"
  )
  expect_identical(args[which(args == "-U") + 1L], "u@x:y/z")
  expect_identical(rec$calls[[1]]$env, c(PGPASSWORD = load_tools_secret))
})

test_that("run_load_tool() captures stdout, stderr and the exit status", {
  rscript <- file.path(R.home("bin"), "Rscript")
  expect_no_warning(
    res <- run_load_tool(
      rscript,
      c("-e", "cat('out\\n'); message('err'); quit(status = 3)")
    )
  )
  expect_identical(res$status, 3L)
  expect_true("out" %in% res$output)
  expect_true("err" %in% res$output)
})

test_that("load_output_excerpt() keeps the first 20 and the last 20 lines", {
  out <- c(
    "pw s3cret-Pw",
    sprintf("line %d", 2:100),
    "",
    "   ",
    "5000 rows sent to SQL Server. Total sent: 5000"
  )
  got <- load_output_excerpt(out, secrets = "s3cret-Pw")
  expect_identical(
    strsplit(got, "\n", fixed = TRUE)[[1]],
    c(
      "pw ***",
      sprintf("line %d", 2:20),
      "[csdb omitted 60 lines here]",
      sprintf("line %d", 81:100)
    )
  )
  # 40 lines or fewer are kept whole.
  expect_identical(
    load_output_excerpt(sprintf("line %d", 1:40)),
    paste0(sprintf("line %d", 1:40), collapse = "\n")
  )
})

test_that("load_output_excerpt() keeps an error line among more than 40 other lines", {
  state <- "SQLState = 22001, NativeError = 0"
  error <- "Error = [Microsoft][ODBC Driver 17 for SQL Server]String data, right truncation"
  out <- c(
    sprintf("before %d", 1:50),
    "1000 rows sent to SQL Server. Total sent: 1000",
    state,
    error,
    sprintf("after %d", 1:50)
  )
  got <- strsplit(load_output_excerpt(out), "\n", fixed = TRUE)[[1]]
  expect_identical(
    got,
    c(
      sprintf("before %d", 1:20),
      "[csdb omitted 60 lines here]",
      state,
      error,
      sprintf("after %d", 31:50)
    )
  )
  # 30 identical pairs collapse to one pair, and the error line is redacted.
  out <- c(sprintf("x %d", 1:100), rep(c(state, "  Error = pw s3cret"), 30))
  got <- strsplit(
    load_output_excerpt(out, secrets = "s3cret"),
    "\n",
    fixed = TRUE
  )[[1]]
  expect_identical(
    got,
    c(
      sprintf("x %d", 1:20),
      "[csdb omitted 60 lines here]",
      sprintf("x %d", 81:100),
      state,
      "  Error = pw ***"
    )
  )
})

test_that("load_output_excerpt() keeps the first 20 and the last 20 distinct error lines", {
  out <- c(
    sprintf("x %d", 1:100),
    sprintf("Error = row %d failed", 1:60)
  )
  got <- strsplit(load_output_excerpt(out), "\n", fixed = TRUE)[[1]]
  expect_identical(
    got,
    c(
      sprintf("x %d", 1:20),
      "[csdb omitted 60 lines here]",
      sprintf("x %d", 81:100),
      sprintf("Error = row %d failed", 1:20),
      "[csdb omitted 20 further error lines]",
      sprintf("Error = row %d failed", 41:60)
    )
  )
})

test_that("load_output_excerpt() holds at most 2 * 20 + 1 + 40 + 1 = 82 lines", {
  bound <- 2L * 20L + 1L + 40L + 1L
  # Distinct error lines and other lines, interleaved, at every size.
  for (k in c(0L, 10L, 41L, 100L, 5000L)) {
    out <- rbind(
      sprintf("line %d", seq_len(k)),
      sprintf("SQLState = %05d, NativeError = %d", seq_len(k), seq_len(k)),
      sprintf("Error = row %d failed", seq_len(k)),
      sprintf(
        "%d rows sent to SQL Server. Total sent: %d",
        seq_len(k),
        seq_len(k)
      )
    )
    got <- strsplit(load_output_excerpt(c(out)), "\n", fixed = TRUE)[[1]]
    expect_lte(length(got), bound)
  }
  expect_length(got, bound)
})

# Part 3: insert_data(confirm_insert_via_nrow = TRUE) ------------------------
#
# A SQLite table, and a load_data_infile() whose first call fails the way the
# PostgreSQL and SQL Server methods fail. Every later call is the real one, so
# the upsert fills its staging table for real.

seeded_sqlite_table <- function(.local_envir = parent.frame()) {
  tab <- DBTable_v9$new(
    dbconfig = sqlite_dbconfig(.local_envir = .local_envir),
    table_name = "confirm_tab",
    field_types = c(id = "INTEGER", x = "TEXT"),
    keys = "id"
  )
  suppressMessages(tab$connect())
  withr::defer(tab$disconnect(), envir = .local_envir)
  suppressMessages(
    tab$insert_data(data.table::data.table(id = 1:2, x = c("old1", "old2")))
  )
  return(tab)
}

# The first load fails with an error of class `class`; later loads are real.
local_failing_first_load <- function(class, .local_envir = parent.frame()) {
  real <- load_data_infile
  calls <- new.env()
  calls$n <- 0L
  local_mocked_bindings(
    load_data_infile = function(...) {
      calls$n <- calls$n + 1L
      if (calls$n == 1L) {
        stop(errorCondition(
          "Loading data into table confirm_tab failed (mocked).",
          class = c(class, "csdb_load_error"),
          table = "confirm_tab",
          tool = "psql \\copy",
          attempts = 1L,
          failure_class = "duplicate key"
        ))
      }
      return(real(...))
    },
    .env = .local_envir
  )
  return(calls)
}

table_rows <- function(tab) {
  d <- DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    "SELECT id, x FROM confirm_tab ORDER BY id"
  )
  return(paste(d$id, d$x))
}

test_that("insert_data(confirm_insert_via_nrow = TRUE) upserts once after a duplicate key", {
  tab <- seeded_sqlite_table()
  calls <- local_failing_first_load("csdb_load_duplicate_key")
  got <- capture_load_conditions(
    tab$insert_data(
      data.table::data.table(id = c(1L, 3L), x = c("new1", "new3")),
      confirm_insert_via_nrow = TRUE
    )
  )
  expect_null(got$error)
  expect_identical(table_rows(tab), c("1 new1", "2 old2", "3 new3"))
  # The failed load, then the one load that fills the upsert's staging table.
  expect_identical(calls$n, 2L)
  expect_match(
    got$messages,
    "insert_data(confirm_insert_via_nrow = TRUE) on table confirm_tab: psql \\copy failed on attempt 1 (duplicate key). It upserts the 2 rows once instead.",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data() without confirm_insert_via_nrow stops on a duplicate key", {
  tab <- seeded_sqlite_table()
  calls <- local_failing_first_load("csdb_load_duplicate_key")
  got <- capture_load_conditions(
    tab$insert_data(
      data.table::data.table(id = c(1L, 3L), x = c("new1", "new3"))
    )
  )
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_identical(table_rows(tab), c("1 old1", "2 old2"))
  expect_identical(calls$n, 1L)
})

test_that("insert_data(confirm_insert_via_nrow = TRUE) does not upsert after another error", {
  tab <- seeded_sqlite_table()
  calls <- local_failing_first_load("csdb_load_other")
  got <- capture_load_conditions(
    tab$insert_data(
      data.table::data.table(id = c(1L, 3L), x = c("new1", "new3")),
      confirm_insert_via_nrow = TRUE
    )
  )
  expect_s3_class(got$error, "csdb_load_other")
  expect_identical(table_rows(tab), c("1 old1", "2 old2"))
  expect_identical(calls$n, 1L)
})
