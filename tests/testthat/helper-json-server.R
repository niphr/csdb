# A mocked PostgreSQL server for the JSON load, on a SQLite connection.
#
# json_load_execute() is mocked: it records each statement, and for an INSERT
# it reads the JSON text back with yyjsonr and appends the rows to the SQLite
# table. A statement that `fail` makes fail appends no row. BEGIN, COMMIT and ROLLBACK are the real ones of the SQLite
# connection, so a rollback really removes the rows.
#
# `fail` is a function(table, n) that returns NULL, an error text to raise, or
# a number of stored rows to report: `table` is the bare table name and `n`
# counts the INSERT statements of that table. The returned environment holds
# every statement in `sql`, and in `client` the number of calls to
# run_load_tool(). The mock of run_load_tool() fails, so a test fails when csdb
# starts an external client.
local_json_server <- function(fail = NULL, .local_envir = parent.frame()) {
  rec <- new.env()
  rec$sql <- character()
  rec$counts <- list()
  rec$client <- 0L
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
      outcome <- if (is.null(fail)) NULL else fail(name, rec$counts[[name]])
      if (is.character(outcome)) {
        stop(outcome, call. = FALSE)
      }
      DBI::dbAppendTable(connection, name, rows)
      if (is.numeric(outcome)) {
        return(outcome)
      }
      return(nrow(rows))
    },
    run_load_tool = function(...) {
      rec$client <- rec$client + 1L
      stop("csdb started an external client", call. = FALSE)
    },
    .env = .local_envir
  )
  return(rec)
}

# Make base::system2() and, when processx is loaded, processx::run() record
# each call and then fail, until the calling test ends. csdb does not import
# processx, so processx can only run when another package has loaded it. The
# returned environment holds the number of calls in `n`. csdb starts every
# external client through system2(), so a PostgreSQL load that starts psql
# shows a call here.
local_no_external_process <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$n <- 0L
  tracer <- bquote({
    assign("n", get("n", envir = .(rec)) + 1L, envir = .(rec))
    stop("an external process was started", call. = FALSE)
  })
  suppressMessages(trace(
    "system2",
    tracer = tracer,
    where = baseenv(),
    print = FALSE
  ))
  withr::defer(
    suppressMessages(untrace("system2", where = baseenv())),
    envir = .local_envir
  )
  if (isNamespaceLoaded("processx")) {
    suppressMessages(trace(
      "run",
      tracer = tracer,
      where = asNamespace("processx"),
      print = FALSE
    ))
    withr::defer(
      suppressMessages(untrace("run", where = asNamespace("processx"))),
      envir = .local_envir
    )
  }
  return(rec)
}
