# The JSON load of PostgreSQL.
#
# With DBConnection_v9$new(pg_load = "json"), a PostgreSQL load sends its
# rows as JSON text over the ODBC connection that csdb already holds. It
# starts no psql, so it makes no login. One statement loads one chunk:
#
#   INSERT INTO t (a, b) SELECT a, b
#   FROM json_populate_recordset(NULL::t, CAST(? AS json))
#
# Every chunk of one load, and every table of one insert_data_many(), runs in
# one transaction. Each psql load is a new login, and the Entra token proxy
# runs ALTER ROLE for each login. The phase 34 benchmark (PostgreSQL 18, 80
# plans of 60,000 rows, 4 workers) measured 320 logins in 34.0 s with psql,
# and 8 logins in 32.2 s with this method.
#
# The failure classes are those of run_checked_load(). They fail closed:
#
#   * a statement that fails before the COMMIT, when the ROLLBACK succeeds and
#     the connection then answers SELECT 1: the server stored no row. A
#     duplicate key is "duplicate key", a lock timeout, a deadlock or a
#     serialization failure is "transient", and csdb runs the whole load
#     again. Any other text is "other".
#   * an error of the COMMIT, a connection that breaks, and a ROLLBACK that
#     csdb cannot confirm: "ambiguous". The server may or may not hold rows.
#
# psqlODBC 13.02 under odbc 1.7.0 reports the SQLSTATE 00000 for every error
# (measured on 2026-10-15 against PostgreSQL 18.6). So csdb reads the message
# text of the server, which is English unless the server sets lc_messages to
# another language.

# The most rows that one statement sends.
json_load_chunk_rows <- 500000L

# The most bytes of JSON text that one statement of several rows sends. PostgreSQL refuses a
# value of 1 GB or more: on 2026-10-15 one statement of 5,000,000 rows, 1.66
# GB of JSON, broke the connection. csdb sizes each chunk from the JSON of a
# sample of rows, and it splits a chunk in half while its text is longer.
json_load_max_bytes <- 2^28

# The most bytes of JSON text for one row. A row with a longer text stops the
# load before csdb sends it, because PostgreSQL refuses a value of 1 GB or
# more.
json_load_row_max_bytes <- 900 * 2^20

json_load_tool <- "json_populate_recordset"

# The server texts of the failure classes. The SQLSTATE that each one has in
# a psql load is in the comment.
pg_json_duplicate_text <- "duplicate key value violates unique constraint" # 23505
pg_json_retry_text <- paste0(
  "canceling statement due to lock timeout|could not obtain lock on|", # 55P03
  "deadlock detected|", # 40P01
  "could not serialize access" # 40001
)
pg_json_lost_text <- paste0(
  pg_lost_text,
  "|The connection has been lost|the connection lost|",
  "terminating connection due to|Could not send Query|",
  "no connection to the server"
)

# Load one data.table into each table, in one transaction, and retry a
# transient failure. `tables` holds the DBI::Id or the name of each table, and
# `dts` the data.table of each table, in the same order. `lock_timeout` is
# NULL, or the longest wait in seconds for one lock. `load_timeout` bounds
# each attempt, and 0 means no limit.
json_load_tables <- function(
  connection,
  dbconfig,
  tables,
  dts,
  load_timeout = 3600,
  lock_timeout = NULL,
  max_rows = json_load_chunk_rows,
  max_bytes = json_load_max_bytes,
  max_row_bytes = json_load_row_max_bytes
) {
  plan <- lapply(seq_along(tables), function(i) {
    p <- json_load_plan(connection, tables[[i]], dts[[i]], max_rows, max_bytes)
    p$max_row_bytes <- max_row_bytes
    return(p)
  })
  table_text <- vapply(plan, function(x) x$table_text, character(1))
  max_attempts <- length(retry_waits) + 1L
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    failed <- json_load_attempt(connection, plan, load_timeout, lock_timeout)
    if (is.null(failed)) {
      break
    }
    failure_class <- classify_json_failure(failed)
    retry <- failure_class == "transient" && attempt < max_attempts
    json_load_report(
      failed,
      failure_class,
      table_text,
      attempt,
      max_attempts,
      retry,
      dbconfig$password
    )
    retry_wait(retry_waits[[attempt]])
  }
  if (attempt > 1L) {
    message(sprintf(
      "csdb: loading data into %s with %s succeeded on attempt %d of at most %d.",
      load_target_words(table_text)$where,
      json_load_tool,
      attempt,
      max_attempts
    ))
  }
  return(invisible(TRUE))
}

# What one table of the load needs: its quoted name, its INSERT statement,
# its data, its columns in the order of the table, and the rows of each
# chunk.
json_load_plan <- function(connection, table, dt, max_rows, max_bytes) {
  table_text <- as.character(DBI::dbQuoteIdentifier(connection, table))
  # The columns and their order, as the psql load takes them. A column that
  # the table has and `dt` lacks stops here, before the transaction starts.
  columns <- DBI::dbListFields(connection, table)
  if (length(columns) == 0) {
    columns <- names(dt)
  }
  dt[0L, columns, with = FALSE]
  columns_text <- paste(
    DBI::dbQuoteIdentifier(connection, columns),
    collapse = ", "
  )
  sql <- paste0(
    "INSERT INTO ",
    table_text,
    " (",
    columns_text,
    ") SELECT ",
    columns_text,
    " FROM json_populate_recordset(NULL::",
    table_text,
    ", CAST(? AS json))"
  )
  return(list(
    table_text = table_text,
    sql = sql,
    dt = dt,
    columns = columns,
    chunks = json_load_chunks(dt, columns, max_rows, max_bytes),
    max_bytes = max_bytes
  ))
}

# The rows of each chunk of `dt`, as a list of integer vectors. A chunk has
# at most `max_rows` rows. Its JSON text has at most `max_bytes` bytes by the
# estimate of json_load_row_bytes().
json_load_chunks <- function(dt, columns, max_rows, max_bytes) {
  n <- nrow(dt)
  if (n == 0L) {
    return(list())
  }
  per_row <- json_load_row_bytes(dt, columns)
  size <- max(1L, min(as.integer(max_rows), floor(max_bytes / per_row)))
  starts <- seq.int(1L, n, by = size)
  return(lapply(starts, function(s) seq.int(s, min(s + size - 1L, n))))
}

# The bytes of JSON text for one row, from at most 1000 rows spread evenly
# over `dt`. A chunk that the estimate makes too long is split later, in
# json_load_send().
json_load_row_bytes <- function(dt, columns) {
  n <- nrow(dt)
  rows <- unique(round(seq.int(1L, n, length.out = min(n, 1000L))))
  bytes <- nchar(
    json_load_text(dt[rows, columns, with = FALSE]),
    type = "bytes"
  )
  return(max(1, bytes / length(rows)))
}

# One attempt: BEGIN, every chunk of every table, COMMIT. Returns NULL after
# a COMMIT that succeeded. Otherwise it rolls back and returns the error. An
# error of the COMMIT carries csdb_phase = "commit".
#
# BEGIN is outside the handler. When it fails, no transaction of this load
# exists, and a ROLLBACK could end a transaction that the caller opened.
json_load_attempt <- function(connection, plan, load_timeout, lock_timeout) {
  deadline <- if (load_timeout > 0) Sys.time() + load_timeout else NULL
  DBI::dbBegin(connection)
  failed <- tryCatch(
    {
      if (!is.null(lock_timeout)) {
        json_load_execute(
          connection,
          sprintf("SET LOCAL lock_timeout = '%ds'", as.integer(lock_timeout))
        )
      }
      json_load_lock(connection, plan, deadline)
      for (p in plan) {
        for (rows in p$chunks) {
          json_load_send(connection, p, rows, deadline)
        }
      }
      NULL
    },
    error = function(e) e
  )
  if (is.null(failed)) {
    failed <- tryCatch(
      {
        json_load_commit(connection)
        NULL
      },
      error = function(e) {
        e$csdb_phase <- "commit"
        return(e)
      }
    )
  }
  if (!is.null(failed)) {
    failed$csdb_rolled_back <- json_load_rollback(connection)
  }
  return(failed)
}

# ROLLBACK, and TRUE only when it succeeded and the connection then answers
# SELECT 1. psqlODBC 13.02 reported a ROLLBACK on a connection that the server
# had closed as a success (2026-10-15), so the SELECT 1 is the proof.
json_load_rollback <- function(connection) {
  done <- tryCatch(
    {
      json_load_rollback_once(connection)
      TRUE
    },
    error = function(e) FALSE
  )
  return(done && json_load_alive(connection))
}

# Tests replace these two functions.
json_load_rollback_once <- function(connection) {
  DBI::dbRollback(connection)
  return(invisible(TRUE))
}

json_load_alive <- function(connection) {
  return(tryCatch(
    {
      DBI::dbGetQuery(connection, "SELECT 1")
      TRUE
    },
    error = function(e) FALSE
  ))
}

# Send the rows `rows` of one table in one statement, and compare the rows
# that the statement stored with the rows sent. A chunk whose JSON text is
# longer than p$max_bytes is sent as two halves.
json_load_send <- function(connection, p, rows, deadline) {
  json <- json_load_text(p$dt[rows, p$columns, with = FALSE])
  bytes <- nchar(json, type = "bytes")
  if (length(rows) == 1L && bytes > p$max_row_bytes) {
    stop(errorCondition(
      paste0(
        "csdb: row ",
        rows,
        " of the data for table ",
        p$table_text,
        " is ",
        bytes,
        " bytes of JSON, more than the ",
        p$max_row_bytes,
        " bytes that csdb sends for one row. csdb did not send it."
      ),
      class = "csdb_json_too_long",
      call = NULL
    ))
  }
  if (bytes > p$max_bytes && length(rows) > 1L) {
    rm(json)
    half <- length(rows) %/% 2L
    json_load_send(connection, p, rows[seq_len(half)], deadline)
    json_load_send(connection, p, rows[-seq_len(half)], deadline)
    return(invisible(NULL))
  }
  json_load_statement_timeout(connection, deadline)
  stored <- json_load_execute(connection, p$sql, json)
  if (!identical(as.numeric(stored), as.numeric(length(rows)))) {
    stop(errorCondition(
      paste0(
        "csdb: table ",
        p$table_text,
        " stored ",
        stored,
        " rows of one statement, and csdb sent ",
        length(rows),
        " rows."
      ),
      class = "csdb_json_count",
      call = NULL
    ))
  }
  return(invisible(NULL))
}

# Take the lock that an INSERT takes, ROW EXCLUSIVE, on every table of the
# load before the first INSERT. The tables are locked in the sorted order of
# their quoted, fully qualified names, so two loads that lock the same tables
# take them in the same order.
#
# psqlODBC 13.02 runs each statement inside a SAVEPOINT. A statement with a
# parameter starts with a describe step, which needs this lock. When the
# describe step fails, psqlODBC rolls back to the SAVEPOINT and runs the
# statement a second time. On 2026-10-15 against PostgreSQL 18.6, an INSERT
# that waited on a lock therefore waited twice: 2 x 2 s with lock_timeout =
# '2s'. A LOCK TABLE statement has no parameter, so it waits once, and the
# INSERT then finds the lock taken.
json_load_lock <- function(connection, plan, deadline) {
  names <- vapply(plan, function(p) p$table_text, character(1))
  for (name in sort(unique(names), method = "radix")) {
    json_load_statement_timeout(connection, deadline)
    json_load_execute(
      connection,
      paste0("LOCK TABLE ", name, " IN ROW EXCLUSIVE MODE")
    )
  }
  return(invisible(NULL))
}

# Before each statement, limit it to the time that remains of load_timeout.
# The server then cancels a statement that runs longer and rolls the
# transaction back.
json_load_statement_timeout <- function(connection, deadline) {
  if (is.null(deadline)) {
    return(invisible(NULL))
  }
  left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
  if (left <= 0) {
    stop("csdb: the load ran longer than load_timeout.", call. = FALSE)
  }
  json_load_execute(
    connection,
    sprintf(
      "SET LOCAL statement_timeout = %d",
      as.integer(ceiling(left * 1000))
    )
  )
  return(invisible(NULL))
}

# One statement of the JSON load, with the JSON text as its one parameter, or
# with no parameter. Returns the number of rows that the statement changed.
# Tests replace this function and json_load_commit().
json_load_execute <- function(connection, sql, json = NULL) {
  if (is.null(json)) {
    return(DBI::dbExecute(connection, sql))
  }
  return(DBI::dbExecute(connection, sql, params = list(json)))
}

json_load_commit <- function(connection) {
  DBI::dbCommit(connection)
  return(invisible(TRUE))
}

# The JSON text of the rows of `dt`: an array with one object for each row,
# NA as null.
json_load_text <- function(dt) {
  return(yyjsonr::write_json_str(
    json_load_values(dt),
    opts = yyjsonr::opts_write_json(
      dataframe = "rows",
      num_specials = "null",
      str_specials = "null"
    )
  ))
}

# The values of `dt` in the form that the JSON text carries. `dt` is a copy,
# and this function changes it by reference.
json_load_values <- function(dt) {
  for (column in names(dt)) {
    data.table::set(dt, j = column, value = json_load_column(dt[[column]]))
  }
  return(dt)
}

# One column in the form that the JSON text carries.
#
# Inf, -Inf and NaN become NA, and a POSIXt becomes as.character(), as
# write_data_infile() does for the psql load. So a TIMESTAMP column stores
# the same value from both loads. This function does not call
# scrub_non_finite(), which changes a whole data.table by reference.
#
# A character value is sent unchanged, so an empty string stays an empty
# string. A logical becomes 1 or 0, as fwrite(logical01 = TRUE) writes it for
# psql: csdb creates BIT(1) for BOOLEAN, and BIT(1) refuses JSON true. A
# factor becomes its labels. A Date is written as "YYYY-MM-DD". A double goes
# to json_load_double(). Any other class becomes as.character().
json_load_column <- function(x) {
  if (is.character(x) || identical(class(x), "integer")) {
    return(x)
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.double(x)) {
    return(json_load_double_class(x))
  }
  # A data.table IDate is of type integer, and as.character() writes it as
  # "YYYY-MM-DD".
  return(as.character(x))
}

# A column of type double: a numeric, a Date, a POSIXct or another class.
json_load_double_class <- function(x) {
  x[is.infinite(x) | is.nan(x)] <- NA
  if (inherits(x, "POSIXt")) {
    return(as.character(x))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (identical(class(x), "numeric")) {
    return(json_load_double(x))
  }
  return(as.character(x))
}

# A double column. yyjsonr writes the shortest text that reads back as the
# same double, but it writes 5 as 5.0, and an INTEGER column refuses "5.0". So
# a column whose values are all whole becomes an integer when every value fits
# one, and otherwise the text of formatC(format = "f", digits = 0).
json_load_double <- function(x) {
  known <- x[!is.na(x)]
  if (length(known) == 0L || any(known != trunc(known))) {
    return(x)
  }
  if (all(abs(known) <= .Machine$integer.max)) {
    return(as.integer(x))
  }
  out <- formatC(x, format = "f", digits = 0)
  out[is.na(x)] <- NA_character_
  return(out)
}

# The failure class of an error of json_load_attempt().
#
# It fails closed. Only a confirmed ROLLBACK (csdb_rolled_back) allows a class
# other than "ambiguous", and only an English server text that csdb knows
# gives "duplicate key" or "transient". A text in another language is
# "other".
classify_json_failure <- function(failed) {
  if (identical(failed$csdb_phase, "commit")) {
    return("ambiguous")
  }
  if (!isTRUE(failed$csdb_rolled_back)) {
    return("ambiguous")
  }
  if (inherits(failed, c("csdb_json_count", "csdb_json_too_long"))) {
    return("other")
  }
  text <- conditionMessage(failed)
  if (grepl(pg_json_lost_text, text)) {
    return("ambiguous")
  }
  if (grepl(pg_json_duplicate_text, text)) {
    return("duplicate key")
  }
  if (grepl(pg_json_retry_text, text)) {
    return("transient")
  }
  return("other")
}

# The message of one failed attempt, and the error after the last attempt.
# They have the form and the classes of the messages and errors of
# run_checked_load().
json_load_report <- function(
  failed,
  failure_class,
  table_text,
  attempt,
  max_attempts,
  retry,
  password
) {
  words <- load_target_words(table_text)
  where <- words$where
  printed <- load_output_excerpt(
    strsplit(conditionMessage(failed), "\n", fixed = TRUE)[[1]],
    password
  )
  message(sprintf(
    "csdb: loading data into %s with %s failed on attempt %d of at most %d (%s). %s It printed:\n%s",
    where,
    json_load_tool,
    attempt,
    max_attempts,
    failure_class,
    if (retry) {
      paste0("Retrying in ", retry_waits[[attempt]], " s.")
    } else {
      "csdb does not retry it."
    },
    printed
  ))
  if (retry) {
    return(invisible(NULL))
  }
  outcome <- if (failure_class == "ambiguous") {
    json_load_ambiguous_text(failed, words)
  } else {
    paste0(
      " The server rolled the transaction back, so it stored no row in ",
      where,
      "."
    )
  }
  stop(errorCondition(
    paste0(
      "Loading data into ",
      where,
      " failed. ",
      json_load_tool,
      " reported an error. The failure class is ",
      failure_class,
      ", after ",
      attempt,
      if (attempt == 1L) " attempt" else " attempts",
      ".",
      outcome,
      " It printed:\n",
      printed
    ),
    class = c(
      paste0("csdb_load_", gsub(" ", "_", failure_class, fixed = TRUE)),
      "csdb_load_error"
    ),
    table = table_text,
    tool = json_load_tool,
    attempts = attempt,
    failure_class = failure_class
  ))
}

# Why the server may have committed the rows of an ambiguous JSON load, and
# what to do before the caller loads the rows again.
json_load_ambiguous_text <- function(failed, words) {
  why <- if (identical(failed$csdb_phase, "commit")) {
    " The COMMIT failed"
  } else if (grepl(pg_json_lost_text, conditionMessage(failed))) {
    " The connection broke during the load"
  } else {
    paste0(
      " csdb could not confirm the ROLLBACK: it failed, or the connection did",
      " not answer SELECT 1 after it"
    )
  }
  return(paste0(
    why,
    ", so the server may or may not have committed the rows to ",
    words$where,
    ". Count the rows in ",
    words$where,
    " before you load these rows again. csdb does not run an ambiguous load again. If ",
    words$where,
    words$has,
    " no primary key and you run the load again by hand,",
    words$the_table,
    " can get duplicate rows."
  ))
}

# The connection for a JSON load. A connection that cannot open, such as a
# login that a token proxy refuses, stops with class csdb_load_transient:
# csdb sent no row. connect() made its own attempts first. Its error says only
# how many, so the error here also holds the message of its last attempt,
# with the error of the server. A psql load keeps its old path, so this
# applies to pg_load = "json" only.
load_connection <- function(dbconnection, table) {
  if (!identical(dbconnection$config$pg_load, "json")) {
    return(dbconnection$autoconnection)
  }
  attempts <- character()
  keep <- function(m) {
    attempts <<- c(attempts, conditionMessage(m))
    return(invisible(NULL))
  }
  failed <- function(e) {
    name <- if (methods::is(table, "Id")) {
      paste(table@name, collapse = ".")
    } else {
      as.character(table)
    }
    where <- load_target_words(name)$where
    stop(errorCondition(
      paste0(
        "Loading data into ",
        where,
        " failed before csdb sent a row: csdb could not connect. The failure",
        " class is transient. The connection error was: ",
        conditionMessage(e),
        if (length(attempts) > 0) {
          paste0(
            ". The last attempt said: ",
            load_output_excerpt(
              utils::tail(attempts, 1),
              dbconnection$config$password
            )
          )
        }
      ),
      class = c("csdb_load_transient", "csdb_load_error"),
      table = where,
      tool = json_load_tool,
      attempts = 1L,
      failure_class = "transient",
      parent = e
    ))
  }
  return(withCallingHandlers(
    tryCatch(dbconnection$autoconnection, error = failed),
    message = keep
  ))
}

# The lc_messages of the server, or NA when csdb cannot read it. Tests replace
# this function.
json_load_lc_messages <- function(connection) {
  return(tryCatch(
    as.character(DBI::dbGetQuery(connection, "SHOW lc_messages")[[1]][[1]]),
    error = function(e) NA_character_
  ))
}

# The warning text for an lc_messages in which csdb cannot read the error
# classes of a JSON load, or NULL for C, POSIX and English.
json_load_locale_text <- function(lc) {
  if (
    !is.na(lc) &&
      (lc %in% c("C", "POSIX") || startsWith(lc, "C.") || startsWith(lc, "en"))
  ) {
    return(NULL)
  }
  return(paste0(
    "csdb: the server sends its messages with lc_messages = '",
    lc,
    "'. csdb finds the duplicate-key and the transient failures of a",
    " pg_load = \"json\" load in the English text only. A load that fails",
    " on this connection therefore has class csdb_load_other or",
    " csdb_load_ambiguous."
  ))
}

# DBConnection_v9 runs this once for each new PostgreSQL connection with
# pg_load = "json".
json_load_check_locale <- function(connection) {
  text <- json_load_locale_text(json_load_lc_messages(connection))
  if (!is.null(text)) {
    warning(text, call. = FALSE)
  }
  return(invisible(NULL))
}
