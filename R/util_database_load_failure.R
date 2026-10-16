# How a load sorts a failed bcp call into a failure class, and the text of
# the messages that report it. run_checked_load() in "util_database_load.R"
# and the JSON load in "util_database_load_json.R" call these helpers. Until
# 2026.10.17 this file also sorted the output of psql.

# bcp prints "SQLState = <state>, NativeError = <number>" before each error.
# A refused port, a host that does not resolve and a login timeout all give
# SQLState 08001 or S1T00. 1205 is a deadlock and 1222 is a lock timeout, and
# the server rolls the batch back for both. 2627 is a PRIMARY KEY or UNIQUE
# constraint, and 2601 is a unique index.
#
# bcp prints "Starting copy..." when it has logged in and starts to send
# rows. A connection error before that line is safe to retry. After it, a
# connection error (08xxx, HYT00 or HYT01) is ambiguous. So is NativeError
# 596, "the session is in the kill state", which bcp 17.11 printed with
# SQLState S1000 when KILL ended its session mid-load (2026-10-03).
mssql_state_pattern <- "SQLState = ([0-9A-Z]{5}), NativeError = (-?[0-9]+)"
mssql_transfer_phase <- "^\\s*Starting copy\\.\\.\\."
mssql_connection_state <- "^(08[0-9A-Z]{3}|HYT0[01]|S1T00)$"
mssql_killed_native <- "596"
mssql_retry_state <- "^40001$"
mssql_retry_native <- c("1205", "1222")
mssql_duplicate_native <- c("2627", "2601")

# Replace every secret, and the password in a PostgreSQL URI, with "***".
redact_load_output <- function(x, secrets = NULL) {
  for (s in secrets) {
    if (!is.null(s) && !is.na(s) && nzchar(s)) {
      x <- gsub(s, "***", x, fixed = TRUE)
    }
  }
  x <- gsub("(postgresql://[^:/@]*:)[^@]*@", "\\1***@", x)
  return(x)
}

# A progress line that bcp prints while it sends rows, such as
# "1000 rows sent to SQL Server. Total sent: 1000".
bcp_progress_pattern <- "^\\s*[0-9]+ rows sent to SQL Server"

# A line that bcp starts with "SQLState = " names the state of the error on
# the next line.
bcp_state_line_pattern <- "^\\s*SQLState = "

# The client output that a load message shows, as one text.
#
# A large bcp load prints thousands of progress lines, so the message drops
# them and every blank line. The other lines are of two kinds:
#
#   * error lines, which start with "Error = " or "SQLState = ". The message
#     keeps the first copy of each distinct error line, wherever it is, and
#     drops every repeat. When more than `max_errors` distinct error lines
#     exist, it keeps the first half and the last half of `max_errors`. One
#     line says how many it omitted between them.
#   * all other lines. The message keeps the first `keep` and the last
#     `keep`. One line says how many lines it omitted between them.
#
# So the message holds at most 2 * keep + 1 + max_errors + 1 lines, which is
# 82 lines with the defaults. Every kept line stays in its original order.
# The lines are selected from the raw output and then redacted. A password
# such as "2" could otherwise change a progress line or an error line.
load_output_excerpt <- function(
  output,
  secrets = NULL,
  keep = 20L,
  max_errors = 40L
) {
  output <- output[nzchar(trimws(output))]
  output <- output[!grepl(bcp_progress_pattern, output)]
  error_line <- grepl(bcp_error_pattern, output) |
    grepl(bcp_state_line_pattern, output)
  show <- rep(TRUE, length(output))
  note <- rep(NA_character_, length(output))
  # The first line of a run that is left out becomes the note about it.
  leave_out <- function(at, text) {
    show[at] <<- FALSE
    show[at[[1]]] <<- TRUE
    return(note[at[[1]]] <<- text)
  }

  other <- which(!error_line)
  n <- length(other)
  if (n > 2L * keep) {
    omitted <- other[(keep + 1L):(n - keep)]
    leave_out(omitted, sprintf("[csdb omitted %d lines here]", length(omitted)))
  }

  errors <- which(error_line)
  repeated <- errors[duplicated(output[errors])]
  show[repeated] <- FALSE
  distinct <- setdiff(errors, repeated)
  m <- length(distinct)
  if (m > max_errors) {
    half <- max_errors %/% 2L
    extra <- distinct[(half + 1L):(m - (max_errors - half))]
    leave_out(
      extra,
      sprintf("[csdb omitted %d further error lines]", length(extra))
    )
  }

  output <- redact_load_output(output, secrets)
  output <- ifelse(is.na(note), output, note)
  return(paste0(output[show], collapse = "\n"))
}

# Return NULL when the client succeeded and stored every row. Otherwise
# return the reason, and whether the output can show the failure class.
#
# A load fails in four cases:
#
#   * the client exited with a non-zero status.
#   * a line of output matches `error_pattern`. bcp exits with 0 in some
#     partial failures and still prints the error.
#   * `rows_sent` is set and no line matches `rows_pattern`. The client then
#     did not say how many rows it stored.
#   * `rows_sent` is set and the number that `rows_pattern` captures differs
#     from it. bcp prints "<n> rows copied.".
#
# The last two come after the client exited with 0, so the server may have
# committed rows. Their failure class is "ambiguous", and csdb does not retry.
#
# `rows_sent` and `tables` MAY hold one element for each of several tables.
# The last length(rows_sent) counts are then compared with them in order.
# Until 2026.10.17 the psql session of insert_data_many() used this. Every
# caller now loads one table.
load_failure_reason <- function(
  result,
  output,
  rows_sent = NULL,
  rows_pattern = NULL,
  error_pattern = NULL,
  tables = NULL
) {
  if (result$status != 0) {
    return(list(text = "returned a non-zero exit status", classify = TRUE))
  }
  if (!is.null(error_pattern) && any(grepl(error_pattern, output))) {
    return(list(text = "reported an error", classify = TRUE))
  }
  if (is.null(rows_sent)) {
    return(NULL)
  }
  hits <- regmatches(output, regexec(rows_pattern, output))
  hits <- unlist(lapply(hits, `[`, 2))
  hits <- hits[!is.na(hits)]
  n_tables <- length(rows_sent)
  if (length(hits) < n_tables) {
    return(list(
      text = paste0(
        "did not report how many rows it stored",
        if (n_tables > 1L) {
          paste0(" into each of the ", n_tables, " tables")
        },
        ", and csdb sent ",
        paste0(rows_sent, collapse = ", "),
        " rows"
      ),
      classify = FALSE,
      exited_ok = TRUE
    ))
  }
  rows_stored <- as.numeric(utils::tail(hits, n_tables))
  wrong <- which(rows_stored != rows_sent)
  if (length(wrong) > 0) {
    i <- wrong[[1]]
    return(list(
      text = paste0(
        "stored ",
        rows_stored[[i]],
        " rows",
        if (n_tables > 1L) {
          paste0(" into table ", tables[[i]])
        },
        ", and csdb sent ",
        rows_sent[[i]],
        " rows"
      ),
      classify = FALSE,
      exited_ok = TRUE
    ))
  }
  return(NULL)
}

# Sort the raw output of a failed bcp call into "duplicate key",
# "ambiguous", "transient" or "other". A duplicate key comes first, because a
# retry cannot remove it. An ambiguous failure comes before a transient one,
# because only a load that certainly stored nothing may run again.
classify_load_failure <- function(output) {
  capture <- function(pattern, group) {
    m <- regmatches(output, regexec(pattern, output))
    return(vapply(m, `[`, character(1), group + 1L))
  }
  return(classify_bcp_failure(
    output,
    states = capture(mssql_state_pattern, 1L),
    natives = capture(mssql_state_pattern, 2L)
  ))
}

classify_bcp_failure <- function(output, states, natives) {
  if (any(natives %in% mssql_duplicate_native)) {
    return("duplicate key")
  }
  connection <- any(grepl(mssql_connection_state, states))
  broken <- connection || any(natives %in% mssql_killed_native)
  if (broken && any(grepl(mssql_transfer_phase, output))) {
    return("ambiguous")
  }
  if (
    any(natives %in% mssql_retry_native) ||
      any(grepl(mssql_retry_state, states)) ||
      connection
  ) {
    return("transient")
  }
  return("other")
}

# The words that the messages of run_checked_load() use for `table`. One table
# keeps the words that csdb used before insert_data_many() existed.
load_target_words <- function(table) {
  if (length(table) > 1L) {
    return(list(
      where = paste0("tables ", paste0(table, collapse = ", ")),
      has = " have",
      the_table = " the tables"
    ))
  }
  return(list(
    where = paste0("table ", table),
    has = " has",
    the_table = " the table"
  ))
}

# Why the server may have committed the rows of an ambiguous load, and the
# instruction to count them before the caller loads the rows again.
count_the_rows_text <- function(timed_out, reason, tool, where) {
  why <- if (timed_out) {
    " csdb killed the client, so the server may or may not have committed the rows to "
  } else if (isTRUE(reason$exited_ok)) {
    paste0(
      " ",
      tool,
      " exited without an error, so the server may have committed rows to "
    )
  } else {
    " The connection broke after the load started, so the server may or may not have committed the rows to "
  }
  return(paste0(
    why,
    where,
    ". Count the rows in ",
    where,
    " before you load these rows again."
  ))
}
