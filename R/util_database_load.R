# The S7 method assignments for load_data_infile and
# upsert_load_data_infile.
#
# The generics and the db_* class objects are in "util_database.R". R
# sources this directory in C collation order. This name sorts after that
# one, so every generic and class exists before the assignments below run.

# S7 method definitions
# load_data_infile methods

# The default `file` is tempfile(fileext = ".csv"). R evaluates it once per
# call, so two concurrent calls write two different files, both in tempdir().
# The method deletes `file` when it returns, also a `file` that the caller
# passed. A write that fails partway leaves a caller's `file` in place, and
# the method deletes a default `file` then too. Until 2026.10.7 the default
# was the fixed path "/xtmp/x123.csv".
S7::method(load_data_infile, db_default) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt = NULL,
  file = tempfile(fileext = ".csv"),
  force_tablock = FALSE,
  load_timeout = 3600
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  t0 <- Sys.time()

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = FALSE]
  }
  # Registered before the write, so a partly written default file is also
  # deleted. A file that the caller passed is deleted only after a full write.
  if (missing(file)) {
    on.exit(unlink(file), add = TRUE)
  }
  write_data_infile(dt = dt, file = file)
  on.exit(unlink(file), add = TRUE)

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- TRUE
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  sql <- paste0(
    "LOAD DATA INFILE ",
    DBI::dbQuoteString(connection, path),
    "\n",
    "INTO TABLE ",
    DBI::dbQuoteIdentifier(connection, table),
    "\n",
    "CHARACTER SET utf8",
    "\n",
    "FIELDS TERMINATED BY ",
    DBI::dbQuoteString(connection, sep),
    "\n",
    "OPTIONALLY ENCLOSED BY ",
    DBI::dbQuoteString(connection, quote),
    "\n",
    "LINES TERMINATED BY ",
    DBI::dbQuoteString(connection, eol),
    "\n",
    "IGNORE ",
    skip + as.integer(header),
    " LINES \n",
    "(",
    paste0(correct_order, collapse = ","),
    ")"
  )
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)

  return(invisible())
}

S7::method(load_data_infile, db_mssql) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  force_tablock = FALSE,
  load_timeout = 3600
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  a <- Sys.time()

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = FALSE]
  }
  # Registered before the write, so a partly written default file is also
  # deleted. A file that the caller passed is deleted only after a full write.
  if (missing(file)) {
    on.exit(unlink(file), add = TRUE)
  }
  write_data_infile(
    dt = dt,
    file = file,
    colnames = FALSE,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )
  on.exit(unlink(file), add = TRUE)

  format_file <- tempfile(tmpdir = tempdir(check = TRUE))
  on.exit(unlink(format_file), add = TRUE)

  # bcp needs a data-file argument in format mode too. This was "nul", the
  # null device on Windows. On Linux "nul" is a relative file name, so bcp
  # opened ./nul and failed in every working directory it cannot write to. A
  # name in tempdir() works on every OS. The `in` call reads format_file.
  format_data_file <- tempfile(tmpdir = tempdir(check = TRUE))
  on.exit(unlink(format_data_file), add = TRUE)

  args <- c(
    table,
    "format",
    format_data_file,
    "-q",
    "-c",
    "-f",
    format_file,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password,
    "-l",
    "30"
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }

  if (Sys.which("bcp") == "") {
    stop(
      "bcp command not found. Please install SQL Server command line tools.",
      call. = FALSE
    )
  }

  run_checked_load(
    command = "bcp",
    args = args,
    tool = "bcp format",
    table = table,
    secrets = dbconfig$password,
    error_pattern = bcp_error_pattern,
    timeout = load_timeout
  )

  # Every option and its value are separate elements. run_load_tool() quotes
  # each element, so bcp receives "-h" and "ORDER(id ASC)" as two arguments.
  hint_arg <- NULL
  if (!is.null(key(dt))) {
    hint_arg <- c(
      "-h",
      paste0("ORDER(", paste0(key(dt), " ASC", collapse = ", "), ")")
    )
  }

  args <- c(
    table,
    "in",
    file,
    "-a",
    "16384",
    hint_arg,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password,
    "-l",
    "30",
    "-f",
    format_file,
    "-m",
    0
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }

  if (Sys.which("bcp") == "") {
    stop(
      "bcp command not found. Please install SQL Server command line tools.",
      call. = FALSE
    )
  }

  run_checked_load(
    command = "bcp",
    args = args,
    tool = "bcp in",
    table = table,
    secrets = dbconfig$password,
    rows_sent = nrow(dt),
    rows_pattern = "^\\s*([0-9]+) rows copied\\.?\\s*$",
    error_pattern = bcp_error_pattern,
    timeout = load_timeout
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  return(invisible())
}

# The PostgreSQL load sends the rows as JSON text over `connection`, and
# starts no client. See util_database_load_json.R. The upsert method below
# loads its staging table through this method. `file` and `force_tablock` are
# accepted so the call sites in DBTable_v9 need no PostgreSQL arm, and are
# then ignored. Until 2026.10.17 this method wrote a file and started psql.
S7::method(load_data_infile, db_postgres) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  force_tablock = FALSE,
  load_timeout = 3600
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  return(json_load_tables(
    connection = connection,
    dbconfig = dbconfig,
    tables = list(table),
    dts = list(dt),
    load_timeout = load_timeout
  ))
}

# Load a data.table into a SQLite table.
#
# There is no staging file and no external client binary here: SQLite is a
# file, and DBI::dbAppendTable() writes 100,000 rows in about 0.02 seconds.
# `file` and `force_tablock` are accepted so the call sites in DBTable_v9 need
# no SQLite arm, and are then ignored.
#
# The copy() is load-bearing. The other three backends reach
# write_data_infile(), which modifies the caller's data.table by reference and
# has always done so. Doing the same here would leave the caller holding a
# table whose Inf values had turned into NA, which is a surprising thing for
# an insert to do to its argument.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(load_data_infile, db_sqlite) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt = NULL,
  file = tempfile(),
  force_tablock = FALSE,
  load_timeout = 3600
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  dt <- data.table::copy(dt)

  # Inf survives dbAppendTable and reads back as Inf, where the CSV backends
  # write NA. Without this the SQLite backend silently disagrees with them.
  scrub_non_finite(dt)

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) {
    dt <- dt[, correct_order, with = FALSE]
  }

  DBI::dbAppendTable(connection, table, dt)

  return(invisible())
}

# Continue with upsert_load_data_infile methods

# The default `file` is tempfile(fileext = ".csv"), one new file in tempdir()
# for each call. load_data_infile() writes it, and this method deletes it when
# it returns, also after a write that fails partway. Until 2026.10.7 the
# default was the fixed path "/tmp/x123.csv", so two concurrent upserts wrote
# the same file.
S7::method(upsert_load_data_infile, db_default) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(fileext = ".csv"),
  fields,
  keys = NULL,
  drop_indexes = NULL,
  load_timeout = 3600
) {
  # load_data_infile() receives `file` explicitly, so this method deletes its
  # own default file. A partly written file is deleted too.
  if (missing(file)) {
    on.exit(unlink(file), add = TRUE)
  }
  temp_name <- random_uuid()
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(connection, sql)

  if (!is.null(drop_indexes)) {
    # `drop_indexes` holds LOGICAL names. Every caller fills it from
    # names(self$indexes): the two DBTable_v9 defaults, and the two
    # DBTableExtended_v9 defaults in cs9. The temporary table is created LIKE
    # {table}, so it inherits {table}'s index names, and those are the
    # PHYSICAL names. Map them here, from the identity of {table} and not of
    # the temporary table. Without that map this drops nothing, and the upsert
    # keeps every index it meant to remove.
    for (i in drop_indexes) {
      physical <- index_physical_name(table = table, index = i)
      try(
        DBI::dbExecute(
          connection,
          glue::glue("ALTER TABLE `{temp_name}` DROP INDEX `{physical}`")
        ),
        TRUE
      )
    }
  }

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    load_timeout = load_timeout
  )

  t0 <- Sys.time()

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue(
    "
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    "
  )
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)

  return(invisible())
}

S7::method(upsert_load_data_infile, db_mssql) <- function(
  connection,
  dbconfig,
  table,
  dt,
  file = tempfile(),
  fields,
  keys,
  drop_indexes = NULL,
  load_timeout = 3600
) {
  # load_data_infile() receives `file` explicitly, so this method deletes its
  # own default file. A partly written file is deleted too.
  if (missing(file)) {
    on.exit(unlink(file), add = TRUE)
  }
  temp_name <- paste0("tmp", random_uuid())
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE 1 = 0;")
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE,
    load_timeout = load_timeout
  )

  a <- Sys.time()
  add_index(
    connection = connection,
    table = temp_name,
    keys = keys
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", keys),
    s = paste0("s.", keys)
  )
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  sql_update_set <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", fields),
    s = paste0("s.", fields)
  )
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue(
    "
  MERGE {table} t
  USING {temp_name} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED BY TARGET
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  "
  )

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  return(invisible())
}

S7::method(upsert_load_data_infile, db_postgres) <- function(
  connection,
  dbconfig,
  table,
  dt,
  file = tempfile(),
  fields,
  keys,
  drop_indexes = NULL,
  load_timeout = 3600
) {
  # load_data_infile() receives `file` explicitly, so this method deletes its
  # own default file. A partly written file is deleted too.
  if (missing(file)) {
    on.exit(unlink(file), add = TRUE)
  }
  temp_name <- DBI::Id(
    schema = table@name[["schema"]],
    paste0("tmp", random_uuid())
  )
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)
  table_text <- DBI::dbQuoteIdentifier(connection, table)

  # A connection that lost its server gets no DROP, because a second statement
  # on it crashes R: see csdb_connection_is_alive(). SELECT INTO makes a
  # plain table, not a temporary one, so the table then stays in the schema.
  # The DROP could not remove it on that connection either.
  on.exit(
    if (csdb_connection_is_alive(connection)) {
      DBI::dbRemoveTable(connection, temp_name)
    },
    add = TRUE,
    after = FALSE
  )

  sql <- glue::glue(
    "SELECT * INTO {temp_name_text} FROM {table_text} WHERE 1 = 0;"
  )
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE,
    load_timeout = load_timeout
  )

  a <- Sys.time()
  # Two arguments here were wrong, and the try() inside add_index() hid both.
  # `"ind" + random_uuid()` is `non-numeric argument to binary operator`,
  # because `+` does not concatenate strings in R.
  #
  # This passes `temp_name`, the DBI::Id, and not the pre-quoted
  # `temp_name_text` beside it. add_index() quotes the table itself from
  # 2026.8.16, so a pre-quoted string would arrive and be quoted a second
  # time. Verified against PostgreSQL 16.14 on norsyss_data1.
  add_index(
    connection = connection,
    table = temp_name,
    keys = keys,
    index = paste0("ind", random_uuid())
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue(
    "{t} = {s}",
    t = paste0("t.", keys),
    s = paste0("s.", keys)
  )
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  update_fields <- setdiff(fields, keys)
  sql_update_set <- glue::glue(
    "{t} = {s}",
    t = update_fields,
    s = paste0("s.", update_fields)
  )
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue(
    "
  MERGE INTO {table_text} t
  USING {temp_name_text} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  "
  )

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  return(invisible())
}

# Upsert a data.table into a SQLite table.
#
# SQLite has no MERGE and no ON DUPLICATE KEY UPDATE. It has
# INSERT ... ON CONFLICT, which needs three things the other backends do not:
#
#   * a PRIMARY KEY or UNIQUE constraint on the conflict target. The SQLite
#     create_table method inlines one, which is why creation and upsert are
#     the same piece of work.
#   * `WHERE true` between the SELECT and the ON CONFLICT clause. Without it
#     SQLite's parser cannot tell the ON CONFLICT clause from a join
#     constraint on the SELECT, and rejects the statement.
#   * DO NOTHING rather than DO UPDATE SET when every field is a key, because
#     there is then nothing left to assign.
#
# The three preconditions are checked before any SQL is emitted. Empty `keys`
# would produce `ON CONFLICT ()`, a syntax error that says nothing about the
# cause. `fields` that are not exactly the table's live columns cannot work at
# all: CREATE TABLE ... AS SELECT discards defaults and constraints, so a
# staging table filled from a partial field list would insert NULL into every
# omitted column.
#
# `drop_indexes` is ignored, exactly as the PostgreSQL method already ignores
# it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(upsert_load_data_infile, db_sqlite) <- function(
  connection,
  dbconfig = NULL,
  table,
  dt,
  file = tempfile(),
  fields,
  keys = NULL,
  drop_indexes = NULL,
  load_timeout = 3600
) {
  if (length(keys) == 0) {
    stop(
      "upsert on SQLite needs at least one key column: ",
      "keys is empty, and ON CONFLICT () is a syntax error.",
      call. = FALSE
    )
  }
  if (!all(keys %in% fields)) {
    stop(
      "upsert on SQLite needs every key to be one of the fields. ",
      "Missing from fields: ",
      paste0(setdiff(keys, fields), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  live_fields <- DBI::dbListFields(connection, table)
  if (!setequal(fields, live_fields)) {
    stop(
      "upsert on SQLite needs fields to be exactly the columns of the table. ",
      "In fields but not the table: ",
      paste0(setdiff(fields, live_fields), collapse = ", "),
      ". In the table but not fields: ",
      paste0(setdiff(live_fields, fields), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  table_text <- DBI::dbQuoteIdentifier(connection, table)
  temp_name <- paste0("tmp", random_uuid())
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)

  on.exit(
    try(
      DBI::dbExecute(
        connection,
        paste0("DROP TABLE IF EXISTS ", temp_name_text)
      ),
      silent = TRUE
    ),
    add = TRUE,
    after = FALSE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE TEMPORARY TABLE ",
      temp_name_text,
      " AS SELECT * FROM ",
      table_text,
      " WHERE 0"
    )
  )

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    load_timeout = load_timeout
  )

  fields_text <- paste0(
    DBI::dbQuoteIdentifier(connection, fields),
    collapse = ", "
  )
  keys_text <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  update_fields <- setdiff(fields, keys)
  if (length(update_fields) > 0) {
    update_fields_text <- DBI::dbQuoteIdentifier(connection, update_fields)
    resolution <- paste0(
      "DO UPDATE SET ",
      paste0(
        update_fields_text,
        " = excluded.",
        update_fields_text,
        collapse = ", "
      )
    )
  } else {
    resolution <- "DO NOTHING"
  }

  DBI::dbExecute(
    connection,
    paste0(
      "INSERT INTO ",
      table_text,
      " (",
      fields_text,
      ") SELECT ",
      fields_text,
      " FROM ",
      temp_name_text,
      " WHERE true ON CONFLICT (",
      keys_text,
      ") ",
      resolution
    )
  )

  return(invisible())
}

# The external load client: bcp for SQL Server. The PostgreSQL load starts no
# client, and util_database_load_json.R gives it the same failure classes.
#
# Until 2026.10.3 the bcp and the psql method called system2() and ignored
# the result. A load that the database refused, such as a duplicate key,
# returned normally and lost the rows. bcp writes its errors to stdout, which
# was discarded.
#
# From 2026.10.3 run_checked_load() runs the client, checks the result, and
# sorts a failure into one of four classes:
#
#   * transient: the failure certainly stored nothing. The connection never
#     started, or the server rolled the load back after a deadlock, a
#     serialization failure or a lock timeout. The load runs again, at most
#     3 times in total.
#   * ambiguous: the connection broke after the load started. The server may
#     or may not have committed the rows. The load stops at once.
#   * duplicate key: the data holds a key that the table already holds. The
#     load stops with an error of class csdb_load_duplicate_key.
#     insert_data(confirm_insert_via_nrow = TRUE) catches it and upserts.
#   * other: everything else. The load stops at once.
#
# A retry is safe only because a failed load stores no row. bcp sends every
# row in one batch because csdb passes no -b. This was verified against a
# real server on 2026-10-03.

# A line that bcp starts with "Error = " reports a refused row or batch.
bcp_error_pattern <- "^\\s*Error = "

# The waits before the second and the third attempt, in seconds. A bcp load,
# a JSON load and DBConnection_v9$connect() all use them. Until 2026.10.12 the
# loads waited 1 s and 3 s, and connect() made 2 attempts 1 s apart by
# default. A proxy that refuses logins, such as norsyss-pg-auth below, needs
# seconds to recover, not 1 s.
retry_waits <- c(2, 10)

# The longest time in milliseconds that a SQLite connection waits for a lock
# that another connection holds. DBConnection_v9$connect() sets it with PRAGMA
# busy_timeout. SQLite allows one writer, and without this wait a second
# writer, for example a forked cs9 plan, stops at once with "database is
# locked".
sqlite_busy_timeout_ms <- 60000L

# The longest time in seconds that the JSON load of insert_data_many() waits
# for one lock. Its transaction starts with SET LOCAL lock_timeout. A longer
# wait stops the load, the server rolls back the whole transaction, and
# json_load_tables() runs the load again after the waits above. One table,
# for example, waits behind a TRUNCATE or a CREATE INDEX that another
# connection has not committed. The ALTER TABLE of widen_declared_doubles()
# waits at most as long.
load_lock_timeout_seconds <- 60

# Run one external load client and capture everything it prints.
#
# Every bcp call in the load path goes through this function, so a test can
# replace it with testthat::local_mocked_bindings() and needs no server. bcp
# writes its errors to stdout, and both streams are captured.
#
# `timeout` is in seconds, and 0 means no limit. system2() kills a client
# that runs longer and returns the status 124.
#
# suppressWarnings() is load-bearing. system2() warns on a non-zero status,
# and that warning holds the whole command line, password included. The
# tryCatch() is there for the same reason: on Windows a client that cannot
# start raises an error that also holds the command line.
# run_checked_load() redacts the output.
#
# system2() pastes `args` into one command line. On Linux a shell runs that
# line, so shQuote() puts each element in quotes, and the client receives
# every element unchanged as one argument. shQuote() picks the quote style of
# the OS. A table name, a file name or a password can hold a space, a quote,
# `;` or `$(...)`. In quotes, none of these can split an argument or run a
# command.
run_load_tool <- function(command, args, timeout = 0) {
  output <- tryCatch(
    suppressWarnings(
      system2(
        command,
        args = shQuote(args),
        stdout = TRUE,
        stderr = TRUE,
        timeout = timeout
      )
    ),
    error = function(e) {
      return(structure(conditionMessage(e), status = 127L))
    }
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  return(list(status = as.integer(status), output = as.character(output)))
}

# Wait between two attempts. A test replaces this function, so the unit tests
# do not sleep.
retry_wait <- function(seconds) {
  Sys.sleep(seconds)
  return(invisible(NULL))
}

# Run a load client, check it, and retry a transient failure.
#
# Each failed attempt emits a message() that names the table, the client, the
# attempt and the failure class. The message also holds the output of the
# client after load_output_excerpt(), for every class. So the log shows the
# output also when a caller catches the final error. A load that succeeds on
# a later attempt emits one more. These are messages and not warnings,
# because a message never changes control flow. options(warn = 2) or a
# warning handler would turn a load that succeeded into a failure. message()
# also writes to stderr at once, so the line reaches the task log when it
# happens.
#
# The final failure stops with an error of class csdb_load_<class> and
# csdb_load_error. Its message holds the output of the client after
# load_output_excerpt(), and the error carries `table`, `tool`, `attempts` and
# `failure_class`.
#
# `table` holds one name for each table that the client loads.
run_checked_load <- function(
  command,
  args,
  tool,
  table,
  secrets = NULL,
  rows_sent = NULL,
  rows_pattern = NULL,
  error_pattern = NULL,
  timeout = 0
) {
  max_attempts <- length(retry_waits) + 1L
  words <- load_target_words(table)
  where <- words$where
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    result <- run_load_tool(command, args, timeout = timeout)
    timed_out <- timeout > 0 && result$status == 124L
    if (timed_out) {
      # Check the timeout before the output. A killed client may have printed
      # "Starting copy..." or nothing, and either would give a wrong class.
      reason <- list(text = paste0("timed out after ", timeout, " s"))
      failure_class <- "ambiguous"
    } else {
      # Parse the raw output. A password can be any text, such as "2" or
      # "Error", so redacting first could hide a row count or an error line.
      reason <- load_failure_reason(
        result,
        result$output,
        rows_sent = rows_sent,
        rows_pattern = rows_pattern,
        error_pattern = error_pattern,
        tables = table
      )
      if (is.null(reason)) {
        break
      }
      # A row count that differs after the client exited with 0: the server
      # may have committed rows, so the load is ambiguous, not retried.
      failure_class <- if (reason$classify) {
        classify_load_failure(result$output)
      } else {
        "ambiguous"
      }
    }
    printed <- load_output_excerpt(result$output, secrets)
    retry <- failure_class == "transient" && attempt < max_attempts
    message(sprintf(
      "csdb: loading data into %s with %s failed on attempt %d of at most %d (%s). %s",
      where,
      tool,
      attempt,
      max_attempts,
      failure_class,
      paste0(
        if (retry) {
          paste0("Retrying in ", retry_waits[[attempt]], " s.")
        } else {
          "csdb does not retry it."
        },
        " It printed:\n",
        printed
      )
    ))
    if (!retry) {
      stop(errorCondition(
        paste0(
          "Loading data into ",
          where,
          " failed. ",
          tool,
          " ",
          reason$text,
          ". Its exit status was ",
          result$status,
          ". The failure class is ",
          failure_class,
          ", after ",
          attempt,
          if (attempt == 1L) " attempt" else " attempts",
          ".",
          if (failure_class == "ambiguous") {
            count_the_rows_text(timed_out, reason, tool, where)
          },
          if (failure_class == "ambiguous") {
            paste0(
              " csdb does not run an ambiguous load again. If ",
              where,
              words$has,
              " no primary key and you run the load again by hand,",
              words$the_table,
              " can get duplicate rows."
            )
          },
          " It printed:\n",
          printed
        ),
        class = c(
          paste0("csdb_load_", gsub(" ", "_", failure_class, fixed = TRUE)),
          "csdb_load_error"
        ),
        table = table,
        tool = tool,
        attempts = attempt,
        failure_class = failure_class
      ))
    }
    retry_wait(retry_waits[[attempt]])
  }
  if (attempt > 1L) {
    message(sprintf(
      "csdb: loading data into %s with %s succeeded on attempt %d of at most %d.",
      where,
      tool,
      attempt,
      max_attempts
    ))
  }
  return(invisible(TRUE))
}
