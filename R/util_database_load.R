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

  a <- Sys.time()

  table_text <- DBI::dbQuoteIdentifier(connection, table)

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

  # run_load_tool() quotes every argument, so this text reaches psql
  # unchanged. psql reads the file name between single quotes, and a single
  # quote inside it is written twice.
  sql <- sprintf(
    "\\copy %s (%s) from '%s' (FORMAT CSV, DELIMITER '\t')",
    table_text,
    paste(DBI::dbQuoteIdentifier(connection, correct_order), collapse = ", "),
    gsub("'", "''", file, fixed = TRUE)
  )

  # psql stops when it cannot connect within 30 s. The URI holds no password:
  # an argument shows in the process list, so psql reads the password from
  # PGPASSWORD. libpq decodes the percent-encoding of each URI part.
  uri_part <- function(x) {
    return(utils::URLencode(as.character(x), reserved = TRUE))
  }
  uri <- sprintf(
    "postgresql://%s@%s:%s/%s?connect_timeout=30",
    uri_part(dbconfig$user),
    uri_part(dbconfig$server),
    dbconfig$port,
    uri_part(dbconfig$db)
  )

  # VERBOSITY=verbose makes psql print the SQLSTATE of an error, which
  # classify_load_failure() reads. It sends no query.
  args <- c(
    "-v",
    "VERBOSITY=verbose",
    "-U",
    dbconfig$user,
    "-c",
    sql,
    uri
  )

  if (Sys.which("psql") == "") {
    stop(
      "psql command not found. Please install PostgreSQL command line tools.",
      call. = FALSE
    )
  }

  # The password hook, when one is set, gives the password for this load.
  password <- password_hook_value()
  if (is.null(password)) {
    password <- dbconfig$password
  }

  run_checked_load(
    command = "psql",
    args = args,
    tool = "psql \\copy",
    table = as.character(table_text),
    secrets = password,
    rows_sent = nrow(dt),
    rows_pattern = "^COPY ([0-9]+)\\s*$",
    timeout = load_timeout,
    env = c(PGPASSWORD = password)
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  return(invisible())
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

  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

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

# The external load clients: psql for PostgreSQL, bcp for SQL Server.
#
# Until 2026.10.3 both methods called system2() and ignored the result. A load
# that the database refused, such as a duplicate key, returned normally and
# lost the rows. bcp writes its errors to stdout, which was discarded.
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
# A retry is safe only because a failed load stores no row. psql \copy is one
# COPY statement, and bcp sends every row in one batch because csdb passes no
# -b. Both were verified against real servers on 2026-10-03.

# A line that bcp starts with "Error = " reports a refused row or batch.
bcp_error_pattern <- "^\\s*Error = "

# The waits before the second and the third attempt, in seconds.
load_retry_waits <- c(1, 3)

# psql prints the SQLSTATE because the load passes -v VERBOSITY=verbose, as
# "ERROR:  23505: duplicate key value ...". The SQLSTATE does not depend on
# the language of the server. A failed connection has no SQLSTATE, so the
# libpq texts below cover it. 53300 is too_many_connections. The server
# raises it while the connection starts, so psql prints only the text. psql
# 14.24 printed "FATAL:  sorry, too many clients already" (PostgreSQL 16).
pg_sqlstate_pattern <- "(^|\\s)\\S+:\\s+([0-9A-Z]{5}):\\s"

# psql starts every error that it meets while it connects with "psql: error: ".
# No row is sent then, so a failed connection is safe to retry. Any other
# error came while \copy ran. A broken connection there is ambiguous: the
# server may have committed the COPY before the client heard the result.
pg_connect_phase <- "^psql: error: "
pg_connect_retry_text <- paste0(
  "Connection refused|Connection timed out|Connection reset by peer|",
  "No route to host|could not translate host name|",
  "Temporary failure in name resolution|timeout expired|",
  "server closed the connection unexpectedly|",
  "the database system is (starting up|shutting down|in recovery mode)|",
  "sorry, too many clients already|remaining connection slots are reserved"
)
# The server rolls the statement back for these: serialization failure,
# deadlock, too many connections, lock timeout, and cannot connect now.
pg_retry_sqlstate <- "^(40001|40P01|53300|55P03|57P03)$"
# A lost connection, or a shutdown that ended the session: ambiguous.
pg_ambiguous_sqlstate <- "^(08[0-9A-Z]{3}|57P0[12])$"
pg_lost_text <- paste0(
  "server closed the connection unexpectedly|connection to server was lost|",
  "could not receive data from server|could not send data to server|",
  "SSL SYSCALL error|Connection reset by peer"
)

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

# Run one external load client and capture everything it prints.
#
# Every psql and bcp call in the load path goes through this function, so a
# test can replace it with testthat::local_mocked_bindings() and needs no
# server. psql writes its errors to stderr and bcp writes them to stdout, so
# both streams are captured.
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
#
# `env` is a named character vector of environment variables that the client
# needs, such as PGPASSWORD. run_load_tool() sets them only while the client
# runs. Afterwards it restores the old value, or the absence of one, also
# after an error.
# system2(env = ) is not used: on Windows it puts "NAME=value" into the
# arguments.
run_load_tool <- function(command, args, timeout = 0, env = NULL) {
  if (length(env) > 0) {
    old <- Sys.getenv(names(env), unset = NA, names = TRUE)
    on.exit(
      {
        was_set <- !is.na(old)
        if (any(was_set)) {
          do.call(Sys.setenv, as.list(old[was_set]))
        }
        if (any(!was_set)) {
          Sys.unsetenv(names(old)[!was_set])
        }
      },
      add = TRUE
    )
    do.call(Sys.setenv, as.list(env))
  }
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
load_retry_wait <- function(seconds) {
  Sys.sleep(seconds)
  return(invisible(NULL))
}

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
#     from it. psql prints "COPY <n>" and bcp prints "<n> rows copied.".
#
# The last two print no error, so the failure class is always "other".
load_failure_reason <- function(
  result,
  output,
  rows_sent = NULL,
  rows_pattern = NULL,
  error_pattern = NULL
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
  if (length(hits) == 0) {
    return(list(
      text = paste0(
        "did not report how many rows it stored, and csdb sent ",
        rows_sent,
        " rows"
      ),
      classify = FALSE
    ))
  }
  rows_stored <- as.numeric(hits[length(hits)])
  if (rows_stored != rows_sent) {
    return(list(
      text = paste0(
        "stored ",
        rows_stored,
        " rows, and csdb sent ",
        rows_sent,
        " rows"
      ),
      classify = FALSE
    ))
  }
  return(NULL)
}

# Sort the raw output of a failed psql or bcp call into "duplicate key",
# "ambiguous", "transient" or "other". A duplicate key comes first, because a
# retry cannot remove it. An ambiguous failure comes before a transient one,
# because only a load that certainly stored nothing may run again.
classify_load_failure <- function(command, output) {
  capture <- function(pattern, group) {
    m <- regmatches(output, regexec(pattern, output))
    return(vapply(m, `[`, character(1), group + 1L))
  }
  if (command == "psql") {
    return(classify_psql_failure(output, capture(pg_sqlstate_pattern, 2L)))
  }
  return(classify_bcp_failure(
    output,
    states = capture(mssql_state_pattern, 1L),
    natives = capture(mssql_state_pattern, 2L)
  ))
}

classify_psql_failure <- function(output, codes) {
  if (any(codes %in% "23505")) {
    return("duplicate key")
  }
  if (any(grepl(pg_connect_phase, output))) {
    if (any(grepl(pg_connect_retry_text, output))) {
      return("transient")
    }
    return("other")
  }
  if (
    any(grepl(pg_ambiguous_sqlstate, codes)) ||
      any(grepl(pg_lost_text, output))
  ) {
    return("ambiguous")
  }
  if (any(grepl(pg_retry_sqlstate, codes))) {
    return("transient")
  }
  return("other")
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

# Run a load client, check it, and retry a transient failure.
#
# Each failed attempt emits a message() that names the table, the client, the
# attempt and the failure class. A load that succeeds on a later attempt
# emits one more. These are messages and not warnings, because a message never
# changes control flow. options(warn = 2) or a warning handler would turn a
# load that succeeded into a failure. message() also writes to stderr at once,
# so the line reaches the task log when it happens.
#
# The final failure stops with an error of class csdb_load_<class> and
# csdb_load_error. Its message holds the output of the client after
# load_output_excerpt(), and the error carries `table`, `tool`, `attempts` and
# `failure_class`. `env` goes to run_load_tool().
run_checked_load <- function(
  command,
  args,
  tool,
  table,
  secrets = NULL,
  rows_sent = NULL,
  rows_pattern = NULL,
  error_pattern = NULL,
  timeout = 0,
  env = NULL
) {
  max_attempts <- length(load_retry_waits) + 1L
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    result <- run_load_tool(command, args, timeout = timeout, env = env)
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
        error_pattern = error_pattern
      )
      if (is.null(reason)) {
        break
      }
      failure_class <- if (reason$classify) {
        classify_load_failure(command, result$output)
      } else {
        "other"
      }
    }
    printed <- load_output_excerpt(result$output, secrets)
    retry <- failure_class == "transient" && attempt < max_attempts
    message(sprintf(
      "csdb: loading data into table %s with %s failed on attempt %d of at most %d (%s). %s",
      table,
      tool,
      attempt,
      max_attempts,
      failure_class,
      if (retry) {
        paste0(
          "Retrying in ",
          load_retry_waits[[attempt]],
          " s. It printed:\n",
          printed
        )
      } else {
        "csdb does not retry it."
      }
    ))
    if (!retry) {
      stop(errorCondition(
        paste0(
          "Loading data into table ",
          table,
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
          if (timed_out) {
            paste0(
              " csdb killed the client, so the server may or may not have",
              " committed the rows to table ",
              table,
              ". Count the rows in table ",
              table,
              " before you load these rows again."
            )
          } else if (failure_class == "ambiguous") {
            paste0(
              " The connection broke after the load started, so the server",
              " may or may not have committed the rows to table ",
              table,
              ". Count the rows in table ",
              table,
              " before you load these rows again."
            )
          },
          if (failure_class == "ambiguous") {
            paste0(
              " csdb does not run an ambiguous load again. If table ",
              table,
              " has no primary key and you run the load again by hand,",
              " the table can get duplicate rows."
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
    load_retry_wait(load_retry_waits[[attempt]])
  }
  if (attempt > 1L) {
    message(sprintf(
      "csdb: loading data into table %s with %s succeeded on attempt %d of at most %d.",
      table,
      tool,
      attempt,
      max_attempts
    ))
  }
  return(invisible(TRUE))
}
