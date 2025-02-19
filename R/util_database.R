random_uuid <- function() {
  x <- uuid::UUIDgenerate(F)
  x <- gsub("-", "", x)
  # the second part here allows for the usage of set.seed()
  x <- paste0("a", x, round(runif(1)*10000000))
  x
}

random_file <- function(folder, extension = ".csv", extra_insert = NULL) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  fs::path(folder, paste0(random_uuid(), extra_insert, extension))
}


write_data_infile <- function(
    dt,
    file = paste0(tempfile(), ".csv"),
    colnames = T,
    eol = "\n",
    quote = "auto",
    na = "\\N",
    sep = ","
) {
  # infinites and NANs get written as text
  # which destroys the upload
  # we need to set them to NA
  for (i in names(dt)) {
    dt[is.infinite(get(i)), (i) := NA]
    dt[is.nan(get(i)), (i) := NA]
    if (inherits(dt[[i]], "POSIXt")) dt[, (i) := as.character(get(i))]
  }
  fwrite(dt,
         file = file,
         logical01 = T,
         na = na,
         col.names = colnames,
         eol = eol,
         quote = quote,
         sep = sep
  )
}

load_data_infile <- function(
    connection,
    dbconfig,
    table,
    dt,
    file,
    force_tablock
) {
  UseMethod("load_data_infile")
}

load_data_infile.default <- function(
    connection = NULL,
    dbconfig = NULL,
    table,
    dt = NULL,
    file = "/xtmp/x123.csv",
    force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  t0 <- Sys.time()

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) dt <- dt[, correct_order, with = F]
  write_data_infile(dt = dt, file = file)
  on.exit(unlink(file), add = T)

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- T
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  sql <- paste0(
    "LOAD DATA INFILE ", DBI::dbQuoteString(connection, path), "\n",
    "INTO TABLE ", DBI::dbQuoteIdentifier(connection, table), "\n",
    "CHARACTER SET utf8", "\n",
    "FIELDS TERMINATED BY ", DBI::dbQuoteString(connection, sep), "\n",
    "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(connection, quote), "\n",
    "LINES TERMINATED BY ", DBI::dbQuoteString(connection, eol), "\n",
    "IGNORE ", skip + as.integer(header), " LINES \n",
    "(", paste0(correct_order, collapse = ","), ")"
  )
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  invisible()
}

`load_data_infile.Microsoft SQL Server` <- function(
    connection = NULL,
    dbconfig = NULL,
    table,
    dt,
    file = tempfile(),
    force_tablock = FALSE
) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  a <- Sys.time()

  # dont do a validation check if running in parallel
  # because there will be race conditions with different
  # instances competing
  # if (!config$in_parallel & interactive()) {
  #   sql <- glue::glue("SELECT COUNT(*) FROM {table};")
  #   n_before <- DBI::dbGetQuery(conn, sql)[1, 1]
  # }

  correct_order <- DBI::dbListFields(connection, table)
  if (length(correct_order) > 0) dt <- dt[, correct_order, with = F]
  write_data_infile(
    dt = dt,
    file = file,
    colnames = F,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )
  on.exit(unlink(file), add = T)

  format_file <- tempfile(tmpdir = tempdir(check = TRUE))
  on.exit(unlink(format_file), add = T)

  args <- c(
    table,
    "format",
    "nul",
    "-q",
    "-c",
    # "-t,",
    "-f",
    format_file,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  # TABLOCK is used by bcp by default. It is disabled when performing inserts in parallel.
  # Upserts can use TABLOCK in parallel, because they initially insert to a random table
  # before merging. This random db table will therefore not be in use by multiple processes
  # simultaneously
  #if (!config$in_parallel | force_tablock) {
  if(FALSE){
    # sometimes this results in the data not being
    # uploaded at all, so for the moment I am disabling this
    # until we can spend more time on it
    # hint_arg <- "TABLOCK"
    hint_arg <- NULL
  } else {
    hint_arg <- NULL
  }

  if (!is.null(key(dt))) {
    hint_arg <- c(hint_arg, paste0("ORDER(", paste0(key(dt), " ASC", collapse = ", "), ")"))
  }
  if (length(hint_arg) > 0) {
    hint_arg <- paste0(hint_arg, collapse = ", ")
    hint_arg <- paste0("-h '", hint_arg, "'")
  }

  args <- c(
    table,
    "in",
    file,
    "-a 16384",
    hint_arg,
    "-S",
    dbconfig$server,
    "-d",
    dbconfig$db,
    "-U",
    dbconfig$user,
    "-P",
    dbconfig$password,
    "-f",
    format_file,
    "-m",
    0
  )
  if (dbconfig$trusted_connection == "yes") {
    args <- c(args, "-T")
  }
  # print(args)
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  # dont do a validation check if running in parallel
  # because there will be race conditions with different
  # instances competing
  #if (!config$in_parallel & interactive()) {
  if(FALSE){
    sql <- glue::glue("SELECT COUNT(*) FROM {table};")
    n_after <- DBI::dbGetQuery(conn, sql)[1, 1]
    n_inserted <- n_after - n_before

    if (n_inserted != nrow(dt)) stop("Wanted to insert ", nrow(dt), " rows but only inserted ", n_inserted)
  }
  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  #update_config_last_updated(type = "data", tag = table)

  invisible()
}

`load_data_infile.PostgreSQL` <- function(
    connection = NULL,
    dbconfig = NULL,
    table,
    dt,
    file = tempfile(),
    force_tablock = FALSE
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
    dt <- dt[, correct_order, with = F]
  }

  write_data_infile(
    dt = dt,
    file = file,
    colnames = F,
    eol = "\n",
    quote = FALSE,
    na = "",
    sep = "\t"
  )

  on.exit(unlink(file), add = T)

  sql <- sprintf(
    "\"\\copy %s (%s) from '%s' (FORMAT CSV, DELIMITER '\t')\"",
    table_text,
    paste(correct_order, collapse = ","),
    file
  )

  uri <- sprintf(
    "postgresql://%s:%s@%s:%s/%s",
    dbconfig$user,
    dbconfig$password,
    dbconfig$server,
    dbconfig$port,
    dbconfig$db
  )

  args <- c(
    "-U",
    dbconfig$user,
    "-c",
    sql,
    uri
  )

  system2(
    "psql",
    args = args,
    stdout = FALSE
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Uploaded {nrow(dt)} rows in {dif} seconds to {table}"))

  invisible()
}

######### upsert_load_data_infile

upsert_load_data_infile <- function(
    connection,
    dbconfig,
    table,
    dt,
    file,
    fields,
    keys,
    drop_indexes){
  UseMethod("upsert_load_data_infile")
}

upsert_load_data_infile.default <- function(
    connection = NULL,
    dbconfig = NULL,
    table,
    dt,
    file = "/tmp/x123.csv",
    fields,
    keys = NULL,
    drop_indexes = NULL
) {
  temp_name <- random_uuid()
  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(connection, sql)

  # TO SPEED UP EFFICIENCY DROP ALL INDEXES HERE
  if (!is.null(drop_indexes)) {
    for (i in drop_indexes) {
      try(
        DBI::dbExecute(
          connection,
          glue::glue("ALTER TABLE `{temp_name}` DROP INDEX `{i}`")
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
    file = file
  )

  t0 <- Sys.time()

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue("
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    ")
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  invisible()
}

`upsert_load_data_infile.Microsoft SQL Server` <- function(
    connection,
    dbconfig,
    table,
    dt,
    file = tempfile(),
    fields,
    keys,
    drop_indexes = NULL
) {
  # conn <- schema$output$conn
  # db_config <- config$db_config
  # table <- schema$output$db_table
  # dt <- data_clean
  # file <- tempfile()
  # fields <- schema$output$db_fields
  # keys <- schema$output$keys
  # drop_indexes <- NULL

  temp_name <- paste0("tmp", random_uuid())

  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE 1 = 0;")
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE
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

  sql_on_keys <- glue::glue("{t} = {s}", t = paste0("t.", keys), s = paste0("s.", keys))
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  sql_update_set <- glue::glue("{t} = {s}", t = paste0("t.", fields), s = paste0("s.", fields))
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue("
  MERGE {table} t
  USING {temp_name} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED BY TARGET
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  ")

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  #update_config_last_updated(type = "data", tag = table)
  invisible()
}

upsert_load_data_infile.PostgreSQL <- function(
    connection,
    dbconfig,
    table,
    dt,
    file = tempfile(),
    fields,
    keys,
    drop_indexes = NULL
) {

  temp_name <- DBI::Id(schema = table@name[["schema"]], paste0("tmp", random_uuid()))
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)
  table_text <- DBI::dbQuoteIdentifier(connection, table)

  # ensure that the table is removed **FIRST** (before deleting the connection)
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("SELECT * INTO {temp_name_text} FROM {table_text} WHERE 1 = 0;")
  DBI::dbExecute(connection, sql)

  load_data_infile(
    connection = connection,
    dbconfig = dbconfig,
    table = temp_name,
    dt = dt,
    file = file,
    force_tablock = TRUE
  )

  a <- Sys.time()
  add_index(
    connection = connection,
    table = temp_name,
    keys = keys,
    index = "ind" + random_uuid()
  )

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql_on_keys <- glue::glue("{t} = {s}", t = paste0("t.", keys), s = paste0("s.", keys))
  sql_on_keys <- paste0(sql_on_keys, collapse = " and ")

  update_fields <- setdiff(fields, keys)
  sql_update_set <- glue::glue("{t} = {s}", t = update_fields, s = paste0("s.", update_fields))
  sql_update_set <- paste0(sql_update_set, collapse = ", ")

  sql_insert_fields <- paste0(fields, collapse = ", ")
  sql_insert_s_fields <- paste0(paste0("s.", fields), collapse = ", ")

  sql <- glue::glue("
  MERGE INTO {table_text} t
  USING {temp_name_text} s
  ON ({sql_on_keys})
  WHEN MATCHED
  THEN UPDATE SET
    {sql_update_set}
  WHEN NOT MATCHED
  THEN INSERT ({sql_insert_fields})
    VALUES ({sql_insert_s_fields});
  ")

  DBI::dbExecute(connection, sql)

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Upserted {nrow(dt)} rows in {dif} seconds from {temp_name} to {table}"))

  invisible()
}

######### create_table
create_table <- function(connection, table, fields, keys, ...) UseMethod("create_table")

create_table.default <- function(connection, table, fields, keys = NULL, ...) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"

  sql <- DBI::sqlCreateTable(connection, table, fields_new,
                             row.names = F, temporary = F
  )
  DBI::dbExecute(connection, sql)
}

`create_table.Microsoft SQL Server` <- function(connection, table, fields, keys = NULL, ...) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "NVARCHAR (1000)"
  fields_new[fields == "DOUBLE"] <- "FLOAT"
  fields_new[fields == "BOOLEAN"] <- "BIT"

  if (!is.null(keys)) fields_new[names(fields_new) %in% keys] <- paste0(fields_new[names(fields_new) %in% keys], " NOT NULL")

  sql <- DBI::sqlCreateTable(
    connection,
    table,
    fields_new,
    row.names = F,
    temporary = F
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")
  DBI::dbExecute(connection, sql)
}

`create_table.PostgreSQL` <- function(connection, table, fields, keys = NULL, role_create_table = NULL) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "VARCHAR"
  fields_new[fields == "DOUBLE"] <- "REAL"
  fields_new[fields == "BOOLEAN"] <- "BIT"
  fields_new[fields == "DATETIME"] <- "TIMESTAMP"

  if (!is.null(keys)) fields_new[names(fields_new) %in% keys] <- paste0(fields_new[names(fields_new) %in% keys], " NOT NULL")

  sql <- DBI::sqlCreateTable(
    connection,
    table,
    fields_new,
    row.names = F,
    temporary = F
  ) |>
    stringr::str_replace("\\\\", "\\") |>
    stringr::str_replace("\"", "") |>
    stringr::str_replace("\"", "")
  # print(sql)

  # include set role if appropriate
  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }

  DBI::dbExecute(connection, sql)
}

######### add_constraint
add_constraint <- function(connection, table, keys) UseMethod("add_constraint")

add_constraint.default <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_remove_all("\\[") %>%
    stringr::str_remove_all("]")
  sql <- glue::glue("
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});")
  # print(sql)
  a <- DBI::dbExecute(connection, sql)
  # DBI::dbExecute(connection, "SHOW INDEX FROM x");
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Added constraint {constraint} in {dif} seconds to {table}"))
}

add_constraint.PostgreSQL <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_remove_all("\\[") %>%
    stringr::str_remove_all("]")
  sql <- glue::glue(
    "ALTER table {table}
    ADD CONSTRAINT {constraint}
    PRIMARY KEY ({primary_keys});"
  )

  a <- DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Added constraint {constraint} in {dif} seconds to {table}"))
}

######### drop_constraint
drop_constraint <- function(connection, table) UseMethod("drop_constraint")

drop_constraint.default <- function(connection, table) {
  constraint <- glue::glue("PK_{table}") %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_remove_all("\\[") %>%
    stringr::str_remove_all("]")
  sql <- glue::glue("
          ALTER table {table}
          DROP CONSTRAINT {constraint};")
  # print(sql)
  try(a <- DBI::dbExecute(connection, sql), TRUE)
}

get_indexes <- function(connection, table) UseMethod("get_indexes")

`get_indexes.Microsoft SQL Server` <- function(connection, table){
  index_name <- NULL
  table_name <- NULL

  table_rows <- connection %>%
    DBI::dbGetQuery("select o.name as table_name, i.name as index_name from sys.objects o join sys.sysindexes i on o.object_id = i.id where o.is_ms_shipped = 0 and i.rowcnt > 0 order by o.name") %>%
    dplyr::filter(!is.na(index_name) & !stringr::str_detect(index_name, "^PK")) %>%
    setDT()
  retval <- table_rows[table_name %in% table]$index_name
  return(retval)
}

get_indexes.PostgreSQL <- function(connection, table){
  index_name <- NULL
  table_name <- NULL

  sql <- "
    select tablename, indexname
    from pg_indexes
  "

  table_rows <- connection %>%
    DBI::dbGetQuery(sql) %>%
    dplyr::filter(!is.na(indexname) & !stringr::str_detect(indexname, "^pk")) %>%
    setDT()
  retval <- table_rows[tablename %in% table]$indexname
  return(retval)
}

drop_index <- function(connection, table, index) UseMethod("drop_index")

drop_index.default <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("ALTER TABLE `{table}` DROP INDEX `{index}`")
    ),
    TRUE
  )
}

`drop_index.Microsoft SQL Server` <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX {table}.{index}")
    ),
    TRUE
  )
}

drop_index.PostgreSQL <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX IF EXISTS {index}")
    ),
    TRUE
  )
}

add_index <- function(connection, table, index, keys) UseMethod("add_index")

add_index.default <- function(connection, table, keys, index) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  sql <- glue::glue("
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;")
  # print(sql)
  try(a <- DBI::dbExecute(connection, sql), T)
}

`add_index.Microsoft SQL Server` <- function(connection, table, keys, index) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX {index} IF NOT EXISTS ON {table} ({keys});")
    ),
    T
  )
}

add_index.PostgreSQL <- function(connection, table, keys, index) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX IF NOT EXISTS {index} ON {table} ({keys});")
    ),
    T
  )
}

drop_all_rows <- function(connection, table) {

  a <- DBI::dbExecute(connection, glue::glue({
    "TRUNCATE TABLE {table};"
  }))

  #update_config_last_updated(type = "data", tag = table)
}

#' Drops the rows where the condition is met
#' @param connection A db connection
#' @param table Table name
#' @param condition A string SQL condition
#' @export

drop_rows_where <- function(connection, table, condition) UseMethod("drop_rows_where")

`drop_rows_where.Microsoft SQL Server` <- function(connection, table, condition) {
  t0 <- Sys.time()

  # find out how many rows to delete
  numrows <- DBI::dbGetQuery(connection, glue::glue(
    "SELECT COUNT(*) FROM {table} WHERE {condition};"
  )) %>%
    as.numeric()
  # message(numrows, " rows remaining to be deleted")

  num_deleting <- 100000
  # need to do this, so that we dont get scientific format in the SQL command
  num_deleting_character <- formatC(num_deleting, format = "f", drop0trailing = T)
  num_delete_calls <- ceiling(numrows / num_deleting)
  # message("We will need to perform ", num_delete_calls, " delete calls of ", num_deleting_character, " rows each.")

  indexes <- csutil::easy_split(1:num_delete_calls, number_of_groups = 10)
  notify_indexes <- unlist(lapply(indexes, max))

  i <- 0
  while (numrows > 0) {

    # delete a large number of rows
    # database must be in SIMPLE recovery mode
    # "ALTER DATABASE sykdomspulsen_surv SET RECOVERY SIMPLE;"
    # checkpointing will ensure transcation log is cleared after each delete operation
    # http://craftydba.com/?p=3079
    #
    #

    b <- DBI::dbExecute(connection, glue::glue(
      "DELETE TOP ({num_deleting_character}) FROM {table} WHERE {condition}; ",
      "CHECKPOINT; "
    ))

    numrows <- DBI::dbGetQuery(connection, glue::glue(
      "SELECT COUNT(*) FROM {table} WHERE {condition};"
    )) %>%
      as.numeric()
    i <- i + 1
    # if (i %in% notify_indexes) message(i, "/", num_delete_calls, " delete calls performed. ", numrows, " rows remaining to be deleted")
  }

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  # if (config$verbose) message(glue::glue("Deleted rows in {dif} seconds from {table}"))

  #update_config_last_updated(type = "data", tag = table)
}

drop_rows_where.PostgreSQL <- function(connection, table, condition) {
  # If there is a need to switch to dropping only a fixed number of rows per call, the syntax is:
  # DBI::dbExecute(connection, glue::glue(
  #   "DELETE FROM {table}
  #   WHERE ctid IN (
  #     SELECT ctid
  #     FROM {table}
  #     WHERE {condition}
  #     LIMIT {num_deleting}
  #   ); "
  #))
  t0 <- Sys.time()

  sql <- glue::glue("delete from {table} where {condition};")

  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Kept rows in {dif} seconds from {table}"))
}

keep_rows_where <- function(connection, table, condition) UseMethod("keep_rows_where")

`keep_rows_where.Microsoft SQL Server` <- function(connection, table, condition) {

  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  DBI::dbExecute(connection, sql)

  DBI::dbRemoveTable(connection, name = table)

  sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table}'")
  DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Kept rows in {dif} seconds from {table}"))

  #update_config_last_updated(type = "data", tag = table)
}

keep_rows_where.PostgreSQL <- function(connection, table, condition) {

  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  DBI::dbExecute(connection, sql)

  DBI::dbRemoveTable(connection, name = table)

  sql <- glue::glue("ALTER TABLE {temp_name} RENAME TO {table}")
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
  #if (config$verbose) message(glue::glue("Kept rows in {dif} seconds from {table}"))
}

list_indexes <- function(connection, table) {
  retval <- DBI::dbGetQuery(
    connection,
    glue::glue("select * from sys.indexes where object_id = (select object_id from sys.objects where name = '{table}')")
  )
  return(retval)
}




#' drop_table
#' @param connection connection
#' @param table table
#' @export
drop_table <- function(connection, table) {
  return(try(DBI::dbRemoveTable(connection, name = table), TRUE))
}
