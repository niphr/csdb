# Database Utilities for csdb package
# This file contains S7 database methods and database-specific utility functions

# Database utility functions

#' Write data.table to file for database bulk insert
#'
#' Internal function to write data.table to CSV file with proper formatting
#' for database bulk insert operations. Handles special cases like infinites,
#' NaNs, and POSIXt objects.
#'
#' @param dt data.table to write
#' @param file Output file path
#' @param colnames Logical indicating whether to include column names
#' @param eol End of line character
#' @param quote Quoting behavior
#' @param na String to use for NA values
#' @param sep Column separator
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
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

#' Truncate all rows from a database table
#'
#' Removes all rows from a database table using TRUNCATE TABLE command.
#' This is more efficient than DELETE for removing all rows.
#'
#' @param connection Database connection object
#' @param table Name of the table to truncate
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
drop_all_rows <- function(connection, table) {
  a <- DBI::dbExecute(connection, glue::glue({
    "TRUNCATE TABLE {table};"
  }))
}

#' List indexes for a database table
#'
#' Internal function to list all indexes for a specific table.
#' Currently only supports Microsoft SQL Server.
#'
#' @param connection Database connection object
#' @param table Name of the table to list indexes for
#' @return data.frame with index information
#' @keywords internal
#' @noRd
list_indexes <- function(connection, table) {
  retval <- DBI::dbGetQuery(
    connection,
    glue::glue("select * from sys.indexes where object_id = (select object_id from sys.objects where name = '{table}')")
  )
  return(retval)
}

# S7 classes for database connections - register S4 classes for S7 dispatch
# This allows S7 to dispatch on actual S4 connection objects  
# We need to register the S4 classes with S7 first, then create methods for them

# Helper function to safely register S4 classes for S7 dispatch
register_s4_classes <- function() {
  # Register S4 classes from odbc package if available
  tryCatch({
    # Ensure odbc package is available before trying to register classes
    if (requireNamespace("odbc", quietly = TRUE)) {
      # Check if PostgreSQL S4 class exists in odbc package and register it
      if (methods::isClass("PostgreSQL") && 
          methods::getClass("PostgreSQL")@package == "odbc") {
        S7::S4_register(methods::getClass("PostgreSQL"))
      }
      
      # Check if Microsoft SQL Server S4 class exists and register it
      if (methods::isClass("Microsoft SQL Server") && 
          methods::getClass("Microsoft SQL Server")@package == "odbc") {
        S7::S4_register(methods::getClass("Microsoft SQL Server"))
      }
    }
  }, error = function(e) {
    # Silently continue if odbc package not available
    NULL
  })
  
  # Register DBI classes if available
  tryCatch({
    if (requireNamespace("DBI", quietly = TRUE)) {
      if (methods::isClass("DBIConnection")) {
        S7::S4_register(methods::getClass("DBIConnection"))
      }
    }
  }, error = function(e) NULL)
}

# Helper function to get or create database class objects for S7 dispatch
get_db_classes <- function() {
  # Try to get actual S4 classes first, fall back to S3 wrappers
  
  # PostgreSQL class
  db_postgres <- tryCatch({
    if (requireNamespace("odbc", quietly = TRUE) && 
        methods::isClass("PostgreSQL") &&
        methods::getClass("PostgreSQL")@package == "odbc") {
      methods::getClass("PostgreSQL")
    } else {
      S7::new_S3_class("PostgreSQL")
    }
  }, error = function(e) S7::new_S3_class("PostgreSQL"))
  
  # Microsoft SQL Server class
  db_mssql <- tryCatch({
    if (requireNamespace("odbc", quietly = TRUE) && 
        methods::isClass("Microsoft SQL Server") &&
        methods::getClass("Microsoft SQL Server")@package == "odbc") {
      methods::getClass("Microsoft SQL Server")
    } else {
      S7::new_S3_class("Microsoft SQL Server")
    }
  }, error = function(e) S7::new_S3_class("Microsoft SQL Server"))
  
  # Default/DBI class
  db_default <- tryCatch({
    if (requireNamespace("DBI", quietly = TRUE) && 
        methods::isClass("DBIConnection")) {
      methods::getClass("DBIConnection")
    } else {
      S7::new_S3_class("DBIConnection")
    }
  }, error = function(e) S7::new_S3_class("DBIConnection"))
  
  return(list(
    postgres = db_postgres,
    mssql = db_mssql,
    default = db_default
  ))
}

# Public function to re-register database methods
# This can be called if database connections are not working properly
#' Re-register database S7 methods
#' 
#' Re-registers S7 methods for database operations. Call this function if you 
#' encounter method dispatch errors with database connections.
#' 
#' @return NULL (called for side effects)
#' @keywords internal
#' @noRd
refresh_database_methods <- function() {
  # Re-register S4 classes
  register_s4_classes()
  
  # Re-get database class objects
  db_classes <- get_db_classes()
  db_postgres <<- db_classes$postgres
  db_mssql <<- db_classes$mssql
  db_default <<- db_classes$default
  
  # Re-register S7 methods
  S7::methods_register()
  
  message("Database methods refreshed. S4 classes re-registered and S7 methods updated.")
  invisible(NULL)
}

# Debug function to show method dispatch information
#' Debug database method dispatch
#' 
#' Shows information about registered classes and methods for debugging
#' method dispatch issues.
#' 
#' @param connection Optional database connection object to check
#' @return List with debugging information
#' @keywords internal
#' @noRd
debug_database_methods <- function(connection = NULL) {
  info <- list()
  
  # Check available packages
  info$packages <- list(
    odbc_available = requireNamespace("odbc", quietly = TRUE),
    DBI_available = requireNamespace("DBI", quietly = TRUE),
    S7_available = requireNamespace("S7", quietly = TRUE)
  )
  
  # Check registered S4 classes
  info$s4_classes <- list()
  if (info$packages$odbc_available) {
    info$s4_classes$PostgreSQL <- tryCatch({
      if (methods::isClass("PostgreSQL")) {
        list(
          exists = TRUE,
          package = methods::getClass("PostgreSQL")@package
        )
      } else {
        list(exists = FALSE)
      }
    }, error = function(e) list(error = e$message))
    
    info$s4_classes$MicrosoftSQLServer <- tryCatch({
      if (methods::isClass("Microsoft SQL Server")) {
        list(
          exists = TRUE,
          package = methods::getClass("Microsoft SQL Server")@package
        )
      } else {
        list(exists = FALSE)
      }
    }, error = function(e) list(error = e$message))
  }
  
  # Check connection object if provided
  if (!is.null(connection)) {
    info$connection <- list(
      class = class(connection),
      is_s4 = methods::is(connection, "S4"),
      package = attr(class(connection), "package")
    )
  }
  
  # Check current database class objects
  info$db_classes <- list(
    postgres_type = class(db_postgres),
    mssql_type = class(db_mssql),
    default_type = class(db_default)
  )
  
  return(info)
}

# Register S4 classes and get database class objects
register_s4_classes()
db_classes <- get_db_classes()
db_postgres <- db_classes$postgres
db_mssql <- db_classes$mssql
db_default <- db_classes$default

# S7 generic definitions (internal use only)
load_data_infile <- S7::new_generic("load_data_infile", "connection")
upsert_load_data_infile <- S7::new_generic("upsert_load_data_infile", "connection")
create_table <- S7::new_generic("create_table", "connection")
add_constraint <- S7::new_generic("add_constraint", "connection")
drop_constraint <- S7::new_generic("drop_constraint", "connection")
get_indexes <- S7::new_generic("get_indexes", "connection")
drop_index <- S7::new_generic("drop_index", "connection")
add_index <- S7::new_generic("add_index", "connection")
drop_rows_where <- S7::new_generic("drop_rows_where", "connection")
keep_rows_where <- S7::new_generic("keep_rows_where", "connection")
drop_table <- S7::new_generic("drop_table", "connection")

# S7 method definitions
# load_data_infile methods
S7::method(load_data_infile, db_default) <- function(connection, 
                                                       dbconfig = NULL, 
                                                       table, 
                                                       dt = NULL, 
                                                       file = "/xtmp/x123.csv", 
                                                       force_tablock = FALSE) {
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

  invisible()
}

S7::method(load_data_infile, db_mssql) <- function(connection, 
                                                     dbconfig = NULL, 
                                                     table, 
                                                     dt, 
                                                     file = tempfile(), 
                                                     force_tablock = FALSE) {
  if (is.null(dt)) {
    return()
  }
  if (nrow(dt) == 0) {
    return()
  }

  a <- Sys.time()

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
  
  if (Sys.which("bcp") == "") {
    stop("bcp command not found. Please install SQL Server command line tools.")
  }
  
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  if(FALSE){
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
  
  if (Sys.which("bcp") == "") {
    stop("bcp command not found. Please install SQL Server command line tools.")
  }
  
  system2(
    "bcp",
    args = args,
    stdout = NULL
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

S7::method(load_data_infile, db_postgres) <- function(connection, 
                                                        dbconfig = NULL, 
                                                        table, 
                                                        dt, 
                                                        file = tempfile(), 
                                                        force_tablock = FALSE) {
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

  if (Sys.which("psql") == "") {
    stop("psql command not found. Please install PostgreSQL command line tools.")
  }

  system2(
    "psql",
    args = args,
    stdout = FALSE
  )

  b <- Sys.time()
  dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)

  invisible()
}

# Continue with upsert_load_data_infile methods
S7::method(upsert_load_data_infile, db_default) <- function(connection, 
                                                              dbconfig = NULL, 
                                                              table, 
                                                              dt, 
                                                              file = "/tmp/x123.csv", 
                                                              fields, 
                                                              keys = NULL, 
                                                              drop_indexes = NULL) {
  temp_name <- random_uuid()
  on.exit(DBI::dbRemoveTable(connection, temp_name), add = TRUE, after = FALSE)

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(connection, sql)

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

  invisible()
}

S7::method(upsert_load_data_infile, db_mssql) <- function(connection, 
                                                            dbconfig, 
                                                            table, 
                                                            dt, 
                                                            file = tempfile(), 
                                                            fields, 
                                                            keys, 
                                                            drop_indexes = NULL) {
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

  invisible()
}

S7::method(upsert_load_data_infile, db_postgres) <- function(connection, 
                                                               dbconfig, 
                                                               table, 
                                                               dt, 
                                                               file = tempfile(), 
                                                               fields, 
                                                               keys, 
                                                               drop_indexes = NULL) {
  temp_name <- DBI::Id(schema = table@name[["schema"]], paste0("tmp", random_uuid()))
  temp_name_text <- DBI::dbQuoteIdentifier(connection, temp_name)
  table_text <- DBI::dbQuoteIdentifier(connection, table)

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

  invisible()
}

# create_table methods
S7::method(create_table, db_default) <- function(connection, table, fields, keys = NULL, role_create_table = NULL, ...) {
  fields_new <- fields
  fields_new[fields == "TEXT"] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"

  sql <- DBI::sqlCreateTable(connection, table, fields_new,
                             row.names = F, temporary = F
  )
  DBI::dbExecute(connection, sql)
}

S7::method(create_table, db_mssql) <- function(connection, table, fields, keys = NULL, role_create_table = NULL, ...) {
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

S7::method(create_table, db_postgres) <- function(connection, table, fields, keys = NULL, role_create_table = NULL, ...) {
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

  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }

  DBI::dbExecute(connection, sql)
}

# add_constraint methods
S7::method(add_constraint, db_default) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue("
          ALTER table {table}
          ADD CONSTRAINT {constraint} PRIMARY KEY CLUSTERED ({primary_keys});")
  a <- DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(add_constraint, db_postgres) <- function(connection, table, keys) {
  t0 <- Sys.time()

  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue(
    "ALTER table {table}
    ADD CONSTRAINT {constraint}
    PRIMARY KEY ({primary_keys});"
  )

  a <- DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

# drop_constraint methods
S7::method(drop_constraint, db_default) <- function(connection, table) {
  constraint <- glue::glue("PK_{table}") |>
    stringr::str_remove_all("\\.") |>
    stringr::str_remove_all("\\[") |>
    stringr::str_remove_all("]")
  sql <- glue::glue("
          ALTER table {table}
          DROP CONSTRAINT {constraint};")
  try(a <- DBI::dbExecute(connection, sql), TRUE)
}

# get_indexes methods
S7::method(get_indexes, db_mssql) <- function(connection, table){
  index_name <- NULL
  table_name <- NULL

  table_rows <- connection |>
    DBI::dbGetQuery("select o.name as table_name, i.name as index_name from sys.objects o join sys.sysindexes i on o.object_id = i.id where o.is_ms_shipped = 0 and i.rowcnt > 0 order by o.name") |>
    dplyr::filter(!is.na(index_name) & !stringr::str_detect(index_name, "^PK")) |>
    setDT()
  retval <- table_rows[table_name %in% table]$index_name
  return(retval)
}

S7::method(get_indexes, db_postgres) <- function(connection, table){
  index_name <- NULL
  table_name <- NULL

  sql <- "
    select tablename, indexname
    from pg_indexes
  "

  table_rows <- connection |>
    DBI::dbGetQuery(sql) |>
    dplyr::filter(!is.na(indexname) & !stringr::str_detect(indexname, "^pk")) |>
    setDT()
  retval <- table_rows[tablename %in% table]$indexname
  return(retval)
}

# drop_index methods
S7::method(drop_index, db_default) <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("ALTER TABLE `{table}` DROP INDEX `{index}`")
    ),
    TRUE
  )
}

S7::method(drop_index, db_mssql) <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX {table}.{index}")
    ),
    TRUE
  )
}

S7::method(drop_index, db_postgres) <- function(connection, table, index) {
  try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX IF EXISTS {index}")
    ),
    TRUE
  )
}

# add_index methods
S7::method(add_index, db_default) <- function(connection, table, index, keys) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  sql <- glue::glue("
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;")
  try(a <- DBI::dbExecute(connection, sql), T)
}

S7::method(add_index, db_mssql) <- function(connection, table, index, keys) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX {index} IF NOT EXISTS ON {table} ({keys});")
    ),
    T
  )
}

S7::method(add_index, db_postgres) <- function(connection, table, index, keys) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX IF NOT EXISTS {index} ON {table} ({keys});")
    ),
    T
  )
}

# drop_rows_where methods
S7::method(drop_rows_where, db_mssql) <- function(connection, table, condition) {
  t0 <- Sys.time()

  numrows <- DBI::dbGetQuery(connection, glue::glue(
    "SELECT COUNT(*) FROM {table} WHERE {condition};"
  )) |>
    as.numeric()

  num_deleting <- 100000
  num_deleting_character <- formatC(num_deleting, format = "f", drop0trailing = T)
  num_delete_calls <- ceiling(numrows / num_deleting)

  indexes <- csutil::easy_split(1:num_delete_calls, number_of_groups = 10)
  notify_indexes <- unlist(lapply(indexes, max))

  i <- 0
  while (numrows > 0) {
    b <- DBI::dbExecute(connection, glue::glue(
      "DELETE TOP ({num_deleting_character}) FROM {table} WHERE {condition}; ",
      "CHECKPOINT; "
    ))

    numrows <- DBI::dbGetQuery(connection, glue::glue(
      "SELECT COUNT(*) FROM {table} WHERE {condition};"
    )) |>
      as.numeric()
    i <- i + 1
  }

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(drop_rows_where, db_postgres) <- function(connection, table, condition) {
  t0 <- Sys.time()

  sql <- glue::glue("delete from {table} where {condition};")

  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

# keep_rows_where methods
S7::method(keep_rows_where, db_mssql) <- function(connection, table, condition, role_create_table = NULL) {
  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  DBI::dbExecute(connection, sql)

  DBI::dbRemoveTable(connection, name = table)

  sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table}'")
  DBI::dbExecute(connection, sql)
  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

S7::method(keep_rows_where, db_postgres) <- function(connection, table, condition, role_create_table = NULL) {
  t0 <- Sys.time()
  temp_name <- paste0("tmp", random_uuid())

  sql <- glue::glue("SELECT * INTO {temp_name} FROM {table} WHERE {condition}")
  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }
  DBI::dbExecute(connection, sql)

  sql <- glue::glue("DROP TABLE {table}")
  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }
  DBI::dbExecute(connection, sql)

  sql <- glue::glue("ALTER TABLE {temp_name} RENAME TO {table}")
  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }
  DBI::dbExecute(connection, sql)

  t1 <- Sys.time()
  dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
}

# drop_table methods
S7::method(drop_table, db_mssql) <- function(connection, table, role_create_table = NULL) {
  return(try(DBI::dbRemoveTable(connection, name = table), TRUE))
}

S7::method(drop_table, db_postgres) <- function(connection, table, role_create_table = NULL) {
  sql <- glue::glue("DROP TABLE {table}")
  if(!is.na(role_create_table)) if(role_create_table!="x"){
    sql <- paste0("SET ROLE ", role_create_table, "; ", sql,"; RESET ROLE")
  }

  return(try(DBI::dbExecute(connection, sql), TRUE))
}

