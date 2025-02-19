#' Blank field_types validator
#' @param db_field_types db_field_types passed to schema
#' @export
validator_field_types_blank <- function(db_field_types) {
  return(TRUE)
}

#' Blank data validator
#' @param data data passed to schema
#' @export
validator_field_contents_blank <- function(data) {
  return(TRUE)
}

#' validator_field_types_csfmt_rts_data_v1
#' An example (schema) validator of field_types used in csfmt_rts_data_v1
#' @param db_field_types db_field_types passed to schema
#' @export
validator_field_types_csfmt_rts_data_v1 <- function(db_field_types) {
  if (!inherits(db_field_types, "character")) {
    return(FALSE)
  }
  if (!length(db_field_types) >= 16) {
    return(FALSE)
  }
  if (!identical(
    db_field_types[1:16],
    c(
      "granularity_time" = "TEXT",
      "granularity_geo" = "TEXT",
      "country_iso3" = "TEXT",
      "location_code" = "TEXT",
      "border" = "INTEGER",
      "age" = "TEXT",
      "sex" = "TEXT",
      "isoyear" = "INTEGER",
      "isoweek" = "INTEGER",
      "isoyearweek" = "TEXT",
      "season" = "TEXT",
      "seasonweek" = "DOUBLE",
      "calyear" = "INTEGER",
      "calmonth" = "INTEGER",
      "calyearmonth" = "TEXT",
      "date" = "DATE"
    )
  )) {
    return(FALSE)
  }

  return(TRUE)
}

#' validator_field_contents_csfmt_rts_data_v1
#' An example (schema) validator of database data used in csfmt_rts_data_v1
#' @param data data passed to schema
#' @export
validator_field_contents_csfmt_rts_data_v1 <- function(data) {
  for (i in unique(data$granularity_time)) {
    if (sum(stringr::str_detect(
      i,
      c(
        "date",
        "isoyear",
        "isoyearweek",
        "^event",
        "total"
      )
    )) == 0) {
      retval <- FALSE
      attr(retval, "var") <- "granularity_time"
      return(retval)
    }
  }

  if (sum(!unique(data$granularity_geo) %in% c(
    "nation",
    "region",
    "hospitaldistrict",
    "county",
    "municip",
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "ward",
    "station",
    "georegion",
    "baregion",
    "missingcounty",
    "missingmunicip",
    "notmainlandcounty",
    "notmainlandmunicip",
    "lab"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "granularity_geo"
    return(retval)
  }

  if (sum(!unique(data$border) %in% c(
    "2020",
    "2024"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "border"
    return(retval)
  }

  if (sum(!unique(data$sex) %in% c(
    "male",
    "female",
    "total"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "sex"
    return(retval)
  }

  if (!inherits(data$date, "Date")) {
    retval <- FALSE
    attr(retval, "var") <- "date"
    return(retval)
  }

  return(TRUE)
}

#' validator_field_types_csfmt_rts_data_v2
#' An example (schema) validator of field_types used in csfmt_rts_data_v2
#' @param db_field_types db_field_types passed to schema
#' @export
validator_field_types_csfmt_rts_data_v2 <- function(db_field_types) {
  if (!inherits(db_field_types, "character")) {
    return(FALSE)
  }
  if (!length(db_field_types) >= 18) {
    return(FALSE)
  }
  if (!identical(
    db_field_types[1:18],
    c(
      "granularity_time" = "TEXT",
      "granularity_geo" = "TEXT",
      "country_iso3" = "TEXT",
      "location_code" = "TEXT",
      "border" = "INTEGER",
      "age" = "TEXT",
      "sex" = "TEXT",
      "isoyear" = "INTEGER",
      "isoweek" = "INTEGER",
      "isoyearweek" = "TEXT",
      "isoquarter" = "INTEGER",
      "isoyearquarter" = "TEXT",
      "season" = "TEXT",
      "seasonweek" = "DOUBLE",
      "calyear" = "INTEGER",
      "calmonth" = "INTEGER",
      "calyearmonth" = "TEXT",
      "date" = "DATE"
    )
  )) {
    return(FALSE)
  }

  return(TRUE)
}

#' validator_field_contents_csfmt_rts_data_v2
#' An example (schema) validator of database data used in csfmt_rts_data_v2
#' @param data data passed to schema
#' @export
validator_field_contents_csfmt_rts_data_v2 <- function(data) {
  for (i in unique(data$granularity_time)) {
    if (sum(stringr::str_detect(
      i,
      c(
        "date",
        "isoyear",
        "isoyearweek",
        "isoyearquarter",
        "season",
        "^event",
        "total"
      )
    )) == 0) {
      retval <- FALSE
      attr(retval, "var") <- "granularity_time"
      return(retval)
    }
  }

  if (sum(!unique(data$granularity_geo) %in% c(
    "nation",
    "georegion",
    "hospitaldistrict",
    "county",
    "municip",
    "wardoslo",
    "extrawardoslo",
    "wardbergen",
    "wardtrondheim",
    "wardstavanger",
    "missingwardoslo",
    "missingwardbergen",
    "missingwardtrondheim",
    "missingwardstavanger",
    "ward",
    "station",
    "baregion",
    "missingcounty",
    "missingmunicip",
    "notmainlandcounty",
    "notmainlandmunicip",
    "lab"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "granularity_geo"
    return(retval)
  }

  if (sum(!unique(data$border) %in% c(
    "2020",
    "2024"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "border"
    return(retval)
  }

  if (sum(!unique(data$sex) %in% c(
    "male",
    "female",
    "total"
  )) > 0) {
    retval <- FALSE
    attr(retval, "var") <- "sex"
    return(retval)
  }

  if (!inherits(data$date, "Date")) {
    retval <- FALSE
    attr(retval, "var") <- "date"
    return(retval)
  }

  return(TRUE)
}

# DBTable_v9 ----
#' R6 Class representing a DB schema/table
#'
#' @description
#' The fundamental way to communicate with database tables.
#'
#' @details
#' This class is a representation of a database table. It is the way that you can
#' access data (e.g. `tbl()`), manipulate data (e.g. `insert_data`, `upsert_data`),
#' and manipulate structural aspects of the database table (e.g. `add_indexes`, `drop_indexes`).
#'
#' @import data.table
#' @import R6
#' @export DBTable_v9
DBTable_v9 <- R6::R6Class(
  "DBTable_v9",

  # public ----
  public = list(
    #' @field dbconnection Database connection.
    dbconnection = NULL,
    #' @field dbconfig Configuration details of the database.
    dbconfig = NULL,
    #' @field table_name Name of the table in the database.
    table_name = NULL,
    #' @field table_name_short_for_mssql_fully_specified_for_postgres Fully specified name of the table in the database (e.g. \[db\].\[dbo\].\[table_name\]).
    table_name_short_for_mssql_fully_specified_for_postgres = NULL,
    #' @field table_name_short_for_mssql_fully_specified_for_postgres_text Fully specified name of the table in the database (e.g. \[db\].\[dbo\].\[table_name\]).
    table_name_short_for_mssql_fully_specified_for_postgres_text = NULL,
    #' @field table_name_fully_specified Fully specified name of the table in the database (e.g. \[db\].\[dbo\].\[table_name\]).
    table_name_fully_specified = NULL,
    #' @field table_name_fully_specified_text Fully specified name of the table in the database (e.g. \[db\].\[dbo\].\[table_name\]) as a text string.
    table_name_fully_specified_text = NULL,
    #' @field field_types The types of each column in the database table (INTEGER, DOUBLE, TEXT, BOOLEAN, DATE, DATETIME).
    field_types = NULL,
    #' @field field_types_with_length The same as \code{field_types} but with \code{(100)} added to the end of all TEXT fields.
    field_types_with_length = NULL,
    #' @field keys The combination of variables that uniquely identify each row in the database.
    keys = NULL,
    #' @field keys_with_length The same as \code{keys} but with \code{(100)} added to the end of all TEXT fields.
    keys_with_length = NULL,
    #' @field indexes A named list of vectors (generally "ind1", "ind2", etc.) that improves the speed of data retrieval operations on a database table.
    indexes = NULL,
    #' @field validator_field_contents A function that validates the data before it is inserted into the database.
    validator_field_contents = NULL,
    #' @field load_folder A temporary folder that is used to write data to before inserting into the database.
    load_folder = tempdir(check = T),
    #' @field censors A named list of censors.
    censors = NULL,

    #' @description
    #' Create a new DBTable_v9 object.
    #'
    #' @param dbconfig Configuration details of the database (driver, server, port, db, schema, user, password, trusted_connection, sslmode).
    #' @param table_name Name of the table in the database.
    #' @param field_types The types of each column in the database table (INTEGER, DOUBLE, TEXT, BOOLEAN, DATE, DATETIME).
    #' @param keys The combination of these variables uniquely identifies each row of data in the table.
    #' @param indexes A named list of vectors (generally "ind1", "ind2", etc.) that improves the speed of data retrieval operations on a database table.
    #' @param validator_field_types A function that validates the \code{field_types} before the DB schema is created.
    #' @param validator_field_contents A function that validates the data before it is inserted into the database.
    #' @return A new `DBTable_v9` object.
    initialize = function(
    dbconfig,
    table_name,
    field_types,
    keys,
    indexes = NULL,
    validator_field_types = validator_field_types_blank,
    validator_field_contents = validator_field_contents_blank
    ) {

      force(dbconfig)
      self$dbconfig <- list()
      self$dbconfig$driver <- dbconfig$driver
      self$dbconfig$server <- dbconfig$server
      self$dbconfig$port <- dbconfig$port
      self$dbconfig$db <- dbconfig$db
      self$dbconfig$schema <- dbconfig$schema
      self$dbconfig$user <- dbconfig$user
      self$dbconfig$password <- dbconfig$password
      self$dbconfig$trusted_connection <- dbconfig$trusted_connection
      self$dbconfig$sslmode <- dbconfig$sslmode

      self$dbconnection <- DBConnection_v9$new(
        driver = self$dbconfig$driver,
        server = self$dbconfig$server,
        port = self$dbconfig$port,
        db = self$dbconfig$db,
        schema = self$dbconfig$schema,
        user = self$dbconfig$user,
        password = self$dbconfig$password,
        trusted_connection = self$dbconfig$trusted_connection,
        sslmode = self$dbconfig$sslmode
      )

      force(table_name)
      self$table_name <- table_name

      if(self$dbconfig$driver %in% c("ODBC Driver 17 for SQL Server")){
        table_fully_specified_vec = c(self$dbconfig$db, self$dbconfig$schema, self$table_name)
      } else {
        table_fully_specified_vec = c(self$dbconfig$schema, self$table_name)
      }
      self$table_name_fully_specified_text <- paste(table_fully_specified_vec, collapse = ".") |>
        stringr::str_remove_all("\\[]\\.")

      if(self$dbconfig$driver %in% c("ODBC Driver 17 for SQL Server")){
        self$table_name_fully_specified <- self$table_name_fully_specified_text
        self$table_name_short_for_mssql_fully_specified_for_postgres <- self$table_name
        self$table_name_short_for_mssql_fully_specified_for_postgres_text <- self$table_name
      } else {
        self$table_name_fully_specified <- DBI::Id(
          #database = self$dbconfig$db, this could be catalog??
          schema = self$dbconfig$schema,
          table = self$table_name
        )
        self$table_name_short_for_mssql_fully_specified_for_postgres <- self$table_name_fully_specified
        self$table_name_short_for_mssql_fully_specified_for_postgres_text <- self$table_name_fully_specified_text
      }

      force(field_types)
      self$field_types <- field_types
      self$field_types_with_length <- field_types

      force(keys)
      self$keys <- keys
      self$keys_with_length <- keys

      force(indexes)
      self$indexes <- indexes

      # validators
      if (!is.null(validator_field_types)) if (!validator_field_types(self$field_types)) stop(glue::glue("field_types not validated in {table_name}"))
      self$validator_field_contents <- validator_field_contents

      # db_field_types_with_lengths
      ind <- self$field_types == "TEXT"
      ind_text_with_specific_length <- stringr::str_detect(self$field_types, "TEXT")
      ind_text_with_specific_length[ind] <- FALSE
      if (sum(ind) > 0) {
        self$field_types_with_length[ind] <- paste0(self$field_types_with_length[ind], " (100)")
      }
      if (sum(ind_text_with_specific_length) > 0) {
        lengths <- stringr::str_extract(self$field_types[ind_text_with_specific_length], "\\([0-9]*\\)")
        self$field_types_with_length[ind_text_with_specific_length] <- paste0(self$field_types_with_length[ind_text_with_specific_length], " ", lengths)
      }

      # remove numbers from field_types
      naming <- names(self$field_types)
      self$field_types <- stringr::str_remove(self$field_types, " \\([0-9]*\\)")
      names(self$field_types) <- naming
      # fixing indexes
      self$keys_with_length <- self$field_types_with_length[self$keys]
    },

    #' @description
    #' Class-specific print function.
    #' @param ... Not in use.
    print = function(...) {
      if (!self$dbconnection$is_connected()) {
        cat(self$table_name_fully_specified_text, crayon::bgRed(crayon::white("(disconnected)\n\n")))
      } else {
        cat(self$table_name_fully_specified_text, crayon::bgCyan(crayon::white("(connected)\n\n")))
      }
      width_of_numbering <- nchar(length(self$field_types))
      for (i in seq_along(self$field_types)) {
        number <- formatC(i, width = width_of_numbering)
        x_name <- names(self$field_types)[i]
        x_type <- self$field_types[i]
        if(x_name %in% self$keys){
          x_key <- crayon::bgRed(crayon::white("(KEY)"))
        } else {
          x_key <- ""
        }
        cat(" ", number, ": ", x_name, " (", x_type, ") ", x_key, "\n", sep = "")
      }
      cat("\n")

      invisible(self)
    },

    #' @description
    #' Connect from the database
    connect = function() {
      self$dbconnection$connect()
      private$lazy_creation_of_table()
    },

    #' @description
    #' Disconnect from the database
    disconnect = function() {
      self$dbconnection$disconnect()
    },

    #' @description
    #' Does the table exist
    table_exists = function() {
      return(DBI::dbExistsTable(self$dbconnection$autoconnection, self$table_name_short_for_mssql_fully_specified_for_postgres))
    },

    #' @description
    #' Create the database table
    create_table = function() {
      # self$connect calls self$create_table.
      # cannot have infinite loop
      create_tab <- TRUE
      if (self$table_exists()) {
        if (!private$check_fields_match()) {
          message(glue::glue("Dropping table {self$table_name} because fields dont match"))
          self$remove_table()
        } else {
          create_tab <- FALSE
        }
      }
      if (create_tab) {
        message(glue::glue("Creating table {self$table_name}"))
        create_table(self$dbconnection$autoconnection, self$table_name_fully_specified, self$field_types, self$keys)
        private$add_constraint()
        self$add_indexes()
      }
    },

    #' @description
    #' Drop the database table
    remove_table = function() {
      if (self$table_exists()) {
        message(glue::glue("Dropping table {self$table_name}"))
        DBI::dbRemoveTable(self$dbconnection$autoconnection, self$table_name_short_for_mssql_fully_specified_for_postgres)
      }
    },

    #' @description
    #' Inserts data
    #' @param newdata The data to insert.
    #' @param confirm_insert_via_nrow Checks nrow() before insert and after insert. If nrow() has not increased sufficiently, then attempt an upsert.
    #' @param verbose Boolean.
    #' Inserts data into the database table
    insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE) {
      private$lazy_creation_of_table()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      #newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"))

      # this will make the insert go faster, because
      # the data will be sorted
      # setkeyv(newdata, self$keys)
      infile <- random_file(private$load_folder_fn(), extra_insert = digest::digest(newdata[1,]))
      load_data_infile(
        connection = self$dbconnection$autoconnection,
        dbconfig = self$dbconnection$config,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        dt = newdata,
        file = infile
      )

      if(confirm_insert_via_nrow){
        nrow_after <- self$nrow(use_count = TRUE)
        if(nrow_after < nrow(newdata)){
          message("After insert have ", nrow_after, " rows. Tried to insert ", nrow(newdata), ". Now trying upsert.")

          self$upsert_data(
            newdata = newdata,
            drop_indexes = NULL,
            verbose = verbose
          )
          nrow_after <- self$nrow(use_count = TRUE)
          if(nrow_after < nrow(newdata)){
            message("After upsert have ", nrow_after, " rows. Tried to upsert ", nrow(newdata), ".")
            stop("Upsert failed")
          }
        }
      }
    },

    #' @description
    #' Upserts data into the database table
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector containing the indexes to be dropped before upserting (can increase performance).
    #' @param verbose Boolean.
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      private$lazy_creation_of_table()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      # newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) stop(glue::glue("upsert_load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"))

      # this will make the insert go faster, because
      # the data will be sorted

      infile <- random_file(private$load_folder_fn(), extra_insert = digest::digest(newdata[1,]))
      upsert_load_data_infile(
        connection = self$dbconnection$autoconnection,
        dbconfig = self$dbconnection$config,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        dt = newdata[, names(self$field_types), with = F],
        file = infile,
        fields = names(self$field_types),
        keys = self$keys,
        drop_indexes = drop_indexes
      )
    },

    #' @description
    #' Drops all rows in the database table
    drop_all_rows = function() {
      private$lazy_creation_of_table()
      drop_all_rows(connection = self$dbconnection$autoconnection, self$table_name_fully_specified_text)

    },

    #' @description
    #' Drops rows in the database table according to the SQL condition.
    #' @param condition SQL text condition.
    drop_rows_where = function(condition) {
      private$lazy_creation_of_table()

      drop_rows_where(
        connection = self$dbconnection$autoconnection,
        self$table_name_short_for_mssql_fully_specified_for_postgres_text,
        condition
      )
    },

    #' @description
    #' Keeps rows in the database table according to the SQL condition.
    #' @param condition SQL text condition.
    keep_rows_where = function(condition) {
      private$lazy_creation_of_table()
      keep_rows_where(connection = self$dbconnection$autoconnection, self$table_name_short_for_mssql_fully_specified_for_postgres_text, condition)
      private$add_constraint()
    },

    #' @description
    #' Drops all rows in the database table and then upserts data.
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector containing the indexes to be dropped before upserting (can increase performance).
    #' @param verbose Boolean.
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      private$lazy_creation_of_table()
      self$drop_all_rows()
      self$upsert_data(
        newdata = newdata,
        drop_indexes = drop_indexes,
        verbose = verbose
      )
    },

    #' @description
    #' Drops all rows in the database table and then inserts data.
    #' @param newdata The data to insert.
    #' @param confirm_insert_via_nrow Checks nrow() before insert and after insert. If nrow() has not increased sufficiently, then attempt an upsert.
    #' @param verbose Boolean.
    drop_all_rows_and_then_insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE) {
      private$lazy_creation_of_table()
      self$drop_all_rows()
      self$insert_data(
        newdata = newdata,
        confirm_insert_via_nrow = confirm_insert_via_nrow,
        verbose = verbose
      )
    },

    #' @description
    #' Provides access to the database table via dplyr::tbl.
    tbl = function() {
      private$lazy_creation_of_table()
      retval <- self$dbconnection$autoconnection %>%
        dplyr::tbl(self$table_name_short_for_mssql_fully_specified_for_postgres)

      return(retval)
    },

    #' @description
    #' Prints a template dplyr::select call that you can easily copy/paste for all your variables.
    print_dplyr_select = function() {
      private$lazy_creation_of_table()
      x <- self$tbl() %>%
        head() %>%
        dplyr::collect() %>%
        names() %>%
        paste0(., collapse = ",\n  ")
      x <- paste0("dplyr::select(\n  ", x, "\n) %>%")
      cat(x)
    },

    #' @description
    #' Adds indexes to the database table from `self$indexes`
    add_indexes = function() {
      private$lazy_creation_of_table()
      for (i in names(self$indexes)) {
        message(glue::glue("Adding index {i}"))

        add_index(
          connection = self$dbconnection$autoconnection,
          table = self$table_name_short_for_mssql_fully_specified_for_postgres_text,
          index = i,
          keys = self$indexes[[i]]
        )
      }
    },

    #' @description
    #' Drops all indees from the database table
    drop_indexes = function() {
      private$lazy_creation_of_table()
      for (i in names(self$indexes)) {
        message(glue::glue("Dropping index {i}"))
        drop_index(
          connection = self$dbconnection$autoconnection,
          table = self$table_name_short_for_mssql_fully_specified_for_postgres_text,
          index = i
        )
      }
    },

    #' @description
    #' Confirms that the names and number of indexes in the database are the same as in the R code.
    #' Does not confirm the contents of the indexes!
    confirm_indexes = function() {
      indexes_db <- get_indexes(
        connection = self$dbconnection$autoconnection,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres_text
      )
      indexes_self <- names(self$indexes)
      if(!identical(indexes_db, indexes_self)){
        self$drop_indexes()
        self$add_indexes()
      }
    },

    #' @description
    #' Gets the number of rows in the database table
    #' @param use_count If true, then uses the count command, which is slow but accurate. If false, then uses summary statistics, which is fast but inaccurate.
    nrow = function(use_count = FALSE){
      if(use_count){
        retval <- self$tbl() |>
          dplyr::summarize(n=n()) |>
          dplyr::collect()
        retval <- retval$n
      } else {
        retval <- get_table_names_and_info(self$dbconnection$autoconnection)
        retval <- retval[table_name %in% self$table_name]$nrow
      }
      return(retval)
    },

    #' @description
    #' Gets the information about the database table
    info = function(){
      retval <- get_table_names_and_info(self$dbconnection$autoconnection)
      retval <- retval[table_name %in% self$table_name]
      data.table::shouldPrint(retval)
      return(retval)
    }
  ),

  # private ----
  private = list(
    # Lazyload the creation of the table
    lazy_created_table = FALSE,
    lazy_creation_of_table = function(){
      if(!private$lazy_created_table){
        self$create_table()
        private$lazy_created_table <- TRUE
      }
    },

    check_fields_match = function() {
      fields <- DBI::dbListFields(self$dbconnection$autoconnection, self$table_name_short_for_mssql_fully_specified_for_postgres)
      retval <- identical(fields, names(self$field_types))
      if (retval == FALSE) {
        message(glue::glue(
          "given fields: {paste0(names(self$field_types),collapse=', ')}\n",
          "db fields: {paste0(fields,collapse=', ')}"
        ))
      }
      return(retval)
    },

    load_folder_fn = function() tempdir(check = T),

    add_constraint = function() {
      add_constraint(
        connection = self$dbconnection$autoconnection,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres_text,
        keys = self$keys
      )
    },

    drop_constraint = function() {
      drop_constraint(
        connection = self$dbconnection$autoconnection,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres_text
      )
    },

    make_censored_data = function(newdata) {
      d <- copy(newdata)
      for (i in seq_along(self$censors)) {
        self$censors[[i]](d)
      }
      return(d)
    },
    finalize = function() {
      # self$db_disconnect()
    }
  )
)
