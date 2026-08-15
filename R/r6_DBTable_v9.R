#' Blank field types validator
#'
#' A pass-through validator that accepts any field types without validation.
#' Use it as a placeholder when you need no check on the field types.
#'
#' @param db_field_types A named character vector of database field types.
#' @return Always returns TRUE.
#' @export
#' @family field type validators
#' @seealso The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, which passes this to
#'   \code{DBTable_v9$new()} as its \code{validator_field_types} argument.
#'   \code{\link{DBTable_v9}} calls the field type validator once, while the
#'   object is being created.
#' @examples
#' # This validator always returns TRUE regardless of input
#' field_types <- c("id" = "INTEGER", "name" = "TEXT", "date" = "DATE")
#' validator_field_types_blank(field_types)
#'
#' # Works with any field types
#' other_types <- c("value" = "DOUBLE", "status" = "BOOLEAN")
#' validator_field_types_blank(other_types)
validator_field_types_blank <- function(db_field_types) {
  return(TRUE)
}

#' Blank data contents validator
#'
#' A pass-through validator that accepts any data without validation.
#' Use it as a placeholder when you need no check on the data contents.
#'
#' @param data A data.frame or data.table containing the data to validate.
#' @return Always returns TRUE.
#' @export
#' @family field contents validators
#' @seealso The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, which passes this to
#'   \code{DBTable_v9$new()} as its \code{validator_field_contents} argument.
#'   \code{\link{DBTable_v9}} calls the field contents validator from its
#'   \code{insert_data()} and \code{upsert_data()} methods.
#' @examples
#' # This validator always returns TRUE regardless of input
#' test_data <- data.frame(id = 1:3, name = c("A", "B", "C"), value = c(10, 20, 30))
#' validator_field_contents_blank(test_data)
#'
#' # Works with any data structure
#' empty_data <- data.frame()
#' validator_field_contents_blank(empty_data)
validator_field_contents_blank <- function(data) {
  return(TRUE)
}

#' Field types validator for csfmt_rts_data_v1 schema
#'
#' Checks that field types conform to the csfmt_rts_data_v1 schema
#' specification. The validator checks the first 16 entries of
#' \code{db_field_types} against the expected structure of that schema.
#'
#' @param db_field_types A named character vector of database field types.
#' @return TRUE if field types are valid for csfmt_rts_data_v1, FALSE
#'   otherwise.
#' @export
#' @family field type validators
#' @seealso \code{\link{DBTable_v9}}, which takes this as its
#'   \code{validator_field_types} argument and calls it once, while the object
#'   is being created.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not use this validator.
#' @examples
#' # Valid field types for csfmt_rts_data_v1
#' valid_fields <- c(
#'   "granularity_time" = "TEXT",
#'   "granularity_geo" = "TEXT",
#'   "country_iso3" = "TEXT",
#'   "location_code" = "TEXT",
#'   "border" = "INTEGER",
#'   "age" = "TEXT",
#'   "sex" = "TEXT",
#'   "isoyear" = "INTEGER",
#'   "isoweek" = "INTEGER",
#'   "isoyearweek" = "TEXT",
#'   "season" = "TEXT",
#'   "seasonweek" = "DOUBLE",
#'   "calyear" = "INTEGER",
#'   "calmonth" = "INTEGER",
#'   "calyearmonth" = "TEXT",
#'   "date" = "DATE",
#'   "cases_n" = "INTEGER"
#' )
#' validator_field_types_csfmt_rts_data_v1(valid_fields)
#'
#' # Invalid field types (wrong structure)
#' invalid_fields <- c("id" = "INTEGER", "name" = "TEXT")
#' validator_field_types_csfmt_rts_data_v1(invalid_fields)
validator_field_types_csfmt_rts_data_v1 <- function(db_field_types) {
  if (!inherits(db_field_types, "character")) {
    return(FALSE)
  }
  if (!length(db_field_types) >= 16) {
    return(FALSE)
  }
  if (
    !identical(
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
    )
  ) {
    return(FALSE)
  }

  return(TRUE)
}

#' Field contents validator for csfmt_rts_data_v1 schema
#'
#' Checks that data contents conform to the csfmt_rts_data_v1 schema
#' specification. The validator checks the granularity_time, granularity_geo,
#' border, sex, and date fields against the values that schema allows.
#'
#' @param data A data.frame or data.table containing the data to validate.
#' @return TRUE if data is valid for csfmt_rts_data_v1, FALSE otherwise (with
#'   error attribute).
#' @export
#' @family field contents validators
#' @seealso \code{\link{DBTable_v9}}, which takes this as its
#'   \code{validator_field_contents} argument and calls it from its
#'   \code{insert_data()} and \code{upsert_data()} methods.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not use this validator.
#' @examples
#' # Valid data for csfmt_rts_data_v1 (all required columns present)
#' valid_data <- data.frame(
#'   granularity_time = c("date", "isoyearweek", "total"),
#'   granularity_geo = c("nation", "county", "municip"),
#'   border = c("2020", "2020", "2020"),
#'   sex = c("total", "total", "total"),
#'   date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01")),
#'   stringsAsFactors = FALSE
#' )
#' validator_field_contents_csfmt_rts_data_v1(valid_data)
#'
#' # Invalid data (unrecognised granularity_geo value)
#' invalid_data <- data.frame(
#'   granularity_time = "date",
#'   granularity_geo = "invalid_geo",
#'   border = "2020",
#'   sex = "total",
#'   date = as.Date("2020-01-01"),
#'   stringsAsFactors = FALSE
#' )
#' validator_field_contents_csfmt_rts_data_v1(invalid_data)
validator_field_contents_csfmt_rts_data_v1 <- function(data) {
  for (i in unique(data$granularity_time)) {
    if (
      sum(stringr::str_detect(
        i,
        c(
          "date",
          "isoyear",
          "isoyearweek",
          "^event",
          "total"
        )
      )) ==
        0
    ) {
      retval <- FALSE
      attr(retval, "var") <- "granularity_time"
      return(retval)
    }
  }

  if (
    sum(
      !unique(data$granularity_geo) %in%
        c(
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
        )
    ) >
      0
  ) {
    retval <- FALSE
    attr(retval, "var") <- "granularity_geo"
    return(retval)
  }

  if (
    sum(
      !unique(data$border) %in%
        c(
          "2020",
          "2024"
        )
    ) >
      0
  ) {
    retval <- FALSE
    attr(retval, "var") <- "border"
    return(retval)
  }

  if (
    sum(
      !unique(data$sex) %in%
        c(
          "male",
          "female",
          "missing",
          "total"
        )
    ) >
      0
  ) {
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

#' Field types validator for csfmt_rts_data_v2 schema
#'
#' Checks that field types conform to the csfmt_rts_data_v2 schema
#' specification. The validator checks the first 18 entries of
#' \code{db_field_types} against the expected structure of that schema.
#'
#' @param db_field_types A named character vector of database field types.
#' @return TRUE if field types are valid for csfmt_rts_data_v2, FALSE
#'   otherwise.
#' @export
#' @family field type validators
#' @seealso \code{\link{DBTable_v9}}, which takes this as its
#'   \code{validator_field_types} argument and calls it once, while the object
#'   is being created.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not use this validator.
#' @examples
#' # Valid field types for csfmt_rts_data_v2. The first 18 must match the
#' # schema, which unlike v1 carries isoquarter and isoyearquarter.
#' valid_fields_v2 <- c(
#'   "granularity_time" = "TEXT",
#'   "granularity_geo" = "TEXT",
#'   "country_iso3" = "TEXT",
#'   "location_code" = "TEXT",
#'   "border" = "INTEGER",
#'   "age" = "TEXT",
#'   "sex" = "TEXT",
#'   "isoyear" = "INTEGER",
#'   "isoweek" = "INTEGER",
#'   "isoyearweek" = "TEXT",
#'   "isoquarter" = "INTEGER",
#'   "isoyearquarter" = "TEXT",
#'   "season" = "TEXT",
#'   "seasonweek" = "DOUBLE",
#'   "calyear" = "INTEGER",
#'   "calmonth" = "INTEGER",
#'   "calyearmonth" = "TEXT",
#'   "date" = "DATE",
#'   "tag_outcome" = "TEXT",
#'   "tag_type" = "TEXT",
#'   "cases_n" = "INTEGER"
#' )
#' validator_field_types_csfmt_rts_data_v2(valid_fields_v2)
#'
#' # The v1 layout is not valid for v2: it has no isoquarter
#' validator_field_types_csfmt_rts_data_v2(valid_fields_v2[-c(11, 12)])
validator_field_types_csfmt_rts_data_v2 <- function(db_field_types) {
  if (!inherits(db_field_types, "character")) {
    return(FALSE)
  }
  if (!length(db_field_types) >= 18) {
    return(FALSE)
  }
  if (
    !identical(
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
    )
  ) {
    return(FALSE)
  }

  return(TRUE)
}

#' Field contents validator for csfmt_rts_data_v2 schema
#'
#' Checks that data contents conform to the csfmt_rts_data_v2 schema
#' specification. The validator checks the granularity_time, granularity_geo,
#' border, sex, and date fields against the values that schema allows.
#'
#' @param data A data.frame or data.table containing the data to validate.
#' @return TRUE if data is valid for csfmt_rts_data_v2, FALSE otherwise (with
#'   error attribute).
#' @export
#' @family field contents validators
#' @seealso \code{\link{DBTable_v9}}, which takes this as its
#'   \code{validator_field_contents} argument and calls it from its
#'   \code{insert_data()} and \code{upsert_data()} methods.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not use this validator.
#' @examples
#' # Valid data for csfmt_rts_data_v2 (all required columns present)
#' valid_data_v2 <- data.frame(
#'   granularity_time = c("date", "isoyearweek", "total"),
#'   granularity_geo = c("nation", "county", "municip"),
#'   border = c("2020", "2020", "2020"),
#'   sex = c("total", "total", "total"),
#'   date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01")),
#'   stringsAsFactors = FALSE
#' )
#' validator_field_contents_csfmt_rts_data_v2(valid_data_v2)
#'
#' # Invalid data (unrecognised granularity_geo value)
#' invalid_data_v2 <- data.frame(
#'   granularity_time = "date",
#'   granularity_geo = "invalid_geo",
#'   border = "2020",
#'   sex = "total",
#'   date = as.Date("2020-01-01"),
#'   stringsAsFactors = FALSE
#' )
#' validator_field_contents_csfmt_rts_data_v2(invalid_data_v2)
validator_field_contents_csfmt_rts_data_v2 <- function(data) {
  for (i in unique(data$granularity_time)) {
    if (
      sum(stringr::str_detect(
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
      )) ==
        0
    ) {
      retval <- FALSE
      attr(retval, "var") <- "granularity_time"
      return(retval)
    }
  }

  if (
    sum(
      !unique(data$granularity_geo) %in%
        c(
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
        )
    ) >
      0
  ) {
    retval <- FALSE
    attr(retval, "var") <- "granularity_geo"
    return(retval)
  }

  if (
    sum(
      !unique(data$border) %in%
        c(
          "2020",
          "2024"
        )
    ) >
      0
  ) {
    retval <- FALSE
    attr(retval, "var") <- "border"
    return(retval)
  }

  if (
    sum(
      !unique(data$sex) %in%
        c(
          "male",
          "female",
          "total"
        )
    ) >
      0
  ) {
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
#' R6 Class representing a database table
#'
#' @description
#' A database table management class that provides operations for data
#' manipulation, schema validation, and table administration. This class
#' combines database connectivity with data validation and bulk operations.
#'
#' @details
#' The DBTable_v9 class is a database table abstraction that provides:
#'
#' \strong{Core functionality:}
#' \itemize{
#'   \item Table creation and schema management.
#'   \item Data insertion with bulk loading capabilities.
#'   \item Upsert operations (insert or update).
#'   \item Index management (creation, deletion).
#'   \item Data validation through customizable validators.
#'   \item Integration with dplyr for data queries.
#' }
#'
#' \strong{Advanced features:}
#' \itemize{
#'   \item Automatic table creation based on field specifications.
#'   \item Schema validation with custom validator functions.
#'   \item Efficient bulk data loading using database-specific methods.
#'   \item Index optimization for query performance.
#'   \item Cross-database compatibility (SQL Server, PostgreSQL).
#' }
#'
#' \strong{Data validation:}
#' The class supports custom validation functions for both field types and data
#' contents, which ensure data integrity and schema compliance.
#'
#' @section What the object creates in the database:
#' One object creates three kinds of thing, and each carries its own name rule.
#'
#' \describe{
#'   \item{The table}{Named \code{table_name}, in the schema that
#'     \code{dbconfig} names.}
#'   \item{The primary key constraint}{Named \code{PK_} plus the fully
#'     specified table name, with every \code{.}, \code{[} and \code{]}
#'     deleted. Schema \code{anon} with table \code{anon_data} therefore gives
#'     \code{PK_anonanon_data}. Two different tables can reach one name,
#'     because the rule deletes the separator. Schema \code{a} with table
#'     \code{bc} and schema \code{ab} with table \code{c} both give
#'     \code{PK_abc}.}
#'   \item{One index per entry in \code{indexes}}{The names you write in
#'     \code{indexes} are logical names. Each index reaches the database under
#'     a physical name of the form \code{ix_<slug>_<16 hexadecimal
#'     characters>}, at most 63 characters. The name carries the table
#'     identity, so two tables in one schema that both declare \code{ind1} get
#'     two indexes. \code{csdb:::index_physical_name()} returns the name for
#'     one table and one logical name.}
#' }
#'
#' @section The case of a constraint name on PostgreSQL:
#' The source writes \code{PK_}, in upper case. PostgreSQL folds an unquoted
#' identifier to lower case, so the catalogue stores \code{pk_}. Measured on
#' the \code{norsyss_data1} database on 2026-08-15: 92 lower case \code{pk_}
#' constraint names, and 0 upper case.
#'
#' A \code{DROP CONSTRAINT} that quotes the source spelling therefore fails on
#' PostgreSQL. Write the name unquoted, or write it in lower case.
#'
#' SQLite does not fold at all. It keeps \code{PK_MixedCase} exactly as the
#' source writes it, so the two backends disagree on one identifier.
#'
#' The physical index name has no such trap. It is lower case already, so it
#' reads the same in the source and in both catalogues.
#'
#' @import data.table
#' @import R6
#' @export DBTable_v9
#' @family database classes
#' @seealso The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}. It builds one of these on
#'   SQLite and inserts the bundled
#'   \code{nor_covid19_cases_by_time_location} dataset. It also shows two
#'   tables that declare one logical index name.
#'   \code{\link{DBConnection_v9}} takes the same arguments as the
#'   \code{dbconfig} list, and one is created here to hold the connection.
#' @examples
#' # Creating the object opens no connection, and the field types are
#' # checked while it is created. These field types do not satisfy the
#' # csfmt_rts_data_v1 schema, so the constructor stops.
#' try(DBTable_v9$new(
#'   dbconfig = list(driver = "PostgreSQL Unicode", server = "localhost"),
#'   table_name = "my_data_table",
#'   field_types = c("id" = "INTEGER"),
#'   keys = "id",
#'   validator_field_types = validator_field_types_csfmt_rts_data_v1
#' ))
#'
#' \donttest{
#' # A full cycle on SQLite, in a file that tempfile() names. SQLite needs
#' # no server, so this block runs anywhere. Name a driver of
#' # "ODBC Driver 17 for SQL Server" or "PostgreSQL Unicode" instead, and
#' # nothing else in the block changes.
#' db_config <- list(driver = "SQLite", db = tempfile(fileext = ".sqlite"))
#'
#' # Indexes are named here, because add_indexes() takes no arguments and
#' # reads them from the object.
#' my_table <- DBTable_v9$new(
#'   dbconfig = db_config,
#'   table_name = "my_data_table",
#'   field_types = c(
#'     "id" = "INTEGER",
#'     "name" = "TEXT",
#'     "value" = "DOUBLE",
#'     "date_created" = "DATE"
#'   ),
#'   keys = "id",
#'   indexes = list("ind1" = c("name", "date_created")),
#'   validator_field_types = validator_field_types_blank,
#'   validator_field_contents = validator_field_contents_blank
#' )
#'
#' my_table$create_table()
#'
#' # insert_data() and upsert_data() need a data.table.
#' my_table$insert_data(data.table::data.table(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie"),
#'   value = c(10.5, 20.3, 15.7),
#'   date_created = as.Date("2023-01-01")
#' ))
#'
#' # tbl() returns a lazy dbplyr reference.
#' my_table$tbl() |>
#'   dplyr::filter(value > 15) |>
#'   dplyr::collect()
#'
#' # Add the indexes that were named above.
#' my_table$add_indexes()
#'
#' my_table$upsert_data(data.table::data.table(
#'   id = 2:4,
#'   name = c("Bob_Updated", "Charlie", "David"),
#'   value = c(25.0, 15.7, 30.2),
#'   date_created = as.Date("2023-01-02")
#' ))
#' my_table$nrow()
#'
#' my_table$disconnect()
#' }
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
    #' @param dbconfig Configuration details of the database (driver, server, port, db, schema, user, password, trusted_connection, sslmode, role_create_table).
    #' @param table_name Name of the table in the database.
    #' @param field_types The types of each column in the database table (INTEGER, DOUBLE, TEXT, BOOLEAN, DATE, DATETIME).
    #' @param keys The combination of these variables uniquely identifies each row of data in the table.
    #' @param indexes A named list of vectors (generally "ind1", "ind2", etc.) that improves the speed of data retrieval operations on a database table.
    #' @param validator_field_types A function that validates the \code{field_types} before the DB schema is created.
    #' @param validator_field_contents A function that validates the data before it is inserted into the database.
    #' @param dbconnection An existing \code{DBConnection_v9} to use, or NULL.
    #'   The object borrows a supplied connection and does not own it.
    #'   \code{disconnect()} then does nothing, so the caller decides when the
    #'   connection closes. The object creates and owns a connection when this
    #'   argument is NULL. It is the last argument, because a subclass can
    #'   forward the earlier seven positionally.
    #' @return A new `DBTable_v9` object.
    initialize = function(
      dbconfig,
      table_name,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = validator_field_types_blank,
      validator_field_contents = validator_field_contents_blank,
      dbconnection = NULL
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
      self$dbconfig$role_create_table <- dbconfig$role_create_table

      force(dbconnection)
      if (is.null(dbconnection)) {
        self$dbconnection <- DBConnection_v9$new(
          driver = self$dbconfig$driver,
          server = self$dbconfig$server,
          port = self$dbconfig$port,
          db = self$dbconfig$db,
          schema = self$dbconfig$schema,
          user = self$dbconfig$user,
          password = self$dbconfig$password,
          trusted_connection = self$dbconfig$trusted_connection,
          sslmode = self$dbconfig$sslmode,
          role_create_table = self$dbconfig$role_create_table
        )
        private$owns_dbconnection <- TRUE
      } else {
        # A supplied connection is borrowed. The caller keeps ownership, and
        # disconnect() below leaves it open.
        self$dbconnection <- dbconnection
        private$owns_dbconnection <- FALSE
      }

      force(table_name)
      self$table_name <- table_name

      if (identical(toupper(self$dbconfig$driver), "SQLITE")) {
        # SQLite has no schemas, so the identifier is the bare table name.
        # This arm does not share the paste() below on purpose: `schema`
        # arrives as "" rather than NULL, because cs9 builds every dbconfig
        # from Sys.getenv(). paste(c("", "tab"), collapse = ".") is ".tab",
        # and str_remove_all("\\[]\\.") does not strip a leading dot.
        self$table_name_fully_specified_text <- self$table_name
        self$table_name_fully_specified <- DBI::Id(table = self$table_name)
        self$table_name_short_for_mssql_fully_specified_for_postgres <- DBI::Id(
          table = self$table_name
        )
        self$table_name_short_for_mssql_fully_specified_for_postgres_text <- self$table_name
      } else if (self$dbconfig$driver %in% c("ODBC Driver 17 for SQL Server")) {
        table_fully_specified_vec <- c(
          self$dbconfig$db,
          self$dbconfig$schema,
          self$table_name
        )
        self$table_name_fully_specified_text <- paste(
          table_fully_specified_vec,
          collapse = "."
        ) |>
          stringr::str_remove_all("\\[]\\.")

        self$table_name_fully_specified <- self$table_name_fully_specified_text
        self$table_name_short_for_mssql_fully_specified_for_postgres <- self$table_name
        self$table_name_short_for_mssql_fully_specified_for_postgres_text <- self$table_name
      } else {
        table_fully_specified_vec <- c(self$dbconfig$schema, self$table_name)
        self$table_name_fully_specified_text <- paste(
          table_fully_specified_vec,
          collapse = "."
        ) |>
          stringr::str_remove_all("\\[]\\.")

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
      if (!is.null(validator_field_types)) {
        if (!validator_field_types(self$field_types)) {
          stop(glue::glue("field_types not validated in {table_name}"))
        }
      }
      self$validator_field_contents <- validator_field_contents

      # db_field_types_with_lengths
      ind <- self$field_types == "TEXT"
      ind_text_with_specific_length <- stringr::str_detect(
        self$field_types,
        "TEXT"
      )
      ind_text_with_specific_length[ind] <- FALSE
      if (sum(ind) > 0) {
        self$field_types_with_length[ind] <- paste0(
          self$field_types_with_length[ind],
          " (100)"
        )
      }
      if (sum(ind_text_with_specific_length) > 0) {
        lengths <- stringr::str_extract(
          self$field_types[ind_text_with_specific_length],
          "\\([0-9]*\\)"
        )
        self$field_types_with_length[ind_text_with_specific_length] <- paste0(
          self$field_types_with_length[ind_text_with_specific_length],
          " ",
          lengths
        )
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
    #' @param ... Not used.
    print = function(...) {
      if (!self$dbconnection$is_connected()) {
        if (requireNamespace("crayon", quietly = TRUE)) {
          cat(
            self$table_name_fully_specified_text,
            crayon::bgRed(crayon::white("(disconnected)\n\n"))
          )
        } else {
          cat(self$table_name_fully_specified_text, "(disconnected)\n\n")
        }
      } else {
        if (requireNamespace("crayon", quietly = TRUE)) {
          cat(
            self$table_name_fully_specified_text,
            crayon::bgCyan(crayon::white("(connected)\n\n"))
          )
        } else {
          cat(self$table_name_fully_specified_text, "(connected)\n\n")
        }
      }
      width_of_numbering <- nchar(length(self$field_types))
      for (i in seq_along(self$field_types)) {
        number <- formatC(i, width = width_of_numbering)
        x_name <- names(self$field_types)[i]
        x_type <- self$field_types[i]
        if (x_name %in% self$keys) {
          x_key <- if (requireNamespace("crayon", quietly = TRUE)) {
            crayon::bgRed(crayon::white("(KEY)"))
          } else {
            "(KEY)"
          }
        } else {
          x_key <- ""
        }
        cat(
          " ",
          number,
          ": ",
          x_name,
          " (",
          x_type,
          ") ",
          x_key,
          "\n",
          sep = ""
        )
      }
      cat("\n")

      invisible(self)
    },

    #' @description
    #' Connect to the database.
    connect = function() {
      self$dbconnection$connect()
      private$lazy_creation_of_table()
    },

    #' @description
    #' Disconnect from the database. This does nothing when the connection came
    #' from the \code{dbconnection} argument, because the caller owns that
    #' connection.
    disconnect = function() {
      if (private$owns_dbconnection) {
        self$dbconnection$disconnect()
      }
    },

    #' @description
    #' Does the table exist?
    table_exists = function() {
      return(DBI::dbExistsTable(
        self$dbconnection$autoconnection,
        self$table_name_short_for_mssql_fully_specified_for_postgres
      ))
    },

    #' @description
    #' Create the database table.
    create_table = function() {
      # self$connect calls self$create_table.
      # cannot have infinite loop
      create_tab <- TRUE
      if (self$table_exists()) {
        if (!private$check_fields_match()) {
          message(glue::glue(
            "Dropping table {self$table_name} because fields dont match"
          ))
          self$remove_table()
        } else {
          create_tab <- FALSE
        }
      }
      if (create_tab) {
        message(glue::glue("Creating table {self$table_name}"))
        create_table(
          connection = self$dbconnection$autoconnection,
          table = self$table_name_fully_specified,
          fields = self$field_types,
          keys = self$keys,
          role_create_table = self$dbconnection$config$role_create_table
        )
        private$add_constraint()
        self$add_indexes()
      }
    },

    #' @description
    #' Drop the database table.
    remove_table = function() {
      if (self$table_exists()) {
        message(glue::glue("Dropping table {self$table_name}"))
        DBI::dbRemoveTable(
          self$dbconnection$autoconnection,
          self$table_name_short_for_mssql_fully_specified_for_postgres
        )
      }
    },

    #' @description
    #' Inserts data into the database table.
    #' @param newdata The data to insert.
    #' @param confirm_insert_via_nrow Checks nrow() before the insert and after the insert. If nrow() did not increase enough, the method attempts an upsert.
    #' @param verbose Boolean.
    insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    ) {
      private$lazy_creation_of_table()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      #newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) {
        stop(glue::glue(
          "load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"
        ))
      }

      # this will make the insert go faster, because
      # the data will be sorted
      # setkeyv(newdata, self$keys)
      infile <- random_file(
        private$load_folder_fn(),
        extra_insert = if (requireNamespace("digest", quietly = TRUE)) {
          digest::digest(newdata[1, ])
        } else {
          ""
        }
      )
      load_data_infile(
        connection = self$dbconnection$autoconnection,
        dbconfig = self$dbconnection$config,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        dt = newdata,
        file = infile
      )

      if (confirm_insert_via_nrow) {
        nrow_after <- self$nrow(use_count = TRUE)
        if (nrow_after < nrow(newdata)) {
          message(
            "After insert have ",
            nrow_after,
            " rows. Tried to insert ",
            nrow(newdata),
            ". Now trying upsert."
          )

          self$upsert_data(
            newdata = newdata,
            drop_indexes = NULL,
            verbose = verbose
          )
          nrow_after <- self$nrow(use_count = TRUE)
          if (nrow_after < nrow(newdata)) {
            message(
              "After upsert have ",
              nrow_after,
              " rows. Tried to upsert ",
              nrow(newdata),
              "."
            )
            stop("Upsert failed")
          }
        }
      }
    },

    #' @description
    #' Upserts data into the database table.
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector of the indexes to drop before the upsert (can increase performance).
    #' @param verbose Boolean.
    upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    ) {
      private$lazy_creation_of_table()
      if (is.null(newdata)) {
        return()
      }
      if (nrow(newdata) == 0) {
        return()
      }

      # newdata <- private$make_censored_data(newdata)

      validated <- self$validator_field_contents(newdata)
      if (!validated) {
        stop(glue::glue(
          "upsert_load_data_infile not validated in {self$table_name}. {attr(validated,'var')}"
        ))
      }

      # this will make the insert go faster, because
      # the data will be sorted

      infile <- random_file(
        private$load_folder_fn(),
        extra_insert = if (requireNamespace("digest", quietly = TRUE)) {
          digest::digest(newdata[1, ])
        } else {
          ""
        }
      )
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
    #' Drops all rows in the database table.
    drop_all_rows = function() {
      private$lazy_creation_of_table()
      drop_all_rows(
        connection = self$dbconnection$autoconnection,
        self$table_name_fully_specified_text
      )
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
      keep_rows_where(
        connection = self$dbconnection$autoconnection,
        self$table_name_short_for_mssql_fully_specified_for_postgres_text,
        condition
      )
      private$add_constraint()
    },

    #' @description
    #' Drops all rows in the database table and then upserts data.
    #' @param newdata The data to insert.
    #' @param drop_indexes A vector of the indexes to drop before the upsert (can increase performance).
    #' @param verbose Boolean.
    drop_all_rows_and_then_upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    ) {
      # The row count comes from the guard, which read it before the drop.
      # Never call nrow() here: a broken dim() method would then raise with
      # the table already empty.
      newdata_n <- private$check_newdata_before_drop_all_rows(
        newdata = newdata,
        method = "drop_all_rows_and_then_upsert_data"
      )
      private$lazy_creation_of_table()
      self$drop_all_rows()
      # A zero-row data.frame that passes the validator empties the table, and
      # raises nothing. cs9::DBPartitionedTableExtended_v9 clears every
      # partition this way.
      if (newdata_n == 0) {
        return(invisible(NULL))
      }
      self$upsert_data(
        newdata = newdata,
        drop_indexes = drop_indexes,
        verbose = verbose
      )
    },

    #' @description
    #' Drops all rows in the database table and then inserts data.
    #' @param newdata The data to insert.
    #' @param confirm_insert_via_nrow Checks nrow() before the insert and after the insert. If nrow() did not increase enough, the method attempts an upsert.
    #' @param verbose Boolean.
    drop_all_rows_and_then_insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    ) {
      newdata_n <- private$check_newdata_before_drop_all_rows(
        newdata = newdata,
        method = "drop_all_rows_and_then_insert_data"
      )
      private$lazy_creation_of_table()
      self$drop_all_rows()
      # See drop_all_rows_and_then_upsert_data() above for why the zero-row
      # branch returns rather than raises. The same comment says why the count
      # comes from the guard rather than from a second nrow() call.
      if (newdata_n == 0) {
        return(invisible(NULL))
      }
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
      retval <- self$dbconnection$autoconnection |>
        dplyr::tbl(self$table_name_short_for_mssql_fully_specified_for_postgres)

      return(retval)
    },

    #' @description
    #' Prints a template dplyr::select call that you can copy and paste for all your variables.
    print_dplyr_select = function() {
      private$lazy_creation_of_table()
      x <- self$tbl() |>
        head() |>
        dplyr::collect() |>
        names() |>
        paste0(., collapse = ",\n  ")
      x <- paste0("dplyr::select(\n  ", x, "\n) |>")
      cat(x)
    },

    #' @description
    #' Adds indexes to the database table from `self$indexes`. Creates each
    #' index in `self$indexes` exactly once, even when the table does not
    #' exist yet and this call is what creates it.
    #'
    #' The names in `self$indexes` are logical names. Each index reaches the
    #' database under a physical name. That name carries the table identity.
    #' Two tables in one schema that declare the same logical name therefore
    #' ask for different index names.
    #'
    #' After each create, the method reads the catalogue. It raises when the
    #' index is absent from this table, and when the index covers columns
    #' other than the declared ones.
    #'
    #' That check is defined for SQLite and for PostgreSQL, and for no other
    #' backend. On any other backend the method creates each index and does
    #' NOT verify it.
    add_indexes = function() {
      # The guard is load-bearing, not defensive. lazy_creation_of_table()
      # below calls create_table(), and create_table() ends by calling this
      # same method. On a table that does not exist yet the inner call added
      # every index, then the outer call resumed and added every index again.
      # Two declared indexes produced four attempts.
      #
      # `CREATE INDEX IF NOT EXISTS` made the second round a silent no-op on
      # PostgreSQL and on SQLite. On the default backend the try() inside
      # add_index() swallowed the duplicate-name error. That try() is gone
      # from 2026.8.16, so the second round is now a real failure.
      #
      # The guard lets the OUTER call do the work. The inner call returns at
      # once, and the outer call adds the indexes after create_table() has
      # returned, so the table is present.
      if (private$adding_indexes) {
        return(invisible(NULL))
      }
      private$adding_indexes <- TRUE
      # on.exit, not a plain assignment at the end. add_index() can now raise,
      # and a flag left TRUE would make every later call a silent no-op.
      on.exit(private$adding_indexes <- FALSE, add = TRUE)

      private$lazy_creation_of_table()
      for (i in names(self$indexes)) {
        private$add_declared_index(i)
      }
      invisible(NULL)
    },

    #' @description
    #' Drops all indexes from the database table.
    #'
    #' The method drops the physical name that `add_indexes()` created, for
    #' every logical name in `self$indexes`. An index that a legacy release
    #' created under the logical name is not dropped here.
    drop_indexes = function() {
      private$lazy_creation_of_table()
      for (i in names(self$indexes)) {
        message(glue::glue("Dropping index {i}"))
        # The DBI::Id field, not the _text field beside it. drop_index() reads
        # the schema out of this argument, and the text form cannot carry the
        # boundary between the schema and the table name. See
        # index_table_identity() in util_database.R.
        drop_index(
          connection = self$dbconnection$autoconnection,
          table = self$table_name_short_for_mssql_fully_specified_for_postgres,
          index = private$physical_index_name(i)
        )
      }
      invisible(NULL)
    },

    #' @description
    #' Confirms that the database holds every index declared in
    #' `self$indexes`, on this table, with the declared columns in the
    #' declared order.
    #'
    #' The method never drops an index to reconcile. It takes one of four
    #' actions per declared index:
    #'
    #' \itemize{
    #'   \item present with the declared columns: nothing.
    #'   \item absent: add it.
    #'   \item present with other columns: raise.
    #'   \item any index csdb did not name: ignore it.
    #' }
    #'
    #' The method reads an index definition on SQLite and on PostgreSQL only.
    #' On any other backend it checks the name alone, so it cannot see a
    #' change of columns.
    confirm_indexes = function() {
      private$lazy_creation_of_table()
      for (i in names(self$indexes)) {
        private$confirm_declared_index(i)
      }
      invisible(NULL)
    },

    #' @description
    #' Gets the number of rows in the database table.
    #' @param use_count If TRUE, then uses the count command, which is slow but accurate. If FALSE, then uses summary statistics, which is fast but inaccurate.
    nrow = function(use_count = FALSE) {
      if (use_count) {
        retval <- self$tbl() |>
          dplyr::summarize(n = dplyr::n()) |>
          dplyr::collect()
        retval <- retval$n
      } else {
        retval <- get_table_names_and_info(self$dbconnection$autoconnection)
        retval <- retval[table_name %in% self$table_name]$nrow
      }
      return(retval)
    },

    #' @description
    #' Gets the information about the database table.
    info = function() {
      retval <- get_table_names_and_info(self$dbconnection$autoconnection)
      retval <- retval[table_name %in% self$table_name]
      data.table::shouldPrint(retval)
      return(retval)
    }
  ),

  # private ----
  private = list(
    # TRUE only when initialize() built the DBConnection_v9 itself. The
    # default matches the behaviour before the dbconnection argument existed,
    # so a subclass that skips super$initialize() still disconnects.
    owns_dbconnection = TRUE,

    # Lazyload the creation of the table
    lazy_created_table = FALSE,

    # TRUE while add_indexes() runs. create_table() calls add_indexes(), and
    # add_indexes() reaches create_table() through lazy_creation_of_table(),
    # so the method can re-enter itself. See add_indexes() for the full path.
    adding_indexes = FALSE,

    # The name one declared index carries in the database. One helper keeps
    # the four naming sites together. They are add_indexes(), drop_indexes(),
    # confirm_indexes(), and the drop inside the default upsert method in
    # util_database.R.
    #
    # The identity is table_name_short_for_mssql_fully_specified_for_postgres
    # and NOT the _text field beside it. The fourth site receives that same
    # object, because upsert_data() passes it. It is a DBI::Id on PostgreSQL
    # and on the default backend, and an Id keeps the boundary between the
    # schema and the table name. The text form joins them with a dot and loses
    # that boundary. A schema or a table name that holds a dot then reads as
    # two components rather than one.
    physical_index_name = function(i) {
      index_physical_name(
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        index = i
      )
    },

    # The columns one physical index covers, in index order. NULL means the
    # backend has no catalogue reader. See the get_index_columns methods in
    # util_database.R.
    #
    # The DBI::Id field, and not the _text field beside it. Every
    # get_index_columns method reads the table name out of this argument as an
    # IDENTITY, never as SQL. It needs the boundary that only the Id keeps.
    # With the text form a table named `an.on.tab` read as three components,
    # the last of them `tab`. The method then reported that the index it had
    # just created was on no table.
    read_index_columns = function(physical) {
      get_index_columns(
        connection = self$dbconnection$autoconnection,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        index = physical
      )
    },

    # Compare the catalogue against the declaration.
    #
    # The comparison is case-insensitive because PostgreSQL folds an unquoted
    # column name to lowercase and SQLite keeps the case it was given. A
    # column list that differs only in case is the same list on both.
    index_columns_match = function(columns, i) {
      identical(tolower(columns), tolower(unname(self$indexes[[i]])))
    },

    # Create one declared index, then read the catalogue back.
    #
    # `CREATE INDEX IF NOT EXISTS` returning without an error says nothing
    # about which table now holds the name. It says nothing about the columns
    # either. Read the catalogue instead.
    add_declared_index = function(i) {
      physical <- private$physical_index_name(i)
      message(glue::glue("Adding index {i}"))

      # The DBI::Id field, and not the _text field beside it. Every add_index
      # method quotes this argument itself from 2026.8.16, and quoting needs
      # the boundary between the schema and the table name. Pre-quoted text
      # would be quoted a second time.
      add_index(
        connection = self$dbconnection$autoconnection,
        table = self$table_name_short_for_mssql_fully_specified_for_postgres,
        index = physical,
        keys = self$indexes[[i]]
      )

      columns <- private$read_index_columns(physical)
      if (is.null(columns)) {
        # Column verification is defined for SQLite and for PostgreSQL, and
        # for no other backend. NULL says this backend has no catalogue
        # reader, so nothing was measured. The index is created and is NOT
        # verified. Raising here instead would break every create on SQL
        # Server and on MySQL, which no test in this package covers.
        return(invisible(NULL))
      }
      if (length(columns) == 0L) {
        stop(glue::glue(
          "Index {i} is not on table {self$table_name} after creating it. ",
          "Its name there is {physical}. The statement raised nothing and ",
          "the catalogue holds no such index on this table."
        ))
      }
      if (!private$index_columns_match(columns, i)) {
        stop(glue::glue(
          "Index {i} on table {self$table_name} covers ",
          "{paste0(columns, collapse = ', ')}. ",
          "The code declares {paste0(self$indexes[[i]], collapse = ', ')}. ",
          "Its name there is {physical}."
        ))
      }
      invisible(NULL)
    },

    # Reconcile one declared index without dropping anything.
    confirm_declared_index = function(i) {
      physical <- private$physical_index_name(i)
      columns <- private$read_index_columns(physical)

      if (is.null(columns)) {
        # This backend has no catalogue reader, so fall back to existence by
        # name. Every backend answers that, and no backend answers the
        # columns unless it is SQLite or PostgreSQL.
        present <- physical %in%
          get_indexes(
            connection = self$dbconnection$autoconnection,
            table = self$table_name_short_for_mssql_fully_specified_for_postgres_text
          )
        if (!present) {
          private$add_declared_index(i)
        }
        return(invisible(NULL))
      }

      if (length(columns) == 0L) {
        private$add_declared_index(i)
        return(invisible(NULL))
      }

      if (!private$index_columns_match(columns, i)) {
        stop(glue::glue(
          "Index {i} on table {self$table_name} covers ",
          "{paste0(columns, collapse = ', ')}. ",
          "The code declares {paste0(self$indexes[[i]], collapse = ', ')}. ",
          "Its name there is {physical}. ",
          "Drop that index and call add_indexes(), or change the ",
          "declaration to match. confirm_indexes() does not drop an index."
        ))
      }
      invisible(NULL)
    },

    # The two drop_all_rows_and_then_* methods empty the table before they
    # write. This method rejects four kinds of newdata before the first row is
    # dropped. They are a NULL, an object that is not a data.frame, a row
    # count that is unusable or unstable, and data that the validator refuses.
    #
    # That list is the whole claim. It is not "the table is unchanged unless
    # the write completes". upsert_data() and insert_data() read newdata again
    # after the drop, so a dim() method that answers differently on a later
    # call still empties the table and then raises. Closing that needs a copy
    # of newdata or a transaction, and this release has neither. See NEWS.md.
    #
    # This check cannot live inside upsert_data() or insert_data(). Both of
    # those return early on a NULL and on a zero-row frame. Both return before
    # they reach the validator. A NULL is therefore not invalid data: the
    # validator never sees it. Measured on 2026-08-15, before this release: 2
    # rows before the call, 0 rows after it, and no error at all.
    #
    # The validator is self$validator_field_contents, the one that
    # upsert_data() and insert_data() already call. There is no second
    # validation rule here.
    #
    # The method returns the row count. The caller MUST use that returned
    # value and MUST NOT call nrow() again after the drop. See the row count
    # block below for what happens when it does.
    check_newdata_before_drop_all_rows = function(newdata, method) {
      if (!is.data.frame(newdata)) {
        if (is.null(newdata)) {
          stop(glue::glue(
            "newdata is NULL. {method}() on table {self$table_name} would ",
            "empty the table and write nothing back. Pass a zero-row ",
            "data.frame to empty the table on purpose."
          ))
        }
        stop(glue::glue(
          "newdata is not a data.frame. {method}() on table ",
          "{self$table_name} received an object of class ",
          "{paste0(class(newdata), collapse = ', ')}."
        ))
      }

      # nrow() runs here, before the drop, and the caller reuses this value.
      # nrow() reads dim(), and a data.frame subclass can carry a dim() method
      # that returns NA, NULL, Inf, or a different answer on each call.
      # is.data.frame() is TRUE on such an object, and a permissive validator
      # accepts it. Reading the count after the drop then raises on
      # `if (n == 0)`, with the table already empty.
      #
      # The two reads detect a count that changes between them. They do NOT
      # prove that a later read agrees.
      #
      # The check adds two dim() calls, and it does not copy newdata. Their
      # cost depends on the dispatched methods. dim() is a generic. An
      # arbitrary method can allocate, and it can be slow.
      as_text <- function(x) {
        out <- paste0(format(x), collapse = ", ")
        if (nzchar(out)) out else "a zero-length value"
      }
      n <- nrow(newdata)
      n_again <- nrow(newdata)
      if (
        !is.numeric(n) ||
          length(n) != 1L ||
          !is.finite(n) ||
          n < 0 ||
          n != trunc(n)
      ) {
        stop(glue::glue(
          "newdata has no usable row count. {method}() on table ",
          "{self$table_name} read nrow(newdata) as {as_text(n)}. A row count ",
          "MUST be one finite number, and MUST NOT be negative. Nothing was ",
          "dropped."
        ))
      }
      if (!isTRUE(n == n_again)) {
        stop(glue::glue(
          "newdata has an unstable row count. {method}() on table ",
          "{self$table_name} read nrow(newdata) as {as_text(n)}, and then as ",
          "{as_text(n_again)}. Nothing was dropped."
        ))
      }

      validated <- self$validator_field_contents(newdata)
      if (!isTRUE(validated)) {
        var <- attr(validated, "var")
        if (is.null(var)) {
          # glue() on a NULL returns character(0), and stop() would then carry
          # no message at all.
          var <- "not named by the validator"
        }
        stop(glue::glue(
          "newdata failed validator_field_contents. {method}() on table ",
          "{self$table_name} rejected it before dropping any row. ",
          "Field: {var}."
        ))
      }

      n
    },

    lazy_creation_of_table = function() {
      if (!private$lazy_created_table) {
        self$create_table()
        private$lazy_created_table <- TRUE
      }
    },

    check_fields_match = function() {
      fields <- DBI::dbListFields(
        self$dbconnection$autoconnection,
        self$table_name_short_for_mssql_fully_specified_for_postgres
      )
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
