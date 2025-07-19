
#' Get table names, number of rows, and size information
#' 
#' Retrieves comprehensive information about database tables including their names,
#' row counts, and storage size metrics. This function provides database-specific
#' implementations for different database systems.
#' 
#' @param connection A database connection object (e.g., from \code{\link[DBI]{dbConnect}})
#' @return A data.table containing table information with columns:
#' \describe{
#'   \item{table_name}{Character. Name of the table}
#'   \item{nrow}{Numeric. Number of rows in the table}
#'   \item{size_total_gb}{Numeric. Total size of the table in gigabytes}
#'   \item{size_data_gb}{Numeric. Size of data in gigabytes}
#'   \item{size_index_gb}{Numeric. Size of indexes in gigabytes}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Microsoft SQL Server example
#' con <- DBI::dbConnect(odbc::odbc(), 
#'                       driver = "ODBC Driver 17 for SQL Server",
#'                       server = "localhost", 
#'                       database = "mydb")
#' table_info <- get_table_names_and_info(con)
#' print(table_info)
#' DBI::dbDisconnect(con)
#' 
#' # PostgreSQL example  
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                       host = "localhost",
#'                       dbname = "mydb",
#'                       user = "user")
#' table_info <- get_table_names_and_info(con)
#' print(table_info)
#' DBI::dbDisconnect(con)
#' }
#' @export
get_table_names_and_info <- function(connection) UseMethod("get_table_names_and_info")

#' @export
`get_table_names_and_info.Microsoft SQL Server` <- function(connection) {
  # Declare variables to avoid R CMD check NOTEs
  . <- NULL
  table_name <- NULL
  name <- NULL
  size_total_gb <- NULL
  reserved <- NULL
  size_data_gb <- NULL
  data <- NULL
  size_index_gb <- NULL
  index_size <- NULL
  rows <- NULL
  nrow <- NULL
  # table_rows <- connection %>%
  #   DBI::dbGetQuery("select o.name as table_name, i.rowcnt as n from sys.objects o join sys.sysindexes i on o.object_id = i.id where o.is_ms_shipped = 0 and i.rowcnt > 0 order by o.name") %>%
  #   setDT() %>% unique()

  # update stats
  # connection %>% DBI::dbExecute("sp_updatestats")
  # get the stats
  table_rows <- connection |>
    DBI::dbGetQuery("sp_msforeachtable 'sp_spaceused [?]'") |>
    setDT()
  table_rows[, size_total_gb := round(as.numeric(stringr::str_extract_all(reserved, "[0-9]+"))/1024/1024, digits = 2)]
  table_rows[, size_data_gb := round(as.numeric(stringr::str_extract_all(data, "[0-9]+"))/1024/1024, digits = 2)]
  table_rows[, size_index_gb := round(as.numeric(stringr::str_extract_all(index_size, "[0-9]+"))/1024/1024, digits = 2)]
  table_rows[, nrow := as.numeric(stringr::str_extract_all(rows, "[0-9]+"))]

  table_rows <- table_rows[,.(
    table_name = name,
    nrow,
    size_total_gb,
    size_data_gb,
    size_index_gb
  )]
  setorder(table_rows, table_name)

  data.table::shouldPrint(table_rows)
  return(table_rows)
}

#' @export
get_table_names_and_info.PostgreSQL <- function(connection) {

  sql = "SELECT table_name
     , row_estimate AS nrow
     , cast(total_bytes as decimal)/1073741824 AS size_total_gb
     , cast(data_bytes as decimal)/1073741824 AS size_data_gb
     , cast(index_bytes as decimal)/1073741824 AS size_index_gb
  FROM (
     SELECT *,
         total_bytes-index_bytes AS data_bytes
     FROM (
         SELECT nspname AS table_schema
                , relname AS table_name
                , c.reltuples AS row_estimate
                , pg_total_relation_size(c.oid) AS total_bytes
                , pg_indexes_size(c.oid) AS index_bytes
            FROM pg_class c
            LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
            WHERE relkind = 'r'
     ) raw_storage
  ) storage_with_data_size
  order by table_name;"

  table_rows = DBI::dbGetQuery(connection, sql) |> setDT()

  data.table::shouldPrint(table_rows)
  return(table_rows)
}
