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
#'   \item{nrow}{Numeric. The row count as the database reports it:
#'     \code{reltuples} from \code{pg_class} on PostgreSQL, which is an
#'     estimate, and the \code{rows} column of \code{sp_spaceused} on
#'     Microsoft SQL Server. On SQLite it is \code{COUNT(*)}, which is exact
#'     rather than an estimate}
#'   \item{size_total_gb}{Numeric. Total size of the table in gigabytes.
#'     \code{NA_real_} on SQLite}
#'   \item{size_data_gb}{Numeric. Size of data in gigabytes.
#'     \code{NA_real_} on SQLite}
#'   \item{size_index_gb}{Numeric. Size of indexes in gigabytes.
#'     \code{NA_real_} on SQLite}
#' }
#'
#' SQLite reports no per-table size. The \code{dbstat} virtual table, which is
#' the only thing that could give one, is not compiled into the SQLite that
#' \code{RSQLite} ships: querying it fails with \code{no such table: dbstat}.
#' \code{pragma page_count} and \code{pragma page_size} exist, but they
#' describe the whole file rather than a table, so all three size columns are
#' \code{NA_real_}.
#' @export
#' @seealso \code{\link{DBTable_v9}}, whose \code{info()} method and whose
#'   \code{nrow(use_count = FALSE)} method call this function.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not mention this function.
#' @examples
#' \dontrun{
#' # Microsoft SQL Server example
#' con <- DBI::dbConnect(odbc::odbc(),
#'   driver = "ODBC Driver 17 for SQL Server",
#'   server = "localhost",
#'   database = "mydb"
#' )
#' table_info <- get_table_names_and_info(con)
#' print(table_info)
#' DBI::dbDisconnect(con)
#'
#' # PostgreSQL example. Methods exist for the "PostgreSQL" and
#' # "Microsoft SQL Server" connection classes that odbc creates.
#' con <- DBI::dbConnect(odbc::odbc(),
#'   driver = "PostgreSQL Unicode",
#'   server = "localhost",
#'   port = 5432,
#'   database = "mydb",
#'   uid = "user",
#'   password = "pass"
#' )
#' table_info <- get_table_names_and_info(con)
#' print(table_info)
#' DBI::dbDisconnect(con)
#' }
#' @export
get_table_names_and_info <- function(connection) {
  UseMethod("get_table_names_and_info")
}

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
  table_rows[,
    size_total_gb := round(
      as.numeric(stringr::str_extract_all(reserved, "[0-9]+")) / 1024 / 1024,
      digits = 2
    )
  ]
  table_rows[,
    size_data_gb := round(
      as.numeric(stringr::str_extract_all(data, "[0-9]+")) / 1024 / 1024,
      digits = 2
    )
  ]
  table_rows[,
    size_index_gb := round(
      as.numeric(stringr::str_extract_all(index_size, "[0-9]+")) / 1024 / 1024,
      digits = 2
    )
  ]
  table_rows[, nrow := as.numeric(stringr::str_extract_all(rows, "[0-9]+"))]

  table_rows <- table_rows[, .(
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
  sql <- "SELECT table_name
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

  table_rows <- DBI::dbGetQuery(connection, sql) |> setDT()

  data.table::shouldPrint(table_rows)
  return(table_rows)
}

#' @export
get_table_names_and_info.SQLiteConnection <- function(connection) {
  # Declare variables to avoid R CMD check NOTEs
  table_name <- NULL

  # `sqlite_master` is the catalogue. The LIKE clause removes SQLite's own
  # objects, notably `sqlite_sequence` behind an AUTOINCREMENT column, which
  # is a real table and would otherwise be reported as a user table.
  #
  # The ESCAPE clause is mandatory, not decoration. `_` is a single-character
  # wildcard in SQL LIKE, so the unescaped `NOT LIKE 'sqlite_%'` also hides
  # every user table whose name begins "sqlite" followed by any character at
  # all. A table named `sqliteFoo` would then be missing from this result, and
  # `DBTable_v9$nrow(use_count = FALSE)` and `DBTable_v9$info()` would return
  # nothing for it. Written `'sqlite\\_%' ESCAPE '\\'` in R, so a literal
  # backslash reaches SQLite.
  names_tables <- DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT name FROM sqlite_master ",
      "WHERE type = 'table' ",
      "AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\' ",
      "ORDER BY name"
    )
  )$name

  # One COUNT(*) per table, so `nrow` is exact. The other two backends read a
  # stored estimate; SQLite keeps no row count, and on a file-sized database
  # the scan is cheap.
  nrow_per_table <- vapply(
    names_tables,
    function(x) {
      as.numeric(DBI::dbGetQuery(
        connection,
        paste0("SELECT COUNT(*) FROM ", DBI::dbQuoteIdentifier(connection, x))
      )[[1]])
    },
    numeric(1),
    USE.NAMES = FALSE
  )

  # Built column by column rather than from a query, so an empty database
  # still returns all five columns, with zero rows, and the callers in
  # DBTable_v9$nrow() and DBTable_v9$info() can subset it unconditionally.
  table_rows <- data.table::data.table(
    table_name = names_tables,
    nrow = nrow_per_table,
    size_total_gb = NA_real_,
    size_data_gb = NA_real_,
    size_index_gb = NA_real_
  )
  setorder(table_rows, table_name)

  data.table::shouldPrint(table_rows)
  return(table_rows)
}
