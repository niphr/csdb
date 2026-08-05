# Get table names, number of rows, and size information

Retrieves comprehensive information about database tables including
their names, row counts, and storage size metrics. This function
provides database-specific implementations for different database
systems.

## Usage

``` r
get_table_names_and_info(connection)
```

## Arguments

- connection:

  A database connection object (e.g., from
  [`dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html))

## Value

A data.table containing table information with columns:

- table_name:

  Character. Name of the table

- nrow:

  Numeric. The row count as the database reports it: `reltuples` from
  `pg_class` on PostgreSQL, which is an estimate, and the `rows` column
  of `sp_spaceused` on Microsoft SQL Server. On SQLite it is `COUNT(*)`,
  which is exact rather than an estimate

- size_total_gb:

  Numeric. Total size of the table in gigabytes. `NA_real_` on SQLite

- size_data_gb:

  Numeric. Size of data in gigabytes. `NA_real_` on SQLite

- size_index_gb:

  Numeric. Size of indexes in gigabytes. `NA_real_` on SQLite

SQLite reports no per-table size. The `dbstat` virtual table, which is
the only thing that could give one, is not compiled into the SQLite that
`RSQLite` ships: querying it fails with `no such table: dbstat`.
`pragma page_count` and `pragma page_size` exist, but they describe the
whole file rather than a table, so all three size columns are
`NA_real_`.

## See also

[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
whose `info()` method and whose `nrow(use_count = FALSE)` method call
this function. The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not mention this function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Microsoft SQL Server example
con <- DBI::dbConnect(odbc::odbc(),
  driver = "ODBC Driver 17 for SQL Server",
  server = "localhost",
  database = "mydb"
)
table_info <- get_table_names_and_info(con)
print(table_info)
DBI::dbDisconnect(con)

# PostgreSQL example. Methods exist for the "PostgreSQL" and
# "Microsoft SQL Server" connection classes that odbc creates.
con <- DBI::dbConnect(odbc::odbc(),
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  database = "mydb",
  uid = "user",
  password = "pass"
)
table_info <- get_table_names_and_info(con)
print(table_info)
DBI::dbDisconnect(con)
} # }
```
