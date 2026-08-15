# Get table names, number of rows, and size information

Gets information about database tables: their names, their row counts,
and their storage sizes. The function is a generic, with one method for
each supported database system.

## Usage

``` r
get_table_names_and_info(connection)
```

## Arguments

- connection:

  A database connection object, for example from
  [`dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html).

## Value

A data.table containing table information with columns:

- table_name:

  Character. Name of the table.

- nrow:

  Numeric. The row count as the database reports it. On PostgreSQL it is
  `reltuples` from `pg_class`, which is an estimate. On Microsoft SQL
  Server it is the `rows` column of `sp_spaceused`. On SQLite it is
  `COUNT(*)`, which is exact rather than an estimate.

- size_total_gb:

  Numeric. Total size of the table in gigabytes. `NA_real_` on SQLite.

- size_data_gb:

  Numeric. Size of data in gigabytes. `NA_real_` on SQLite.

- size_index_gb:

  Numeric. Size of indexes in gigabytes. `NA_real_` on SQLite.

SQLite reports no per-table size. The `dbstat` virtual table is the only
source of one. The SQLite build that `RSQLite` ships does not compile
`dbstat` in: a query against it fails with `no such table: dbstat`.
`pragma page_count` and `pragma page_size` exist, but they describe the
whole file rather than one table, so all three size columns are
`NA_real_`.

## See also

[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
whose `info()` method and whose `nrow(use_count = FALSE)` method call
this function. The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not mention this function.

## Examples

``` r
# \donttest{
# SQLite needs no server, so this block runs anywhere. The three size
# columns are NA_real_ here, and carry a number on the other two
# backends.
con <- DBI::dbConnect(RSQLite::SQLite(), tempfile(fileext = ".sqlite"))
DBI::dbWriteTable(con, "cases", data.frame(id = 1:3, n = c(7, 8, 9)))

get_table_names_and_info(con)
#>    table_name  nrow size_total_gb size_data_gb size_index_gb
#>        <char> <num>         <num>        <num>         <num>
#> 1:      cases     3            NA           NA            NA

DBI::dbDisconnect(con)
# }

if (FALSE) { # \dontrun{
# A server backend needs a running server, so this block cannot run
# here. Methods exist for the "PostgreSQL" and "Microsoft SQL Server"
# connection classes that odbc creates.
con <- DBI::dbConnect(odbc::odbc(),
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  database = "mydb",
  uid = "user",
  password = "pass"
)
get_table_names_and_info(con)
DBI::dbDisconnect(con)
} # }
```
