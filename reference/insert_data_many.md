# Insert data into several tables in one transaction

Inserts one data.table into each table of a list. On PostgreSQL, one
transaction loads every table, so by default the call stores every row
or no row. The one exception is a duplicate key with
`confirm_insert_via_nrow = TRUE`: csdb then inserts table by table.

## Usage

``` r
insert_data_many(
  tables,
  newdata,
  confirm_insert_via_nrow = FALSE,
  verbose = TRUE,
  load_timeout = 3600
)
```

## Arguments

- tables:

  A list of `DBTable_v9` objects.

- newdata:

  A list of data.tables, one for each table, in the order of `tables`.

- confirm_insert_via_nrow:

  See `DBTable_v9$insert_data()`.

- verbose:

  See `DBTable_v9$insert_data()`.

- load_timeout:

  The longest time in seconds that the load MAY run. On PostgreSQL, the
  server cancels the statement that runs past this time and stores no
  row, and the error has class `csdb_load_other`.

## Value

`NULL`, invisibly.

## Details

On PostgreSQL, one transaction on the connection of the first table
loads every table. csdb sends the rows as JSON text, at most 500,000
rows in one statement, and starts no client. A table whose data.table is
`NULL` or has no rows gets no statement. After each statement, csdb
compares the stored rows with the rows sent. A difference raises an
error before the commit, so the server stores no row in any table.

The transaction holds a lock on every table until it commits, so a
`TRUNCATE` or an index change on one of them waits. The transaction
waits at most 60 s for one lock. After a longer wait, a deadlock or a
serialization failure, csdb runs the whole load again, at most 3
attempts in total. A load whose connection breaks, or whose COMMIT
fails, does not run again. The error then says to count the rows first.

A duplicate key stops the load. With `confirm_insert_via_nrow = TRUE`,
csdb then calls `$insert_data(confirm_insert_via_nrow = TRUE)` on each
table, which upserts a table that stops on the duplicate key.

With another driver, or with tables that have different connection
settings, csdb calls `$insert_data()` on each table in turn.

## Examples

``` r
# \donttest{
# SQLite has no JSON load, so csdb inserts into each table in turn here.
db_config <- list(driver = "SQLite", db = tempfile(fileext = ".sqlite"))
connection <- DBConnection_v9$new(driver = "SQLite", db = db_config$db)
tables <- lapply(c("cases_a", "cases_b"), function(name) {
  DBTable_v9$new(
    dbconfig = db_config,
    table_name = name,
    field_types = c("id" = "INTEGER", "n" = "INTEGER"),
    keys = "id",
    dbconnection = connection
  )
})
insert_data_many(
  tables,
  list(
    data.table::data.table(id = 1:2, n = c(5L, 7L)),
    data.table::data.table(id = 1L, n = 3L)
  )
)
#> Creating table cases_a
#> Creating table cases_b
tables[[1]]$nrow()
#> [1] 2
connection$disconnect()
# }
```
