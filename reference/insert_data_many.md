# Insert data into several tables in one transaction

Inserts one data.table into each table of a list. On PostgreSQL, one
`psql` session loads every table in one transaction, so by default the
call stores every row or no row. The one exception is a duplicate key
with `confirm_insert_via_nrow = TRUE`: csdb then inserts table by table.

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

  The longest time in seconds that the `psql` session MAY run. csdb then
  stops it with an error of class `csdb_load_ambiguous`.

## Value

`NULL`, invisibly.

## Details

On PostgreSQL, csdb starts `psql` once, with `--single-transaction`, and
runs one `\copy` for each table. A table whose data.table is `NULL` or
has no rows gets no `\copy`. After each `\copy`, the script compares the
stored rows with the rows sent. A difference raises an error before the
commit, so the server stores no row in any table.

The transaction holds a lock on every table until it commits, so a
`TRUNCATE` or an index change on one of them waits. The session waits at
most 60 s for one lock. After a longer wait, a deadlock or a refused
login, csdb runs the whole session again, at most 3 attempts in total. A
connection that breaks after the load starts is not run again, and the
error says to count the rows first. So is a row count that differs after
`psql` exits.

A duplicate key stops the session. With
`confirm_insert_via_nrow = TRUE`, csdb then calls
`$insert_data(confirm_insert_via_nrow = TRUE)` on each table, which
upserts a table that stops on the duplicate key.

With another driver, or with tables that have different connection
settings, csdb calls `$insert_data()` on each table in turn.

## Examples

``` r
# \donttest{
# SQLite has no psql, so csdb inserts into each table in turn here.
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
