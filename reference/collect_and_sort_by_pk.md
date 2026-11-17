# Collect a query and order it by the primary key of its table

`collect_and_sort_by_pk()` replaces
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
for a query that starts at
[DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`.
It returns a data.table, ordered by the primary key of that table and
then by every other column.

## Usage

``` r
collect_and_sort_by_pk(x)
```

## Arguments

- x:

  A lazy table that starts at
  [DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`.

## Value

A data.table with the rows of `x`, sorted as described in Details.

## Details

PostgreSQL returns rows in no fixed order. The same query can return the
same rows in a different order on each call. Code that reads `d[1]`,
`d[.N]` or `tail(d, 1)` after
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
therefore gets an arbitrary row. `collect_and_sort_by_pk()` makes that
row deterministic.

[DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`
attaches the primary key of the table to the lazy table as the attribute
`csdb_pk`. These dbplyr verbs keep the attribute on their result:
[`filter()`](https://rdrr.io/r/stats/filter.html), `select()`,
`mutate()`, `group_by()`, `summarise()`, `arrange()`, `distinct()`,
[`head()`](https://rdrr.io/r/utils/head.html), and `left_join()` and
`inner_join()` on the `x` side.

The function sorts with
[`setorder_by_pk()`](https://niphr.github.io/csdb/reference/setorder_by_pk.md).
The primary key columns that the result has come first, in primary key
order. Every other column follows, in column order. A primary key column
that a `select()` or a `summarise()` removed is not a sort column.

The attribute holds column names. After a `rename()` or a `mutate()` of
a primary key column, the order is still fixed, but it is no longer the
primary key order.

The sort runs after the collect. It cannot choose which rows a
[`head()`](https://rdrr.io/r/utils/head.html) or another limit inside
the query returns. Put a
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
before the limit when the rows matter.

The function stops with an error when `x` carries no primary key. It
does not fall back to an unsorted collect. These are the causes:

- the query did not start at
  [DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`.

- the table has no primary key, because its `keys` is `NULL`.

- a step dropped the attribute, for example
  [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html),
  or a join where the csdb table is on the `y` side.

For a result without a primary key, for example from
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html),
use
[`setorder_by_pk()`](https://niphr.github.io/csdb/reference/setorder_by_pk.md)
with `table = NULL`.

## See also

Other row order:
[`set_pk()`](https://niphr.github.io/csdb/reference/set_pk.md),
[`setorder_by_pk()`](https://niphr.github.io/csdb/reference/setorder_by_pk.md)

## Examples

``` r
# \donttest{
# SQLite needs no server, so this block runs anywhere.
my_table <- DBTable_v9$new(
  dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
  table_name = "my_data_table",
  field_types = c("id" = "INTEGER", "week" = "INTEGER", "n" = "DOUBLE"),
  keys = c("id", "week")
)
my_table$insert_data(data.table::data.table(
  id = c(2L, 1L, 2L, 1L),
  week = c(2L, 2L, 1L, 1L),
  n = c(4, 3, 2, 1)
))
#> Creating table my_data_table

d <- my_table$tbl() |>
  dplyr::filter(id == 1L) |>
  collect_and_sort_by_pk()

# The last row is the last week of id 1, on every database.
d[.N]
#>       id  week     n
#>    <int> <int> <num>
#> 1:     1     2     3

my_table$disconnect()
# }
```
