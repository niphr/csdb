# Attach a primary key to a lazy table

`set_pk()` attaches `keys` to `x`, so that
[`collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.md)
can sort the result.
[DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`
calls it for every table. Call it for a lazy table that you build
yourself, for example from SQL text with
[`dplyr::sql()`](https://dplyr.tidyverse.org/reference/sql.html), when
its rows have a primary key.

## Usage

``` r
set_pk(x, keys)
```

## Arguments

- x:

  A lazy table.

- keys:

  A character vector with the primary key columns, or `NULL`. `NULL`
  removes the key, and
  [`collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.md)
  then stops.

## Value

`x`, with the key attached.

## See also

Other row order:
[`collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.md),
[`setorder_by_pk()`](https://niphr.github.io/csdb/reference/setorder_by_pk.md)

## Examples

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "t", data.frame(a = c(2L, 1L), b = c("y", "x")))
q <- set_pk(dplyr::tbl(con, dplyr::sql("SELECT a, b FROM t")), "a")
collect_and_sort_by_pk(q)
#>        a      b
#>    <int> <char>
#> 1:     1      x
#> 2:     2      y
DBI::dbDisconnect(con)
```
