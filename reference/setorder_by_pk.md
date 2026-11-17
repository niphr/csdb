# Order a data.table by the primary key of a table

`setorder_by_pk()` orders `d` by reference. It sorts first on the
primary key columns of `table`, and then on every other column of `d`.

## Usage

``` r
setorder_by_pk(d, table = NULL)
```

## Arguments

- d:

  A data.table. The function changes the row order of `d` by reference.
  A data.frame or a tibble stops with an error, because the function
  cannot change it by reference. Convert it first with
  [`data.table::setDT()`](https://rdrr.io/pkg/data.table/man/setDT.html).

- table:

  `NULL`, or an object with a field `keys` that holds the primary key as
  a character vector. A
  [DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md) has
  this field. A plain `list(keys = c("b", "a"))` also works.

## Value

`d`, sorted. The return value is the same object as the argument `d`, so
the function works at the end of a pipe and in an assignment.

## Details

A database does not define the order of the rows that a query returns.
PostgreSQL can return the same rows in a different order on each call.
Code that reads `d[1]`, `d[.N]` or `tail(d, 1)` after
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
therefore gets an arbitrary row. Call `setorder_by_pk()` on the result
to make that row deterministic.

The sort columns are, in this order:

1.  the columns of `table$keys` that `d` has, in the order of
    `table$keys`.

2.  every other column of `d`, in the column order of `d`.

The function ignores a column of `table$keys` that `d` does not have.
With `table = NULL`, or with `table$keys` equal to `NULL`, the function
sorts on every column of `d`. Each column sorts in ascending order, with
`NA` first. A list column or a complex column is no sort column, because
[`data.table::setorderv()`](https://rdrr.io/pkg/data.table/man/setorder.html)
cannot sort it.

The function matches the key by column name only. The result is always
in one fixed order. It is the primary key order only while the key
columns keep their names and values.

For a query that starts at
[DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md)`$tbl()`,
use
[`collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.md).
It collects and sorts in one step.

## See also

Other row order:
[`collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.md),
[`set_pk()`](https://niphr.github.io/csdb/reference/set_pk.md)

## Examples

``` r
d <- data.table::data.table(
  a = c(2L, 1L, 2L, 1L),
  b = c("y", "y", "x", "x"),
  v = c(4, 3, 2, 1)
)
setorder_by_pk(d, list(keys = c("b", "a")))
#>        a      b     v
#>    <int> <char> <num>
#> 1:     1      x     1
#> 2:     2      x     2
#> 3:     1      y     3
#> 4:     2      y     4
d
#>        a      b     v
#>    <int> <char> <num>
#> 1:     1      x     1
#> 2:     2      x     2
#> 3:     1      y     3
#> 4:     2      y     4

# Without a table, the function sorts on every column.
setorder_by_pk(d)
#>        a      b     v
#>    <int> <char> <num>
#> 1:     1      x     1
#> 2:     1      y     3
#> 3:     2      x     2
#> 4:     2      y     4
```
