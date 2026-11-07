# Profile a DBTable_v9

Profiles every row of a
[DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md) with
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).
The table supplies the query, the field types and the keys.

## Usage

``` r
profile_table(table, columns = NULL, ...)
```

## Arguments

- table:

  A [DBTable_v9](https://niphr.github.io/csdb/reference/DBTable_v9.md).

- columns:

  The sensitivity of each column, as in
  [`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).

- ...:

  Other arguments to
  [`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).
  Do not give `connection`, `sql`, `field_types` or `keys`.

## Value

A `csdb_profile`, as
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md)
returns it. The query is `dbplyr::sql_render(table$tbl())`.

## See also

Other profile functions:
[`profile_data()`](https://niphr.github.io/csdb/reference/profile_data.md),
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md),
[`read_profile()`](https://niphr.github.io/csdb/reference/read_profile.md),
[`write_profile()`](https://niphr.github.io/csdb/reference/write_profile.md)

## Examples

``` r
tab <- DBTable_v9$new(
  dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
  table_name = "visits",
  field_types = c(id = "INTEGER", sex = "TEXT"),
  keys = "id",
  validator_field_types = validator_field_types_blank,
  validator_field_contents = validator_field_contents_blank
)
tab$create_table()
#> Creating table visits
#> NULL
tab$insert_data(data.table::data.table(
  id = 1:12,
  sex = rep(c("female", "male"), 6)
))
p <- profile_table(tab, columns = c(sex = "full"))
p$columns$sex
#> $type
#> [1] "TEXT"
#> 
#> $sensitivity
#> [1] "full"
#> 
#> $missing_share
#> [1] 0
#> 
#> $levels
#> [1] "female" "male"  
#> 
#> $n
#> [1] 6 6
#> 
tab$dbconnection$disconnect()
```
