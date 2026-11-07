# A profile that holds only a schema

Builds a `csdb_profile` in which every column is `schema_only`. Use it
with
[`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md)
for a table that has no real profile.

## Usage

``` r
profile_schema(field_types, keys = NULL)
```

## Arguments

- field_types:

  A named character vector of csdb field types: `TEXT`, `INTEGER`,
  `DOUBLE`, `BOOLEAN`, `DATE` or `DATETIME`.

- keys:

  A character vector of key column names, or `NULL`.

## Value

A `csdb_profile`. Each column has `type`, `sensitivity` `"schema_only"`
and `missing_share` 0. `n_rows` is 0, and `provenance$source` is
`"schema"`.

## See also

Other simulation functions:
[`profile_compare()`](https://niphr.github.io/csdb/reference/profile_compare.md),
[`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md)

## Examples

``` r
p <- profile_schema(c(id = "INTEGER", sex = "TEXT"), keys = "id")
simulate_from_profile(p, n = 5)
#>       id    sex
#>    <int> <char>
#> 1:     1  sex_9
#> 2:     2  sex_4
#> 3:     3  sex_7
#> 4:     4  sex_1
#> 5:     5  sex_2
```
