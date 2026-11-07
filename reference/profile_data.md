# Profile a data frame

Writes `d` to an in-memory SQLite database and profiles it with
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).
Use it to profile data that is already in R.

## Usage

``` r
profile_data(d, field_types, ...)
```

## Arguments

- d:

  A data frame that holds every column of `field_types`.

- field_types:

  A named character vector of csdb field types, as in
  [`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).
  Columns of `d` that it does not name are not written.

- ...:

  Other arguments to
  [`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).
  Do not give `connection`, `sql` or `field_types`.

## Value

A `csdb_profile`, as
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md)
returns it. The query selects every column of the in-memory table
`profile_data`.

## See also

Other profile functions:
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md),
[`profile_table()`](https://niphr.github.io/csdb/reference/profile_table.md),
[`read_profile()`](https://niphr.github.io/csdb/reference/read_profile.md),
[`write_profile()`](https://niphr.github.io/csdb/reference/write_profile.md)

## Examples

``` r
d <- data.frame(
  day = as.Date("2024-01-01") + rep(0:3, each = 5),
  cases = rep(c(2L, 7L, 9L, 12L), each = 5)
)
p <- profile_data(
  d,
  field_types = c(day = "DATE", cases = "INTEGER"),
  columns = c(day = "full", cases = "full")
)
p$columns$day
#> $type
#> [1] "DATE"
#> 
#> $sensitivity
#> [1] "full"
#> 
#> $missing_share
#> [1] 0
#> 
#> $days
#> [1] "2024-01-01" "2024-01-02" "2024-01-03" "2024-01-04"
#> 
#> $n
#> [1] 5 5 5 5
#> 
```
