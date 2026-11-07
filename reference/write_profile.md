# Write a profile to a YAML file

Writes a `csdb_profile` as YAML.
[`read_profile()`](https://niphr.github.io/csdb/reference/read_profile.md)
reads the file back to an object identical to `profile`.

## Usage

``` r
write_profile(profile, path)
```

## Arguments

- profile:

  A `csdb_profile`, from
  [`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md).

- path:

  The file to write. The function overwrites it.

## Value

`path`, invisibly.

## See also

Other profile functions:
[`profile_data()`](https://niphr.github.io/csdb/reference/profile_data.md),
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md),
[`profile_table()`](https://niphr.github.io/csdb/reference/profile_table.md),
[`read_profile()`](https://niphr.github.io/csdb/reference/read_profile.md)

## Examples

``` r
d <- data.frame(sex = rep(c("female", "male"), 6))
p <- profile_data(d, c(sex = "TEXT"), columns = c(sex = "full"))
f <- write_profile(p, tempfile(fileext = ".yaml"))
cat(readLines(f), sep = "\n")
#> provenance:
#>   format: 1
#>   source: ~
#>   query: SELECT * FROM `profile_data`
#>   driver: SQLiteConnection
#>   profiled_at: '2026-11-07T16:39:23Z'
#>   k: 5
#>   noise: ~
#>   max_levels: 50
#>   csdb_version: 2026.11.7
#> field_types:
#>   sex: TEXT
#> keys: []
#> n_rows: 12
#> columns:
#>   sex:
#>     type: TEXT
#>     sensitivity: full
#>     missing_share: 0.0
#>     levels:
#>     - female
#>     - male
#>     'n':
#>     - 6
#>     - 6
#> group_sizes: []
#> crosstabs: []
#> dependencies: []
#> notes: []
```
