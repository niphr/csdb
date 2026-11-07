# Read a profile from a YAML file

Reads a file that
[`write_profile()`](https://niphr.github.io/csdb/reference/write_profile.md)
wrote. The function decodes each value by its field name and by the
`type` of its column.

## Usage

``` r
read_profile(path)
```

## Arguments

- path:

  The YAML file to read.

## Value

A `csdb_profile`. `identical(read_profile(write_profile(p, f)), p)` is
`TRUE`.

## See also

Other profile functions:
[`profile_data()`](https://niphr.github.io/csdb/reference/profile_data.md),
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md),
[`profile_table()`](https://niphr.github.io/csdb/reference/profile_table.md),
[`write_profile()`](https://niphr.github.io/csdb/reference/write_profile.md)

## Examples

``` r
d <- data.frame(day = as.Date("2024-01-01") + rep(0:1, each = 6))
p <- profile_data(d, c(day = "DATE"), columns = c(day = "full"))
f <- write_profile(p, tempfile(fileext = ".yaml"))
identical(read_profile(f), p)
#> [1] TRUE
```
