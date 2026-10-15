# Get the caller context

Returns the string that
[`csdb_set_context()`](https://niphr.github.io/csdb/reference/csdb_set_context.md)
set.

## Usage

``` r
csdb_get_context()
```

## Value

One string, `""` when no context is set.

## See also

Other caller context functions:
[`csdb_set_context()`](https://niphr.github.io/csdb/reference/csdb_set_context.md)

## Examples

``` r
csdb_get_context()
#> [1] ""
```
