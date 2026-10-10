# Get the current password hook

Returns the function that
[`csdb_set_password_hook`](https://niphr.github.io/csdb/reference/csdb_set_password_hook.md)
registered.

## Usage

``` r
csdb_get_password_hook()
```

## Value

The current password hook function, or NULL when no hook is set.

## See also

Other password hook functions:
[`csdb_set_password_hook()`](https://niphr.github.io/csdb/reference/csdb_set_password_hook.md)

## Examples

``` r
# Returns NULL when no hook is set.
csdb_get_password_hook()
#> NULL
```
