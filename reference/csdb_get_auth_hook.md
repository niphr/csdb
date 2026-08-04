# Get the current authentication hook

Returns the currently registered authentication hook function.

## Usage

``` r
csdb_get_auth_hook()
```

## Value

The current auth hook function, or NULL if none is set.

## See also

[`DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md),
whose `connect()` method calls this function to look up the hook. The
introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not mention this function.

Other auth hook functions:
[`csdb_set_auth_hook()`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md)

## Examples

``` r
# Returns NULL when no hook has been set
csdb_get_auth_hook()
#> NULL

if (FALSE) { # \dontrun{
# Register a hook and then retrieve it
csdb_set_auth_hook(function() system2("/bin/kinit", stdout = NULL))
hook <- csdb_get_auth_hook()
is.function(hook)
} # }
```
