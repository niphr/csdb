# Get the current authentication hook

Returns the currently registered authentication hook function.

## Usage

``` r
csdb_get_auth_hook()
```

## Value

The current auth hook function, or NULL if none is set.

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
