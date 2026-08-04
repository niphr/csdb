# Set authentication hook for database connections

Register a function to be called when a database connection fails. This
is useful for refreshing Kerberos tickets or other authentication
credentials before retrying the connection.

## Usage

``` r
csdb_set_auth_hook(hook)
```

## Arguments

- hook:

  A function with no arguments that performs authentication, or NULL to
  clear the hook.

## Value

Invisibly returns the previous hook (if any).

## See also

[`DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md),
whose `connect()` method calls the registered hook once, after its first
failed attempt. The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not mention this function.

Other auth hook functions:
[`csdb_get_auth_hook()`](https://niphr.github.io/csdb/reference/csdb_get_auth_hook.md)

## Examples

``` r
# The hook is held in the csdb.auth_hook option. Setting one returns
# the previous hook, so it can be put back afterwards.
previous <- csdb_set_auth_hook(function() invisible(NULL))
is.function(csdb_get_auth_hook())
#> [1] TRUE
csdb_set_auth_hook(previous)
csdb_get_auth_hook()
#> NULL

if (FALSE) { # \dontrun{
# A real hook refreshes credentials, e.g. a Kerberos ticket
csdb_set_auth_hook(function() {
  system2("/bin/authenticate.sh", stdout = NULL)
})

# Clear the hook
csdb_set_auth_hook(NULL)
} # }
```
