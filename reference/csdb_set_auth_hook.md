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

## Examples

``` r
if (FALSE) { # \dontrun{
# Set an auth hook to refresh Kerberos credentials
csdb_set_auth_hook(function() {
  system2("/bin/authenticate.sh", stdout = NULL)
})

# Clear the hook
csdb_set_auth_hook(NULL)
} # }
```
