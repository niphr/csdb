# Set the password hook for PostgreSQL connections

Registers a function that returns the password for each new PostgreSQL
connection and each `psql` load. Use it for a password that expires,
such as an Entra access token.

## Usage

``` r
csdb_set_password_hook(hook)
```

## Arguments

- hook:

  A function with no arguments that returns the password, or NULL to
  clear the hook.

## Value

Invisibly returns the previous hook, or NULL when no hook was set.

## Details

csdb calls the hook with no arguments, and uses its current return value
as the password:

- once for each attempt of `DBConnection_v9$connect()` with the driver
  `"PostgreSQL Unicode"`.

- once for each PostgreSQL load or upsert that runs `psql`.

The hook MUST return a single non-empty string. Any other value stops
the connection or the load with an error that names the hook. The error
does not show the value.

The hook applies to PostgreSQL only. A SQL Server, SQLite or other
connection, and a `bcp` load, use the `password` in their settings. csdb
therefore never sends the hook value to another kind of server.

The message of a failed connection shows `***` in place of the hook
value.

## See also

[`csdb_set_auth_hook`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md)
registers a function that `DBConnection_v9$connect()` calls after its
first failed attempt.

Other password hook functions:
[`csdb_get_password_hook()`](https://niphr.github.io/csdb/reference/csdb_get_password_hook.md)

## Examples

``` r
# The hook is held in the csdb.password_hook option. Registering the hook
# does not call it.
previous <- csdb_set_password_hook(function() "example-token")
is.function(csdb_get_password_hook())
#> [1] TRUE

# Put back whatever was registered before.
csdb_set_password_hook(previous)
csdb_get_password_hook()
#> NULL
```
