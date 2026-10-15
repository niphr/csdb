# Set the caller context

Sets a short string that names the caller, for example
`"<task>|<plan>"`. PostgreSQL shows it as the `application_name` of each
csdb connection in `pg_stat_activity`.

## Usage

``` r
csdb_set_context(x)
```

## Arguments

- x:

  One string. `""` clears the context.

## Value

Invisibly returns the previous context.

## Details

With a non-empty context, csdb runs `SET application_name` with the
context:

- right after each PostgreSQL connection that `DBConnection_v9` opens.
  After a failed SET, csdb runs `SELECT 1`. A connection that answers
  stays open and reports nothing. A connection that does not answer
  counts as a failed connect attempt.

- as the first statement of each `psql` load, after the login. A failed
  SET stops the load.

The name keeps only the letters A-Z and a-z, the digits 0-9 and the
characters `_ | . : -`. csdb drops every other character, and then keeps
the first 63 characters, the limit of PostgreSQL. A context that keeps
no character sends no SET.

The context is an option, so a forked worker inherits it. A new R
process, for example one that `callr` starts, does not.

Do not put a secret in the context. `pg_stat_activity` shows it.

## See also

Other caller context functions:
[`csdb_get_context()`](https://niphr.github.io/csdb/reference/csdb_get_context.md)

## Examples

``` r
previous <- csdb_set_context("my_task|plan7")
csdb_get_context()
#> [1] "my_task|plan7"
csdb_set_context(previous)
```
