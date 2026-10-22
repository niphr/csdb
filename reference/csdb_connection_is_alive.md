# Does a database connection still reach its server?

Returns FALSE when a catalog request on `connection` shows that the
connection lost its server. Use it before cleanup SQL on a connection
that MAY have lost its server.

## Usage

``` r
csdb_connection_is_alive(connection)
```

## Arguments

- connection:

  A DBI connection, or NULL.

## Value

TRUE or FALSE. FALSE for NULL, for a closed connection, and when the
catalog request gives a condition that shows a lost connection.

## Details

[`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
cannot answer this question. It reports TRUE on an odbc connection whose
server ended the session. On such a connection, the first statement
gives an R error. The second statement crashes R with a segfault in odbc
1.7.0 with psqlODBC 13.02, and
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) cannot catch a
segfault.

The function asks the server whether the table `csdb_connection_probe`
exists. odbc sends that request as an ODBC catalog call, not as a
statement. On a lost connection it gives an R error, also after a failed
statement. This was measured on 2026-10-22 against PostgreSQL 16.15.

Only a condition whose message shows a lost connection gives FALSE. Any
other error or warning of the request gives TRUE, so a live connection
still gets its cleanup SQL. See `csdb_connection_lost_pattern` in the
source for the messages.

PostgreSQL releases the advisory locks, the role and the temporary
tables of a session when the session ends. Cleanup SQL for them then has
nothing to do.

## Examples

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
csdb_connection_is_alive(con)
#> [1] TRUE
DBI::dbDisconnect(con)
csdb_connection_is_alive(con)
#> [1] FALSE
```
