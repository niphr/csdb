# R6 Class representing a database connection

A database connection manager that handles connections to various
database systems including Microsoft SQL Server and PostgreSQL. This
class provides connection management, authentication, and automatic
reconnection.

## Details

The DBConnection_v9 class holds the database connection logic and
provides a consistent interface to different database systems. It
supports both trusted connections and user/password authentication. It
handles connection failures, and it reconnects automatically.

Key features:

- Support for multiple database systems (SQL Server, PostgreSQL).

- Automatic connection management with retry logic.

- Secure credential handling.

- Connection status monitoring.

- Graceful error handling and recovery.

- A connection is never shared with another process.

## Fork safety

A connection belongs to the process that opened it. After a fork, the
child holds a copy of this object and a copy of the parent's connection.
Both processes then use one socket. PostgreSQL can return wrong results
and report no error. Measured against the NorSySS server on 2026-08-14.
A child asked for `select 4` and read 3. The parent asked for
`select 999` and read 2.
[`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
reports TRUE on such a handle, so nothing else detects it.

This class records the process that opens each connection. It drops any
connection whose recorded process is not the current one.
`is_connected()` then returns FALSE, `connection` returns NULL, and
`autoconnection` opens a new connection for the current process.
`disconnect()` closes nothing, because the handle belongs to the other
process.

The object never closes an inherited handle, and it keeps a reference to
it. Both parts are needed. A close, by
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
or by the garbage collector, would close the other process's socket.

## See also

The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
which creates one of these, connects, and disconnects again.
[`csdb_set_auth_hook`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md)
registers the function that `connect()` calls after its first failed
attempt.

Other database classes:
[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md)

## Public fields

- `config`:

  Configuration details of the database.

## Active bindings

- `connection`:

  Database connection. NULL when another process opened it.

- `autoconnection`:

  Database connection that automatically connects if possible. After a
  fork it opens a connection for the current process.

## Methods

### Public methods

- [`DBConnection_v9$new()`](#method-DBConnection_v9-initialize)

- [`DBConnection_v9$is_connected()`](#method-DBConnection_v9-is_connected)

- [`DBConnection_v9$print()`](#method-DBConnection_v9-print)

- [`DBConnection_v9$connect()`](#method-DBConnection_v9-connect)

- [`DBConnection_v9$disconnect()`](#method-DBConnection_v9-disconnect)

- [`DBConnection_v9$clone()`](#method-DBConnection_v9-clone)

------------------------------------------------------------------------

### `DBConnection_v9$new()`

Create a new DBConnection_v9 object.

#### Usage

    DBConnection_v9$new(
      driver = NULL,
      server = NULL,
      port = NULL,
      db = NULL,
      schema = NULL,
      user = NULL,
      password = NULL,
      trusted_connection = NULL,
      sslmode = NULL,
      role_create_table = NULL
    )

#### Arguments

- `driver`:

  Driver.

- `server`:

  Server.

- `port`:

  Port.

- `db`:

  DB.

- `schema`:

  Schema (e.g. "dbo").

- `user`:

  User.

- `password`:

  Password.

- `trusted_connection`:

  NULL or "yes".

- `sslmode`:

  NULL or "require".

- `role_create_table`:

  NULL or the role to take when creating tables.

#### Returns

A new \`DBConnection_v9\` object.

------------------------------------------------------------------------

### `DBConnection_v9$is_connected()`

Is the DB schema connected?

A connection that another process opened does not count. The method
drops that connection first, and then reports FALSE.

#### Usage

    DBConnection_v9$is_connected()

#### Returns

TRUE/FALSE.

------------------------------------------------------------------------

### `DBConnection_v9$print()`

Class-specific print function.

#### Usage

    DBConnection_v9$print(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### `DBConnection_v9$connect()`

Connect to the database.

The method drops a connection that another process opened, and then
opens a connection for the current process.

#### Usage

    DBConnection_v9$connect(attempts = 2)

#### Arguments

- `attempts`:

  Number of attempts to connect.

------------------------------------------------------------------------

### `DBConnection_v9$disconnect()`

Disconnect from the database.

The method closes only a connection that this process opened. A
connection that another process opened stays open.

#### Usage

    DBConnection_v9$disconnect()

------------------------------------------------------------------------

### `DBConnection_v9$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DBConnection_v9$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Creating the object stores the settings. It opens no connection,
# so this runs without a database server.
db <- DBConnection_v9$new(
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  db = "mydb",
  user = "myuser",
  password = "mypass"
)
db$is_connected()
#> [1] FALSE
db
#> (disconnected)
#> 
#> Driver:              PostgreSQL Unicode 
#> Server:              localhost 
#> Port:                5432 
#> DB:                  mydb 
#> User:                myuser 
#> Password:            ****** 
#> SSL mode:            x 
#> 

# \donttest{
# The full cycle, on SQLite. SQLite needs no server, so this block runs
# anywhere. Only the driver and the db argument change for a server.
# vignette("backends", package = "csdb") puts the two configurations
# side by side.
sqlite_db <- DBConnection_v9$new(
  driver = "SQLite",
  db = tempfile(fileext = ".sqlite")
)

sqlite_db$connect()
sqlite_db$is_connected()
#> [1] TRUE
DBI::dbListTables(sqlite_db$connection)
#> character(0)

sqlite_db$disconnect()
sqlite_db$is_connected()
#> [1] FALSE

# $autoconnection opens the file again, so a read after a disconnect
# still works.
DBI::dbListTables(sqlite_db$autoconnection)
#> character(0)
sqlite_db$disconnect()
# }
```
