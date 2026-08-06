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

  Database connection.

- `autoconnection`:

  Database connection that automatically connects if possible.

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

#### Usage

    DBConnection_v9$connect(attempts = 2)

#### Arguments

- `attempts`:

  Number of attempts to connect.

------------------------------------------------------------------------

### `DBConnection_v9$disconnect()`

Disconnect from the database.

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

if (FALSE) { # \dontrun{
# Create a SQL Server connection
db_config <- DBConnection_v9$new(
  driver = "ODBC Driver 17 for SQL Server",
  server = "localhost",
  port = 1433,
  db = "mydb",
  user = "myuser",
  password = "mypass"
)

# Connect to the database
db_config$connect()

# Check connection status
db_config$is_connected()

# Use the connection
tables <- DBI::dbListTables(db_config$connection)

# Disconnect when done
db_config$disconnect()

# PostgreSQL example. Only "PostgreSQL Unicode" reaches the
# PostgreSQL branch of the connection code.
pg_config <- DBConnection_v9$new(
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  db = "mydb",
  user = "myuser",
  password = "mypass"
)

pg_config$connect()
# ... use connection ...
pg_config$disconnect()
} # }
```
