# R6 Class representing a database connection

A robust database connection manager that handles connections to various
database systems including Microsoft SQL Server and PostgreSQL. This
class provides connection management, authentication, and automatic
reconnection capabilities.

## Details

The DBConnection_v9 class encapsulates database connection logic and
provides a consistent interface for connecting to different database
systems. It supports both trusted connections and user/password
authentication, handles connection failures gracefully, and provides
automatic reconnection functionality.

Key features:

- Support for multiple database systems (SQL Server, PostgreSQL)

- Automatic connection management with retry logic

- Secure credential handling

- Connection status monitoring

- Graceful error handling and recovery

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

- [`DBConnection_v9$new()`](#method-DBConnection_v9-new)

- [`DBConnection_v9$is_connected()`](#method-DBConnection_v9-is_connected)

- [`DBConnection_v9$print()`](#method-DBConnection_v9-print)

- [`DBConnection_v9$connect()`](#method-DBConnection_v9-connect)

- [`DBConnection_v9$disconnect()`](#method-DBConnection_v9-disconnect)

- [`DBConnection_v9$clone()`](#method-DBConnection_v9-clone)

------------------------------------------------------------------------

### Method `new()`

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

  Driver

- `server`:

  Server

- `port`:

  Port

- `db`:

  DB

- `schema`:

  Schema (e.g. "dbo")

- `user`:

  User

- `password`:

  Password

- `trusted_connection`:

  NULL or "yes"

- `sslmode`:

  NULL or "require"

- `role_create_table`:

  NULL or the role to take when creating tables.

#### Returns

A new \`DBConnection_v9\` object.

------------------------------------------------------------------------

### Method `is_connected()`

Is the DB schema connected?

#### Usage

    DBConnection_v9$is_connected()

#### Returns

TRUE/FALSE

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Class-specific print function.

#### Usage

    DBConnection_v9$print(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### Method `connect()`

Connect to the database

#### Usage

    DBConnection_v9$connect(attempts = 2)

#### Arguments

- `attempts`:

  Number of attempts to be made to try to connect

------------------------------------------------------------------------

### Method `disconnect()`

Disconnect from the database

#### Usage

    DBConnection_v9$disconnect()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DBConnection_v9$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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

# PostgreSQL example
pg_config <- DBConnection_v9$new(
  driver = "PostgreSQL",
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
