# R6 Class representing a database table with advanced data management capabilities

A comprehensive database table management class that provides high-level
operations for data manipulation, schema validation, and table
administration. This class combines database connectivity with data
validation and efficient bulk operations.

## Details

The DBTable_v9 class is a sophisticated database table abstraction that
provides:

**Core functionality:**

- Table creation and schema management

- Data insertion with bulk loading capabilities

- Upsert operations (insert or update)

- Index management (creation, deletion)

- Data validation through customizable validators

- Integration with dplyr for data queries

**Advanced features:**

- Automatic table creation based on field specifications

- Schema validation with custom validator functions

- Efficient bulk data loading using database-specific methods

- Index optimization for query performance

- Cross-database compatibility (SQL Server, PostgreSQL)

**Data validation:** The class supports custom validation functions for
both field types and data contents, ensuring data integrity and schema
compliance.

## Public fields

- `dbconnection`:

  Database connection.

- `dbconfig`:

  Configuration details of the database.

- `table_name`:

  Name of the table in the database.

- `table_name_short_for_mssql_fully_specified_for_postgres`:

  Fully specified name of the table in the database (e.g.
  \\db\\.\\dbo\\.\\table_name\\).

- `table_name_short_for_mssql_fully_specified_for_postgres_text`:

  Fully specified name of the table in the database (e.g.
  \\db\\.\\dbo\\.\\table_name\\).

- `table_name_fully_specified`:

  Fully specified name of the table in the database (e.g.
  \\db\\.\\dbo\\.\\table_name\\).

- `table_name_fully_specified_text`:

  Fully specified name of the table in the database (e.g.
  \\db\\.\\dbo\\.\\table_name\\) as a text string.

- `field_types`:

  The types of each column in the database table (INTEGER, DOUBLE, TEXT,
  BOOLEAN, DATE, DATETIME).

- `field_types_with_length`:

  The same as `field_types` but with `(100)` added to the end of all
  TEXT fields.

- `keys`:

  The combination of variables that uniquely identify each row in the
  database.

- `keys_with_length`:

  The same as `keys` but with `(100)` added to the end of all TEXT
  fields.

- `indexes`:

  A named list of vectors (generally "ind1", "ind2", etc.) that improves
  the speed of data retrieval operations on a database table.

- `validator_field_contents`:

  A function that validates the data before it is inserted into the
  database.

- `load_folder`:

  A temporary folder that is used to write data to before inserting into
  the database.

- `censors`:

  A named list of censors.

## Methods

### Public methods

- [`DBTable_v9$new()`](#method-DBTable_v9-new)

- [`DBTable_v9$print()`](#method-DBTable_v9-print)

- [`DBTable_v9$connect()`](#method-DBTable_v9-connect)

- [`DBTable_v9$disconnect()`](#method-DBTable_v9-disconnect)

- [`DBTable_v9$table_exists()`](#method-DBTable_v9-table_exists)

- [`DBTable_v9$create_table()`](#method-DBTable_v9-create_table)

- [`DBTable_v9$remove_table()`](#method-DBTable_v9-remove_table)

- [`DBTable_v9$insert_data()`](#method-DBTable_v9-insert_data)

- [`DBTable_v9$upsert_data()`](#method-DBTable_v9-upsert_data)

- [`DBTable_v9$drop_all_rows()`](#method-DBTable_v9-drop_all_rows)

- [`DBTable_v9$drop_rows_where()`](#method-DBTable_v9-drop_rows_where)

- [`DBTable_v9$keep_rows_where()`](#method-DBTable_v9-keep_rows_where)

- [`DBTable_v9$drop_all_rows_and_then_upsert_data()`](#method-DBTable_v9-drop_all_rows_and_then_upsert_data)

- [`DBTable_v9$drop_all_rows_and_then_insert_data()`](#method-DBTable_v9-drop_all_rows_and_then_insert_data)

- [`DBTable_v9$tbl()`](#method-DBTable_v9-tbl)

- [`DBTable_v9$print_dplyr_select()`](#method-DBTable_v9-print_dplyr_select)

- [`DBTable_v9$add_indexes()`](#method-DBTable_v9-add_indexes)

- [`DBTable_v9$drop_indexes()`](#method-DBTable_v9-drop_indexes)

- [`DBTable_v9$confirm_indexes()`](#method-DBTable_v9-confirm_indexes)

- [`DBTable_v9$nrow()`](#method-DBTable_v9-nrow)

- [`DBTable_v9$info()`](#method-DBTable_v9-info)

- [`DBTable_v9$clone()`](#method-DBTable_v9-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DBTable_v9 object.

#### Usage

    DBTable_v9$new(
      dbconfig,
      table_name,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = validator_field_types_blank,
      validator_field_contents = validator_field_contents_blank
    )

#### Arguments

- `dbconfig`:

  Configuration details of the database (driver, server, port, db,
  schema, user, password, trusted_connection, sslmode,
  role_create_table).

- `table_name`:

  Name of the table in the database.

- `field_types`:

  The types of each column in the database table (INTEGER, DOUBLE, TEXT,
  BOOLEAN, DATE, DATETIME).

- `keys`:

  The combination of these variables uniquely identifies each row of
  data in the table.

- `indexes`:

  A named list of vectors (generally "ind1", "ind2", etc.) that improves
  the speed of data retrieval operations on a database table.

- `validator_field_types`:

  A function that validates the `field_types` before the DB schema is
  created.

- `validator_field_contents`:

  A function that validates the data before it is inserted into the
  database.

#### Returns

A new \`DBTable_v9\` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Class-specific print function.

#### Usage

    DBTable_v9$print(...)

#### Arguments

- `...`:

  Not in use.

------------------------------------------------------------------------

### Method `connect()`

Connect from the database

#### Usage

    DBTable_v9$connect()

------------------------------------------------------------------------

### Method `disconnect()`

Disconnect from the database

#### Usage

    DBTable_v9$disconnect()

------------------------------------------------------------------------

### Method `table_exists()`

Does the table exist

#### Usage

    DBTable_v9$table_exists()

------------------------------------------------------------------------

### Method `create_table()`

Create the database table

#### Usage

    DBTable_v9$create_table()

------------------------------------------------------------------------

### Method `remove_table()`

Drop the database table

#### Usage

    DBTable_v9$remove_table()

------------------------------------------------------------------------

### Method `insert_data()`

Inserts data

#### Usage

    DBTable_v9$insert_data(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    )

#### Arguments

- `newdata`:

  The data to insert.

- `confirm_insert_via_nrow`:

  Checks nrow() before insert and after insert. If nrow() has not
  increased sufficiently, then attempt an upsert.

- `verbose`:

  Boolean. Inserts data into the database table

------------------------------------------------------------------------

### Method `upsert_data()`

Upserts data into the database table

#### Usage

    DBTable_v9$upsert_data(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    )

#### Arguments

- `newdata`:

  The data to insert.

- `drop_indexes`:

  A vector containing the indexes to be dropped before upserting (can
  increase performance).

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### Method `drop_all_rows()`

Drops all rows in the database table

#### Usage

    DBTable_v9$drop_all_rows()

------------------------------------------------------------------------

### Method `drop_rows_where()`

Drops rows in the database table according to the SQL condition.

#### Usage

    DBTable_v9$drop_rows_where(condition)

#### Arguments

- `condition`:

  SQL text condition.

------------------------------------------------------------------------

### Method `keep_rows_where()`

Keeps rows in the database table according to the SQL condition.

#### Usage

    DBTable_v9$keep_rows_where(condition)

#### Arguments

- `condition`:

  SQL text condition.

------------------------------------------------------------------------

### Method `drop_all_rows_and_then_upsert_data()`

Drops all rows in the database table and then upserts data.

#### Usage

    DBTable_v9$drop_all_rows_and_then_upsert_data(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    )

#### Arguments

- `newdata`:

  The data to insert.

- `drop_indexes`:

  A vector containing the indexes to be dropped before upserting (can
  increase performance).

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### Method `drop_all_rows_and_then_insert_data()`

Drops all rows in the database table and then inserts data.

#### Usage

    DBTable_v9$drop_all_rows_and_then_insert_data(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    )

#### Arguments

- `newdata`:

  The data to insert.

- `confirm_insert_via_nrow`:

  Checks nrow() before insert and after insert. If nrow() has not
  increased sufficiently, then attempt an upsert.

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### Method `tbl()`

Provides access to the database table via dplyr::tbl.

#### Usage

    DBTable_v9$tbl()

------------------------------------------------------------------------

### Method `print_dplyr_select()`

Prints a template dplyr::select call that you can easily copy/paste for
all your variables.

#### Usage

    DBTable_v9$print_dplyr_select()

------------------------------------------------------------------------

### Method `add_indexes()`

Adds indexes to the database table from \`self\$indexes\`

#### Usage

    DBTable_v9$add_indexes()

------------------------------------------------------------------------

### Method `drop_indexes()`

Drops all indees from the database table

#### Usage

    DBTable_v9$drop_indexes()

------------------------------------------------------------------------

### Method `confirm_indexes()`

Confirms that the names and number of indexes in the database are the
same as in the R code. Does not confirm the contents of the indexes!

#### Usage

    DBTable_v9$confirm_indexes()

------------------------------------------------------------------------

### Method [`nrow()`](https://rdrr.io/r/base/nrow.html)

Gets the number of rows in the database table

#### Usage

    DBTable_v9$nrow(use_count = FALSE)

#### Arguments

- `use_count`:

  If true, then uses the count command, which is slow but accurate. If
  false, then uses summary statistics, which is fast but inaccurate.

------------------------------------------------------------------------

### Method `info()`

Gets the information about the database table

#### Usage

    DBTable_v9$info()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DBTable_v9$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create database connection
db_config <- list(
  driver = "ODBC Driver 17 for SQL Server",
  server = "localhost",
  db = "mydb",
  user = "myuser",
  password = "mypass"
)

# Define table schema
field_types <- c(
  "id" = "INTEGER",
  "name" = "TEXT",
  "value" = "DOUBLE",
  "date_created" = "DATE"
)

# Create table object
my_table <- DBTable_v9$new(
  dbconfig = db_config,
  table_name = "my_data_table",
  field_types = field_types,
  keys = c("id"),
  validator_field_types = validator_field_types_blank,
  validator_field_contents = validator_field_contents_blank
)

# Create table in database
my_table$create_table()

# Insert data
sample_data <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  value = c(10.5, 20.3, 15.7),
  date_created = as.Date("2023-01-01")
)
my_table$insert_data(sample_data)

# Query data using dplyr
result <- my_table$tbl() |>
  dplyr::filter(value > 15) |>
  dplyr::collect()

# Add indexes for performance
my_table$add_indexes(c("name", "date_created"))

# Upsert (insert or update) data
new_data <- data.frame(
  id = 2:4,
  name = c("Bob_Updated", "Charlie", "David"),
  value = c(25.0, 15.7, 30.2),
  date_created = as.Date("2023-01-02")
)
my_table$upsert_data(new_data)
} # }
```
