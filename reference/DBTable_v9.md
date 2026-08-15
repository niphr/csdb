# R6 Class representing a database table

A database table management class that provides operations for data
manipulation, schema validation, and table administration. This class
combines database connectivity with data validation and bulk operations.

## Details

The DBTable_v9 class is a database table abstraction that provides:

**Core functionality:**

- Table creation and schema management.

- Data insertion with bulk loading capabilities.

- Upsert operations (insert or update).

- Index management (creation, deletion).

- Data validation through customizable validators.

- Integration with dplyr for data queries.

**Advanced features:**

- Automatic table creation based on field specifications.

- Schema validation with custom validator functions.

- Efficient bulk data loading using database-specific methods.

- Index optimization for query performance.

- Cross-database compatibility (SQL Server, PostgreSQL).

**Data validation:** The class supports custom validation functions for
both field types and data contents, which ensure data integrity and
schema compliance.

## What the object creates in the database

One object creates three kinds of thing, and each carries its own name
rule.

- The table:

  Named `table_name`, in the schema that `dbconfig` names.

- The primary key constraint:

  Named `PK_` plus the fully specified table name, with every `.`, `[`
  and `]` deleted. Schema `anon` with table `anon_data` therefore gives
  `PK_anonanon_data`. Two different tables can reach one name, because
  the rule deletes the separator. Schema `a` with table `bc` and schema
  `ab` with table `c` both give `PK_abc`.

- One index per entry in `indexes`:

  The names you write in `indexes` are logical names. Each index reaches
  the database under a physical name of the form
  `ix_<slug>_<16 hexadecimal characters>`, at most 63 characters. The
  name carries the table identity, so two tables in one schema that both
  declare `ind1` get two indexes. `csdb:::index_physical_name()` returns
  the name for one table and one logical name.

## The case of a constraint name on PostgreSQL

The source writes `PK_`, in upper case. PostgreSQL folds an unquoted
identifier to lower case, so the catalogue stores `pk_`. Measured on the
`norsyss_data1` database on 2026-08-15: 92 lower case `pk_` constraint
names, and 0 upper case.

A `DROP CONSTRAINT` that quotes the source spelling therefore fails on
PostgreSQL. Write the name unquoted, or write it in lower case.

SQLite does not fold at all. It keeps `PK_MixedCase` exactly as the
source writes it, so the two backends disagree on one identifier.

The physical index name has no such trap. It is lower case already, so
it reads the same in the source and in both catalogues.

## See also

The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md).
It builds one of these on SQLite and inserts the bundled
`nor_covid19_cases_by_time_location` dataset. It also shows two tables
that declare one logical index name.
[`DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md)
takes the same arguments as the `dbconfig` list, and one is created here
to hold the connection.

Other database classes:
[`DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md)

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

- [`DBTable_v9$new()`](#method-DBTable_v9-initialize)

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

### `DBTable_v9$new()`

Create a new DBTable_v9 object.

#### Usage

    DBTable_v9$new(
      dbconfig,
      table_name,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = validator_field_types_blank,
      validator_field_contents = validator_field_contents_blank,
      dbconnection = NULL
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

- `dbconnection`:

  An existing `DBConnection_v9` to use, or NULL. The object borrows a
  supplied connection and does not own it. `disconnect()` then does
  nothing, so the caller decides when the connection closes. The object
  creates and owns a connection when this argument is NULL. It is the
  last argument, because a subclass can forward the earlier seven
  positionally.

#### Returns

A new \`DBTable_v9\` object.

------------------------------------------------------------------------

### `DBTable_v9$print()`

Class-specific print function.

#### Usage

    DBTable_v9$print(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### `DBTable_v9$connect()`

Connect to the database.

#### Usage

    DBTable_v9$connect()

------------------------------------------------------------------------

### `DBTable_v9$disconnect()`

Disconnect from the database. This does nothing when the connection came
from the `dbconnection` argument, because the caller owns that
connection.

#### Usage

    DBTable_v9$disconnect()

------------------------------------------------------------------------

### `DBTable_v9$table_exists()`

Does the table exist?

#### Usage

    DBTable_v9$table_exists()

------------------------------------------------------------------------

### `DBTable_v9$create_table()`

Create the database table.

#### Usage

    DBTable_v9$create_table()

------------------------------------------------------------------------

### `DBTable_v9$remove_table()`

Drop the database table.

#### Usage

    DBTable_v9$remove_table()

------------------------------------------------------------------------

### `DBTable_v9$insert_data()`

Inserts data into the database table.

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

  Checks nrow() before the insert and after the insert. If nrow() did
  not increase enough, the method attempts an upsert.

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### `DBTable_v9$upsert_data()`

Upserts data into the database table.

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

  A vector of the indexes to drop before the upsert (can increase
  performance).

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### `DBTable_v9$drop_all_rows()`

Drops all rows in the database table.

#### Usage

    DBTable_v9$drop_all_rows()

------------------------------------------------------------------------

### `DBTable_v9$drop_rows_where()`

Drops rows in the database table according to the SQL condition.

#### Usage

    DBTable_v9$drop_rows_where(condition)

#### Arguments

- `condition`:

  SQL text condition.

------------------------------------------------------------------------

### `DBTable_v9$keep_rows_where()`

Keeps rows in the database table according to the SQL condition.

#### Usage

    DBTable_v9$keep_rows_where(condition)

#### Arguments

- `condition`:

  SQL text condition.

------------------------------------------------------------------------

### `DBTable_v9$drop_all_rows_and_then_upsert_data()`

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

  A vector of the indexes to drop before the upsert (can increase
  performance).

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### `DBTable_v9$drop_all_rows_and_then_insert_data()`

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

  Checks nrow() before the insert and after the insert. If nrow() did
  not increase enough, the method attempts an upsert.

- `verbose`:

  Boolean.

------------------------------------------------------------------------

### `DBTable_v9$tbl()`

Provides access to the database table via dplyr::tbl.

#### Usage

    DBTable_v9$tbl()

------------------------------------------------------------------------

### `DBTable_v9$print_dplyr_select()`

Prints a template dplyr::select call that you can copy and paste for all
your variables.

#### Usage

    DBTable_v9$print_dplyr_select()

------------------------------------------------------------------------

### `DBTable_v9$add_indexes()`

Adds indexes to the database table from \`self\$indexes\`. Creates each
index in \`self\$indexes\` exactly once, even when the table does not
exist yet and this call is what creates it.

The names in \`self\$indexes\` are logical names. Each index reaches the
database under a physical name. That name carries the table identity.
Two tables in one schema that declare the same logical name therefore
ask for different index names.

After each create, the method reads the catalogue. It raises when the
index is absent from this table, and when the index covers columns other
than the declared ones.

That check is defined for SQLite and for PostgreSQL, and for no other
backend. On any other backend the method creates each index and does NOT
verify it.

#### Usage

    DBTable_v9$add_indexes()

------------------------------------------------------------------------

### `DBTable_v9$drop_indexes()`

Drops all indexes from the database table.

The method drops the physical name that \`add_indexes()\` created, for
every logical name in \`self\$indexes\`. An index that a legacy release
created under the logical name is not dropped here.

#### Usage

    DBTable_v9$drop_indexes()

------------------------------------------------------------------------

### `DBTable_v9$confirm_indexes()`

Confirms that the database holds every index declared in
\`self\$indexes\`, on this table, with the declared columns in the
declared order.

The method never drops an index to reconcile. It takes one of four
actions per declared index:

- present with the declared columns: nothing.

- absent: add it.

- present with other columns: raise.

- any index csdb did not name: ignore it.

The method reads an index definition on SQLite and on PostgreSQL only.
On any other backend it checks the name alone, so it cannot see a change
of columns.

#### Usage

    DBTable_v9$confirm_indexes()

------------------------------------------------------------------------

### `DBTable_v9$nrow()`

Gets the number of rows in the database table.

#### Usage

    DBTable_v9$nrow(use_count = FALSE)

#### Arguments

- `use_count`:

  If TRUE, then uses the count command, which is slow but accurate. If
  FALSE, then uses summary statistics, which is fast but inaccurate.

------------------------------------------------------------------------

### `DBTable_v9$info()`

Gets the information about the database table.

#### Usage

    DBTable_v9$info()

------------------------------------------------------------------------

### `DBTable_v9$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DBTable_v9$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Creating the object opens no connection, and the field types are
# checked while it is created. These field types do not satisfy the
# csfmt_rts_data_v1 schema, so the constructor stops.
try(DBTable_v9$new(
  dbconfig = list(driver = "PostgreSQL Unicode", server = "localhost"),
  table_name = "my_data_table",
  field_types = c("id" = "INTEGER"),
  keys = "id",
  validator_field_types = validator_field_types_csfmt_rts_data_v1
))
#> Error in initialize(...) : field_types not validated in my_data_table

# \donttest{
# A full cycle on SQLite, in a file that tempfile() names. SQLite needs
# no server, so this block runs anywhere. Name a driver of
# "ODBC Driver 17 for SQL Server" or "PostgreSQL Unicode" instead, and
# nothing else in the block changes.
db_config <- list(driver = "SQLite", db = tempfile(fileext = ".sqlite"))

# Indexes are named here, because add_indexes() takes no arguments and
# reads them from the object.
my_table <- DBTable_v9$new(
  dbconfig = db_config,
  table_name = "my_data_table",
  field_types = c(
    "id" = "INTEGER",
    "name" = "TEXT",
    "value" = "DOUBLE",
    "date_created" = "DATE"
  ),
  keys = "id",
  indexes = list("ind1" = c("name", "date_created")),
  validator_field_types = validator_field_types_blank,
  validator_field_contents = validator_field_contents_blank
)

my_table$create_table()
#> Creating table my_data_table
#> Adding index ind1

# insert_data() and upsert_data() need a data.table.
my_table$insert_data(data.table::data.table(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  value = c(10.5, 20.3, 15.7),
  date_created = as.Date("2023-01-01")
))

# tbl() returns a lazy dbplyr reference.
my_table$tbl() |>
  dplyr::filter(value > 15) |>
  dplyr::collect()
#> # A tibble: 2 × 4
#>      id name    value date_created
#>   <int> <chr>   <dbl> <date>      
#> 1     2 Bob      20.3 2023-01-01  
#> 2     3 Charlie  15.7 2023-01-01  

# Add the indexes that were named above.
my_table$add_indexes()
#> Adding index ind1

my_table$upsert_data(data.table::data.table(
  id = 2:4,
  name = c("Bob_Updated", "Charlie", "David"),
  value = c(25.0, 15.7, 30.2),
  date_created = as.Date("2023-01-02")
))
my_table$nrow()
#> [1] 4

my_table$disconnect()
# }
```
