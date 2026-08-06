# Introduction to csdb

## What csdb is for

`csdb` puts a large surveillance dataset into a database and takes it
out again, without you writing SQL. You describe a table once: its
columns and their types, its primary key, and its indexes. You then
insert, upsert, delete and read rows through R methods. Before it
writes, it can check that your data matches an agreed column format, so
a table cannot quietly change shape between runs.

### Two classes, and the split between them

`DBConnection_v9` is the connection: the settings, plus `$connect()`,
`$disconnect()` and `$autoconnection`. `DBTable_v9` is one table: its
field types, its keys, its indexes, and every read and write method.

You do not have to build the connection first. `DBTable_v9$new()` takes
the same settings as a `dbconfig` list, builds its own
`DBConnection_v9`, and keeps it at `$dbconnection`. Build a
`DBConnection_v9` yourself when you want a connection without a table.

Neither constructor touches the database. Both return an object that is
not connected:

``` r
con <- csdb::DBConnection_v9$new(driver = "SQLite", db = tempfile(fileext = ".sqlite"))
con$is_connected()
#> [1] FALSE

tab <- csdb::DBTable_v9$new(
  dbconfig = list(
    driver = "PostgreSQL Unicode",
    server = "localhost",
    port = 5432,
    db = "mydb",
    schema = "public",
    user = "u",
    password = "p"
  ),
  table_name = "cases",
  field_types = c("location_code" = "TEXT", "date" = "DATE", "cases_n" = "INTEGER"),
  keys = c("location_code", "date")
)
tab$dbconnection$is_connected()
#> [1] FALSE
```

The second block names a PostgreSQL server that is not running, and
still returns an object. The first method that reaches the database is
what connects.

### The validators, and one gap

A validator is a function you hand to `DBTable_v9$new()`. `csdb` exports
six:

``` r
sort(grep("^validator_", getNamespaceExports("csdb"), value = TRUE))
#> [1] "validator_field_contents_blank"            
#> [2] "validator_field_contents_csfmt_rts_data_v1"
#> [3] "validator_field_contents_csfmt_rts_data_v2"
#> [4] "validator_field_types_blank"               
#> [5] "validator_field_types_csfmt_rts_data_v1"   
#> [6] "validator_field_types_csfmt_rts_data_v2"
```

The `_blank` pair accepts anything. The other four check two csverse
surveillance formats, `csfmt_rts_data_v1` and `csfmt_rts_data_v2`. A
`_field_types` validator runs once, inside `$new()`. A `_field_contents`
validator runs inside `$insert_data()` and `$upsert_data()`.

**There is no `csfmt_rts_data_v3` validator.** That is a current
limitation, and it matters now. `cstidy` marks v1 and v2 deprecated, and
points new work at `set_csfmt_rts_data_v3()`. `csalert`’s pipeline ends
in `ens_collapse(heal = TRUE)`, which returns a `csfmt_rts_data_v3`. So
today’s analysis output has no validator here that matches it. You can
still store it: pass the `_blank` pair, or a function of your own.
`csdb` then checks nothing about the columns.

### The three backends

| Backend              | Connects through                                 | Needs a server |
|----------------------|--------------------------------------------------|----------------|
| PostgreSQL           | `odbc`, driver `"PostgreSQL Unicode"`            | Yes            |
| Microsoft SQL Server | `odbc`, driver `"ODBC Driver 17 for SQL Server"` | Yes            |
| SQLite               | `RSQLite`, where `db` is a file path             | No             |

One `dbconfig` list picks the backend, and nothing else in your code
changes.
[`vignette("backends", package = "csdb")`](https://niphr.github.io/csdb/articles/backends.md)
puts a PostgreSQL configuration and a SQLite configuration side by side,
and lists what differs between them.

### Where csdb sits

`csdb` is the storage layer under the csverse surveillance stack. It
imports `csutil`. Its validators are written against `cstidy`’s data
formats, by column name and type. No `csdb` code calls `cstidy` or
`csalert`, and neither is a dependency.

``` r
library(data.table)
library(magrittr)
```

## Overview

`csdb` provides an abstracted database-access layer for the Core
Surveillance (csverse) ecosystem. The package exposes two R6 classes:

- **`DBConnection_v9`** — wraps a single database connection and manages
  its lifecycle (connect, disconnect, reconnect).
- **`DBTable_v9`** — represents one database table. It inserts, upserts
  and deletes rows, manages indexes, and validates field types and
  contents.

csdb supports three backends. Microsoft SQL Server and PostgreSQL
connect over ODBC. Pass their server, port, user and password as
arguments to `DBConnection_v9$new()`. csdb never reads an environment
variable itself. To keep credentials out of scripts and version control,
read the variable yourself and pass the result in. SQLite connects
through `RSQLite`, where `db` is a file path. csdb reads no server,
port, user or password for SQLite.

This vignette runs on SQLite, in a file created by
[`tempfile()`](https://rdrr.io/r/base/tempfile.html). Every chunk below
therefore executes on a machine with no database server. For what
changes when you point the same code at PostgreSQL instead, see
[`vignette("backends", package = "csdb")`](https://niphr.github.io/csdb/articles/backends.md).

## `DBConnection_v9`

`DBConnection_v9$new()` stores connection parameters but does not open a
connection immediately. `$connect()` opens the connection;
`$disconnect()` closes it. The `$autoconnection` field returns an active
connection, and reconnects automatically when needed.

The example below creates a connection object, connects, inspects the
connection fields, then disconnects. After you disconnect, `$connection`
reports `DISCONNECTED`, while
[`class()`](https://rdrr.io/r/base/class.html) still reports the driver
type, and a read of `$autoconnection` opens the file again.

## `DBTable_v9`

`DBTable_v9$new()` takes a `dbconfig` list (the same parameters as
`DBConnection_v9`), plus `table_name`, `field_types`, `keys`, `indexes`,
and validators. Keys define the primary key used for upsert operations.
Validators can enforce field-type and field-content constraints;
`validator_field_types_blank` and `validator_field_contents_blank` skip
validation entirely.

The example below creates a table object backed by the same SQLite file.
It clears any existing rows and inserts the bundled
`nor_covid19_cases_by_time_location` dataset. It then connects and
returns a lazy `tbl()` reference via dbplyr. The file is new, so
`$drop_all_rows()` creates the table and its index first and then
reports that it removed 0 rows.

``` r
db_file <- tempfile(fileext = ".sqlite")

dbconnection <- csdb::DBConnection_v9$new(
  driver = "SQLite",
  db = db_file
)
dbconnection
#> (disconnected)
#> 
#> Driver:              SQLite 
#> File:                /tmp/Rtmp3tTfJa/file3a0c741e4a2129.sqlite
dbconnection$connect()

dbconnection$connection
#> <SQLiteConnection>
#>   Path: /tmp/Rtmp3tTfJa/file3a0c741e4a2129.sqlite
#>   Extensions: TRUE
dbconnection$autoconnection
#> <SQLiteConnection>
#>   Path: /tmp/Rtmp3tTfJa/file3a0c741e4a2129.sqlite
#>   Extensions: TRUE
dbconnection
#> (connected)
#> 
#> Driver:              SQLite 
#> File:                /tmp/Rtmp3tTfJa/file3a0c741e4a2129.sqlite

dbconnection$disconnect()
dbconnection$connection
#> <SQLiteConnection>
#>   DISCONNECTED
class(dbconnection$connection)
#> [1] "SQLiteConnection"
#> attr(,"package")
#> [1] "RSQLite"
class(dbconnection$autoconnection)
#> [1] "SQLiteConnection"
#> attr(,"package")
#> [1] "RSQLite"

dbtable <- csdb::DBTable_v9$new(
  dbconfig = list(
    driver = "SQLite",
    db = db_file
  ),
  table_name = "anon_test",
  field_types = c(
      "granularity_time" = "TEXT",
      "granularity_geo" = "TEXT",
      "country_iso3" = "TEXT",
      "location_code" = "TEXT",
      "border" = "INTEGER",
      "age" = "TEXT",
      "sex" = "TEXT",
      "isoyear" = "INTEGER",
      "isoweek" = "INTEGER",
      "isoyearweek" = "TEXT",
      "season" = "TEXT",
      "seasonweek" = "DOUBLE",
      "calyear" = "INTEGER",
      "calmonth" = "INTEGER",
      "calyearmonth" = "TEXT",
      "date" = "DATE",
      "covid19_cases_testdate_n" = "INTEGER",
      "covid19_cases_testdate_pr100000" = "DOUBLE"
    ),
    keys = c(
      "granularity_time",
      "location_code",
      "date",
      "age",
      "sex"
    ),
    indexes = list(
      "ind1" = c("granularity_time", "granularity_geo", "country_iso3", "location_code", "border", "age", "sex", "date", "isoyear", "isoweek", "isoyearweek")
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
)
dbtable$drop_all_rows()
#> Creating table anon_test
#> Adding index ind1
#> [1] 0
dbtable$insert_data(csdb::nor_covid19_cases_by_time_location)
dbtable$connect()
dbtable$dbconnection$is_connected()
#> [1] TRUE
dbtable$tbl()
#> # Source:   table<`anon_test`> [?? x 18]
#> # Database: sqlite 3.51.2 [/tmp/Rtmp3tTfJa/file3a0c741e4a2129.sqlite]
#>    granularity_time granularity_geo country_iso3 location_code border age  
#>    <chr>            <chr>           <chr>        <chr>          <int> <chr>
#>  1 day              county          nor          county_nor03    2020 total
#>  2 day              county          nor          county_nor03    2020 total
#>  3 day              county          nor          county_nor03    2020 total
#>  4 day              county          nor          county_nor03    2020 total
#>  5 day              county          nor          county_nor03    2020 total
#>  6 day              county          nor          county_nor03    2020 total
#>  7 day              county          nor          county_nor03    2020 total
#>  8 day              county          nor          county_nor03    2020 total
#>  9 day              county          nor          county_nor03    2020 total
#> 10 day              county          nor          county_nor03    2020 total
#> # ℹ more rows
#> # ℹ 12 more variables: sex <chr>, isoyear <int>, isoweek <int>,
#> #   isoyearweek <chr>, season <chr>, seasonweek <dbl>, calyear <int>,
#> #   calmonth <int>, calyearmonth <chr>, date <date>,
#> #   covid19_cases_testdate_n <int>, covid19_cases_testdate_pr100000 <dbl>
```
