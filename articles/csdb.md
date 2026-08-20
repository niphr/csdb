# Introduction to csdb

## What csdb is for

`csdb` puts a large surveillance dataset into a database and takes it
out again, without you writing SQL. You describe a table once: its
columns and their types, its primary key, and its indexes. You then
insert, upsert, delete and read rows through R methods. Before it
writes, it can check that your data matches an agreed column format, so
a table cannot quietly change shape between runs.

No chunk in this vignette needs a database server. Every chunk that
reaches a database uses SQLite, in a file that
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) names.
[`vignette("backends", package = "csdb")`](https://niphr.github.io/csdb/articles/backends.md)
puts a PostgreSQL configuration and a SQLite configuration side by side.

### What this vignette does and does not prove

`R CMD check` re-runs every chunk. A chunk with `error = FALSE` turns
the check red if it raises unexpectedly. A chunk with `error = TRUE`
passes whether it raises or not.

This vignette therefore guards against new unexpected errors. The
expected errors and every printed value are demonstrations, and the test
suites assert them. See `tests/testthat/` in the package source.

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
changes. `csdb` reads no environment variable itself. To keep a password
out of a script, read the variable yourself and pass the result to
`DBTable_v9$new()`.

### Where csdb sits

`csdb` is the storage layer under the csverse surveillance stack. It
imports `csutil`. Its validators are written against `cstidy`’s data
formats, by column name and type. No `csdb` code calls `cstidy` or
`csalert`, and neither is a dependency.

## A worked example

The block below creates a table object, clears it, and inserts the
bundled `nor_covid19_cases_by_time_location` dataset. The file is new,
so `$drop_all_rows()` creates the table and its index first, and then
reports that it removed 0 rows. `$tbl()` returns a lazy dbplyr
reference.

``` r
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%

db_file <- tempfile(fileext = ".sqlite")

dbconnection <- csdb::DBConnection_v9$new(
  driver = "SQLite",
  db = db_file
)
dbconnection
#> (disconnected)
#> 
#> Driver:              SQLite 
#> File:                /tmp/Rtmp3qynQ7/file1f603bf3c4b4.sqlite
dbconnection$connect()
dbconnection$connection
#> <SQLiteConnection>
#>   Path: /tmp/Rtmp3qynQ7/file1f603bf3c4b4.sqlite
#>   Extensions: TRUE
dbconnection$disconnect()
class(dbconnection$connection)
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
    "ind1" = c("granularity_time", "location_code", "date")
  ),
  validator_field_types = csdb::validator_field_types_blank,
  validator_field_contents = csdb::validator_field_contents_blank
)
dbtable$drop_all_rows()
#> Creating table anon_test
#> Adding index ind1
#> [1] 0
dbtable$insert_data(csdb::nor_covid19_cases_by_time_location)
dbtable$tbl()
#> # A query:  ?? x 18
#> # Database: sqlite 3.53.3 [/tmp/Rtmp3qynQ7/file1f603bf3c4b4.sqlite]
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
dbtable$disconnect()
```

## Who owns the connection

`DBTable_v9$new()` takes an optional `dbconnection` argument. That
argument decides who closes the connection.

Pass nothing and the table builds its own `DBConnection_v9`. The table
owns it, and `$disconnect()` closes it. Pass one in and the table
borrows it. The caller keeps ownership, and the borrower’s
`$disconnect()` closes nothing.

Ownership exists because one connection can serve many tables. A caller
that builds many tables against one database then holds one connection,
and not one connection per table.

``` r
# A table that builds its own connection owns it.
owner <- csdb::DBTable_v9$new(
  dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
  table_name = "anon_own",
  field_types = c("x" = "TEXT"),
  keys = "x"
)
owner$connect()
#> Creating table anon_own
owner$dbconnection$is_connected()
#> [1] TRUE

# The owner closes its own connection.
owner$disconnect()
owner$dbconnection$is_connected()
#> [1] FALSE
```

``` r
shared_file <- tempfile(fileext = ".sqlite")
shared <- csdb::DBConnection_v9$new(driver = "SQLite", db = shared_file)

borrower <- csdb::DBTable_v9$new(
  dbconfig = list(driver = "SQLite", db = shared_file),
  table_name = "anon_borrow",
  field_types = c("x" = "TEXT"),
  keys = "x",
  dbconnection = shared
)
borrower$connect()
#> Creating table anon_borrow
shared$is_connected()
#> [1] TRUE

# The borrower does not own the connection, so this closes nothing.
borrower$disconnect()
shared$is_connected()
#> [1] TRUE

# The owner closes it.
shared$disconnect()
shared$is_connected()
#> [1] FALSE
```

## A connection does not survive a fork

A connection belongs to the process that opened it. After a fork, the
parent and the child both hold one socket. An inherited PostgreSQL
connection can then return wrong rows, and report no error. Measured
against the NorSySS server on 2026-08-14. A child asked for `select 4`
and read 3. The parent asked for `select 999` and read 2.

[`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
cannot see this. It reports TRUE on an inherited handle, so a reconnect
that tests validity never fires.

`DBConnection_v9` records the process that opened each connection
instead. It drops any connection whose recorded process is not the
current one. The block below changes that recorded process rather than
forking, because
[`parallel::mcparallel()`](https://rdrr.io/r/parallel/mcparallel.html)
does not exist on Windows.

``` r
db <- csdb::DBConnection_v9$new(driver = "SQLite", db = tempfile(fileext = ".sqlite"))
db$connect()
handle <- db$connection

# What DBI sees, before and after the connection becomes another process's.
DBI::dbIsValid(handle)
#> [1] TRUE
db$.__enclos_env__$private$pconnection_pid <- Sys.getpid() + 1L
DBI::dbIsValid(handle)
#> [1] TRUE

# What csdb sees.
db$is_connected()
#> [1] FALSE
is.null(db$connection)
#> [1] TRUE

# $autoconnection opens a handle for this process.
identical(db$autoconnection, handle)
#> [1] FALSE

# The inherited handle stays open. Closing it would close the other
# process's socket.
DBI::dbIsValid(handle)
#> [1] TRUE
db$disconnect()
```

The example reaches into `$.__enclos_env__$private`, which your own code
MUST NOT do.

A fork writes nothing into that field. The child inherits the recorded
opener process ID unchanged, and it is the child’s own process ID that
differs. The example holds the current process ID fixed and moves the
recorded one, which produces the same mismatch from the other side.

## Two tables that declare one index name

An index name is unique per schema on PostgreSQL, and unique per file on
SQLite. Two tables that both declare `ind1` therefore ask for one name.
`CREATE INDEX IF NOT EXISTS` answers a name that is already taken with a
notice, and not with an error. The first table won the name, and every
later table silently got no index.

The names in `indexes = list("ind1" = ...)` are logical names. Each one
reaches the database under a physical name that carries the table
identity. Two tables can now declare `ind1` and both get an index.

``` r
index_file <- tempfile(fileext = ".sqlite")

new_table <- function(nm) {
  csdb::DBTable_v9$new(
    dbconfig = list(driver = "SQLite", db = index_file),
    table_name = nm,
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = "x",
    indexes = list("ind1" = "n")
  )
}

alpha <- new_table("anon_alpha")
beta <- new_table("anon_beta")
alpha$add_indexes()
#> Creating table anon_alpha
#> Adding index ind1
beta$add_indexes()
#> Creating table anon_beta
#> Adding index ind1

# Two tables, one declared name, two indexes.
DBI::dbGetQuery(
  alpha$dbconnection$autoconnection,
  "SELECT tbl_name, name FROM sqlite_master
    WHERE type = 'index' AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\'
    ORDER BY tbl_name"
)
#>     tbl_name                                name
#> 1 anon_alpha ix_anon_alpha_ind1_d62f09263d205768
#> 2  anon_beta  ix_anon_beta_ind1_8daa70092ed55809
```

`add_indexes()` reads the catalogue after each create. A statement that
returns without an error proves nothing about which table holds the
name. The method raises when the index is absent from this table, and
when it covers columns other than the declared ones. That check is
defined for SQLite and for PostgreSQL, and for no other backend.

``` r
alpha$disconnect()
beta$disconnect()
```

## Emptying a table before you write to it

`$drop_all_rows_and_then_insert_data()` and
`$drop_all_rows_and_then_upsert_data()` empty the table and then write.
Both check `newdata` before they drop a row, and both answer these four
cases the same way.

``` r
# Every table below starts with the same three sentinel rows. A row that
# survives a rejected call proves the guard ran before the drop.
d <- data.table::data.table(x = c("a", "b", "c"), n = 1:3)

seeded_table <- function(nm, validator) {
  tab <- csdb::DBTable_v9$new(
    dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
    table_name = nm,
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = "x",
    validator_field_contents = validator
  )
  tab$insert_data(d)
  tab
}

# Refuses any frame holding a value of n that is not positive. A zero-row
# frame passes it, because all(integer(0) > 0) is TRUE.
positive_only <- function(data) all(data$n > 0)

# Refuses a zero-row frame, and accepts everything else.
nonempty_only <- function(data) nrow(data) > 0

bad <- data.table::data.table(x = c("d", "e"), n = c(5L, -1L))
```

**1. A `NULL` raises, and the table keeps every row.** `insert_data()`
returns early on a `NULL`, so its validator never sees one. Without this
check the table emptied and nothing raised.

``` r
tab_null <- seeded_table("anon_guard_null", positive_only)
#> Creating table anon_guard_null
tab_null$nrow()
#> [1] 3

tab_null$drop_all_rows_and_then_insert_data(NULL)
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata is NULL. drop_all_rows_and_then_insert_data() on table anon_guard_null would empty the table and write nothing back. Pass a zero-row data.frame to empty the table on purpose.
tab_null$nrow()
#> [1] 3
```

**2. A zero-row frame that the validator refuses raises, and the table
keeps every row.**

``` r
tab_zero_bad <- seeded_table("anon_guard_zero_bad", nonempty_only)
#> Creating table anon_guard_zero_bad
tab_zero_bad$nrow()
#> [1] 3

tab_zero_bad$drop_all_rows_and_then_insert_data(d[0])
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata failed validator_field_contents. drop_all_rows_and_then_insert_data() on table anon_guard_zero_bad rejected it before dropping any row. Field: not named by the validator.
tab_zero_bad$nrow()
#> [1] 3
```

**3. A zero-row frame that the validator accepts empties the table, and
raises nothing.** That is how `cs9::DBPartitionedTableExtended_v9`
clears a partition.

``` r
tab_zero_ok <- seeded_table("anon_guard_zero_ok", positive_only)
#> Creating table anon_guard_zero_ok
tab_zero_ok$nrow()
#> [1] 3

tab_zero_ok$drop_all_rows_and_then_insert_data(d[0])
tab_zero_ok$nrow()
#> [1] 0
```

**4. A frame with rows reaches the validator before the drop.** A
refused frame raises, and every sentinel row is still there afterwards.

``` r
tab_rows <- seeded_table("anon_guard_rows", positive_only)
#> Creating table anon_guard_rows
tab_rows$nrow()
#> [1] 3

tab_rows$drop_all_rows_and_then_insert_data(bad)
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata failed validator_field_contents. drop_all_rows_and_then_insert_data() on table anon_guard_rows rejected it before dropping any row. Field: not named by the validator.
tab_rows$tbl() |> dplyr::collect()
#> # A tibble: 3 × 2
#>   x         n
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3
```

An accepted frame empties the table and takes the write.

``` r
tab_rows$drop_all_rows_and_then_insert_data(d[1:2])
tab_rows$nrow()
#> [1] 2
```

### The upsert method answers the same four cases

`$drop_all_rows_and_then_upsert_data()` runs the same guard, in the same
four cases and the same order.

``` r
# 1. A NULL raises.
tab_up <- seeded_table("anon_guard_upsert", positive_only)
#> Creating table anon_guard_upsert
tab_up$drop_all_rows_and_then_upsert_data(NULL)
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata is NULL. drop_all_rows_and_then_upsert_data() on table anon_guard_upsert would empty the table and write nothing back. Pass a zero-row data.frame to empty the table on purpose.
tab_up$nrow()
#> [1] 3
```

``` r
# 2. A zero-row frame the validator refuses raises, and every sentinel row
#    stays. This is the case that tells "returns early on zero rows" apart
#    from "validates zero rows too".
tab_up_zero <- seeded_table("anon_guard_upsert_zero", nonempty_only)
#> Creating table anon_guard_upsert_zero
tab_up_zero$drop_all_rows_and_then_upsert_data(d[0])
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata failed validator_field_contents. drop_all_rows_and_then_upsert_data() on table anon_guard_upsert_zero rejected it before dropping any row. Field: not named by the validator.
tab_up_zero$tbl() |> dplyr::collect()
#> # A tibble: 3 × 2
#>   x         n
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3
```

``` r
# 3. A zero-row frame the validator accepts empties the table.
tab_up$drop_all_rows_and_then_upsert_data(d[0])
tab_up$nrow()
#> [1] 0
```

``` r
# 4. A frame with rows reaches the validator before the drop. A refused frame
#    leaves every sentinel row in place.
tab_up_rows <- seeded_table("anon_guard_upsert_rows", positive_only)
#> Creating table anon_guard_upsert_rows
tab_up_rows$drop_all_rows_and_then_upsert_data(bad)
#> Error in `private$check_newdata_before_drop_all_rows()`:
#> ! newdata failed validator_field_contents. drop_all_rows_and_then_upsert_data() on table anon_guard_upsert_rows rejected it before dropping any row. Field: not named by the validator.
tab_up_rows$tbl() |> dplyr::collect()
#> # A tibble: 3 × 2
#>   x         n
#>   <chr> <int>
#> 1 a         1
#> 2 b         2
#> 3 c         3
```

``` r
# An accepted frame empties the table and takes the write.
tab_up_rows$drop_all_rows_and_then_upsert_data(d[1:2])
tab_up_rows$nrow()
#> [1] 2
```

The guard covers what `csdb` can see before the drop. It is not a
transaction. A write that fails after the truncation still leaves the
table empty.
