# Version 2026.8.6

## Documentation
* Repository prose now follows ASD-STE100 (Simplified Technical English). The sweep covered the roxygen2 blocks in `R/`, both vignettes, and `README.md`. It changed no claim and no executable code. `index.md` needed no change.
* No roxygen sentence runs over 25 words. Counted per authored unit, which is one `@description`, `@param`, `@return`, `@seealso`, paragraph or Rd `\item`, the count fell from 6 to 0. The longest sentence fell from 36 words to 24.
* Roxygen fields and `\itemize` items now end in a full stop. Without one, a sentence splitter runs straight through the field boundary and reports a merge as one sentence. That is where the 74-word to 97-word readings came from, in blocks whose longest authored sentence was under 25 words.
* `vignettes/csdb.Rmd`, `vignettes/backends.Rmd` and `README.md` are also at zero sentences over 25 words. The counts before were 3, 1 and 2.
* `vignettes/csdb.Rmd` and `vignettes/csdb.Rmd.orig` carry identical prose edits, so the generated file and its source stay in sync. `vignettes/_PRECOMPILER.R` was not re-run, and no chunk output changed.
* The v3 statement in the introduction vignette keeps its size. `csdb` CAN store a `csfmt_rts_data_v3`, with the `_blank` pair or with a validator of your own. What is missing is a validator that knows the v3 shape.
* Ornamental adjectives are gone from the reference pages: "robust" from `DBConnection_v9`, "sophisticated" and "comprehensive" from `DBTable_v9`, and "comprehensive" from `get_table_names_and_info()`. The `DBTable_v9` title is now "R6 Class representing a database table", parallel with `DBConnection_v9`.

* The introduction vignette opens with prose instead of with code output. pkgdown promotes `vignettes/csdb.Rmd` to "Get started", and the first thing on that page was the `data.table` attach message: `Attaching package: 'data.table'` and the `%notin%` masking line. A new "What csdb is for" section now comes first. It says what the package does, splits `DBConnection_v9` (the connection) from `DBTable_v9` (one table), tabulates the three backends, names the missing v3 validator, and says where `csdb` sits in the stack. Its two chunks run without a database server.
* The `library(data.table)` and `library(magrittr)` chunk is now `message = FALSE`. No chunk in the vignette uses `%>%` or bare `data.table` syntax, so the two attach messages announced masking that nothing below them relied on. The `library()` calls themselves are unchanged.
* The overview states a current limitation plainly. `csdb` exports field-type and field-contents validators for `csfmt_rts_data_v1` and `csfmt_rts_data_v2` and none for `csfmt_rts_data_v3`; `grep("v3", getNamespaceExports("csdb"))` returns `character(0)`. That matters now rather than later, because `cstidy` marks v1 and v2 deprecated in favour of `set_csfmt_rts_data_v3()`, and `csalert`'s pipeline ends in `ens_collapse(heal = TRUE)`, which returns a `csfmt_rts_data_v3`. A v3 result can still be written, with the blank validators or with a function of the user's own, but nothing then checks its columns.
* The overview shows `is_connected()` returning `FALSE` immediately after `DBConnection_v9$new()` and again after `DBTable_v9$new()`. The second call uses a PostgreSQL configuration naming a server that is not running, which is the strongest form of the claim: neither constructor opens a connection.
* `vignettes/csdb.Rmd` was regenerated from `vignettes/csdb.Rmd.orig` by `vignettes/_PRECOMPILER.R`. Apart from the new section and the two suppressed attach messages, the only change in it is the `tempfile()` path, which differs on every run.

## Bug Fixes
* `DBTable_v9$connect()` was documented as "Connect from the database". It connects to the database, which is what `DBConnection_v9$connect()` already said.
* `DBTable_v9$drop_indexes()` was documented as "Drops all indees from the database table". The word is "indexes".
* `DBTable_v9$insert_data()` carried a stray prose line after `@param verbose`, so roxygen2 rendered the `verbose` argument as "Boolean. Inserts data into the database table". That sentence is now the method's `@description`, and `verbose` reads "Boolean."

## Development
* `man/` was regenerated with roxygen2 8.0.0, the version `DESCRIPTION` declares. `NAMESPACE` is unchanged.
* Release notes for 2026.8.5 and earlier are left as they shipped. A changelog is a record, and rewording a released entry changes that record.

# Version 2026.8.5

## New Features
* SQLite is a third backend. `driver = "SQLite"` with `db` set to a file path connects through `RSQLite`, which is now in `Imports`. The driver string is matched case-insensitively, so `sqlite`, `SQLite` and `SQLITE` all select it; the two ODBC driver strings keep exact matching, because they must equal an `odbcinst.ini` entry.
* `DBConnection_v9` creates the parent directory of `db` if it does not exist, then opens the file with `extended_types = TRUE`. That argument is required, not cosmetic: without it a `DATE` column reads back as the integer `18262` rather than a `Date`, and `validator_field_contents_csfmt_rts_data_v1()` rejects it. No `USE <db>;` is issued, because the file is already the database.
* `DBConnection_v9$print()` shows the driver and the file path for SQLite, and omits server, port, user, password, SSL mode and trusted connection, none of which SQLite reads.
* `DBTable_v9` identifiers under SQLite are the bare table name: `DBI::Id(table = <table_name>)` and the plain string. `schema` is ignored entirely, because SQLite has no schemas.
* `create_table()` on SQLite inlines the primary key in the `CREATE TABLE` statement and marks every key column `NOT NULL`. `add_constraint()` is therefore a no-op there. SQLite has no `ALTER TABLE ... ADD CONSTRAINT ... PRIMARY KEY`; the statement the other backends use is a syntax error.
* The SQLite field-type map is closed: `TEXT`, `INTEGER`, `DOUBLE`, `BOOLEAN`, `DATE` and `DATETIME` are accepted and anything else is an error naming the column and the type. SQLite accepts any declared type name, so `VARCHAR(100)`, `TEXT(100)` or a misspelling would otherwise create a table with an unintended affinity and no warning.

* `insert_data()` on SQLite writes through `DBI::dbAppendTable()`. There is no staging CSV and no external client binary: SQLite is a file, and `dbAppendTable()` writes 100,000 rows in about 0.02 seconds. The `file` argument is accepted and ignored.
* `insert_data()` on SQLite copies its argument before writing, so the caller's `data.table` is not modified by reference. The three other backends reach `write_data_infile()`, which has always modified it in place.
* `upsert_data()` on SQLite stages the rows in a temporary table and then issues `INSERT ... ON CONFLICT (<keys>) DO UPDATE SET`, falling back to `DO NOTHING` when every field is a key. SQLite has neither `MERGE` nor `ON DUPLICATE KEY UPDATE`. Three preconditions are checked before any SQL is emitted, because each fails late and obscurely otherwise: `keys` must be non-empty, or the statement is `ON CONFLICT ()`; every key must be one of the fields; and `fields` must be exactly the table's live columns, because `CREATE TABLE ... AS SELECT` discards defaults and a partial field list would insert NULL into every omitted column.
* `drop_all_rows()` is now an S7 generic. SQL Server and PostgreSQL keep the `TRUNCATE TABLE` statement they always received, unchanged; SQLite gets `DELETE FROM <table>`, because `TRUNCATE TABLE` is a syntax error there. `DELETE` leaves the primary key and every index intact, which matters because the SQLite `add_constraint()` cannot put a dropped primary key back.
* `keep_rows_where()` on SQLite emits `DELETE FROM <table> WHERE (<condition>) IS NOT TRUE`, not `NOT (<condition>)`. The two are not the same statement: `DELETE` removes only rows whose predicate evaluates to TRUE, and the negation of NULL is NULL, so a plain negation silently retains every row on which the condition is NULL, although `SELECT ... WHERE <condition>` would not have kept it. `IS NOT TRUE` folds NULL into FALSE and gives the exact complement. It is also a `DELETE` rather than the drop-and-rename the other two backends use, for the same primary-key reason as `drop_all_rows()`.
* `drop_rows_where()` on SQLite emits `DELETE FROM <table> WHERE <condition>`.

* `add_indexes()` on SQLite emits `CREATE INDEX IF NOT EXISTS <index> ON <table> (<keys>)`, with the table name unqualified. SQLite lets the index name carry a schema but never the table: `CREATE INDEX ind ON main.tab (a)` is `near ".": syntax error`. `drop_indexes()` emits `DROP INDEX IF EXISTS <index>`, which names the index alone, because a SQLite index belongs to the schema rather than to the table.
* `confirm_indexes()` on SQLite now executes no DDL when the indexes already match. `get_indexes()` excludes SQLite's own index names and orders by `rowid`. Both are required: a `PRIMARY KEY` auto-creates `sqlite_autoindex_<table>_1`, which is a row in `sqlite_master` exactly like a user index, and `rowid` order is creation order, which is the order `add_indexes()` works in. `confirm_indexes()` compares with `identical()`, so an extra name or a different order would drop and re-add every index on every call. The return value is a plain character vector for the same reason.
* `get_table_names_and_info()` has a method for `SQLiteConnection`. `nrow` is `COUNT(*)`, which is exact, unlike the `reltuples` estimate PostgreSQL reports and the `sp_spaceused` figure SQL Server reports. All three size columns are `NA_real_`: the `dbstat` virtual table is not compiled into the SQLite that `RSQLite` ships, so there is no per-table size to report, and `pragma page_count` describes the whole file. An empty database returns a zero-row table that still has all five columns.
* Both SQLite catalogue filters write the exclusion as `name NOT LIKE 'sqlite\_%' ESCAPE '\'`, escaping the underscore. `_` is a single-character wildcard in SQL `LIKE`, so the unescaped `'sqlite_%'` hides every name beginning "sqlite" followed by any character at all, not only SQLite's own objects. A user index named `sqliteIdx` would never be found by `get_indexes()`, and `confirm_indexes()` would drop and re-add it on every call; a user table named `sqliteFoo` would be missing from `get_table_names_and_info()`, so `DBTable_v9$nrow(use_count = FALSE)` and `DBTable_v9$info()` would report nothing for it.

## Known limitations
* `confirm_indexes()` compares index *names* only. An index with the right name and the wrong columns passes. This is the existing behaviour of all three backends and SQLite matches it.

## Documentation
* The introduction vignette now runs on SQLite, in a file created by `tempfile()`. It is precompiled from `vignettes/csdb.Rmd.orig`, and that precompilation used to need a live PostgreSQL database. `knitr::knit()` defaults to `error = TRUE`, so on a machine without one it did not fail: it exited 0 and wrote seven `#> Error` transcripts into the committed `vignettes/csdb.Rmd`, including a `Could not connect to database server ''`. Anyone can now rebuild the vignette and get the same output.
* Added `vignettes/backends.Rmd`, which puts a PostgreSQL `dbconfig` and a SQLite `dbconfig` side by side, runs one `DBTable_v9$new()` definition against each, and tabulates what a user must know: `schema` is ignored, the primary key is inlined at `CREATE TABLE` and cannot be added later, an unrecognised field type is rejected rather than passed through, `get_table_names_and_info()` reports an exact `COUNT(*)` and `NA` sizes, and no external client binary is needed. No chunk in it executes.
* `README.md`'s quick start is now the SQLite one, so it runs on a bare machine, and it links to both vignettes. The `$keep_rows_where()` caution is qualified: the copy, drop and rename it describes is the ODBC path, not the SQLite one.
* `index.md` and the `_pkgdown.yml` hero lede both name SQLite alongside PostgreSQL and SQL Server.

## Development
* Added `tests/testthat/test-sqlite-connection.R`, the first tests in the package that open a database connection. SQLite is a file, so they need no server.
* Added `tests/testthat/test-sqlite-indexes.R`. The block that proves `confirm_indexes()` emits no DDL reads `PRAGMA schema_version` before and after, not the index names: the names are identical whether the call did nothing or dropped and recreated every index, and `schema_version` increments on every schema change. A separate block creates an index named `sqliteBar` and a table named `sqliteFoo` and asserts both are visible, which is what pins the `ESCAPE` clause on the two catalogue filters.
* Added `tests/testthat/test-sqlite-data.R`, covering the five write and delete paths: type round-trip, the non-finite scrub, the caller's data.table being left alone, upsert update-not-duplicate, the three upsert preconditions, the NULL-condition row, `drop_all_rows()` leaving the indexes, and identifiers that need quoting.
* The `Inf`/`NaN` to `NA` loop moved out of `write_data_infile()` into an internal `scrub_non_finite()`, called from there and from the SQLite write path. `Inf` survives `DBI::dbAppendTable()` and reads back as `Inf`, so without it SQLite would silently disagree with the two backends that write `NA`. The `POSIXt` to character conversion is not shared: SQLite needs a `POSIXct` to stay one, so that `extended_types = TRUE` round-trips it through a `DATETIME` column.
* `dbplyr` is in `Imports`. It always was a hard requirement and was never declared: `DBTable_v9$tbl()` calls `dplyr::tbl()` on a DBI connection, which dispatches to `dplyr:::tbl.DBIConnection()` and stops in `check_dbplyr()` when dbplyr is absent. Three documented methods go through it, `tbl()`, `print_dplyr_select()` and `nrow(use_count = TRUE)`, and `tbl()` is the only read path the package offers, so a csdb without dbplyr is write-only. The gap never surfaced because nothing in csdb called `tbl()` until the SQLite tests did; on a library without dbplyr those seven blocks error and the other 108 assertions pass. `Suggests` was rejected on measurement: dbplyr adds three packages to an `Imports` closure of 42, and the alternative is to make the package's only read path optional. No csdb code names dbplyr, so `fix_dbplyr()` in `R/xxx_small_import_fix.R` holds a `dbplyr::` reference for the same reason `fix_r6()` and `fix_s7()` hold theirs: without it `R CMD check` reports "All declared Imports should be used".
* `RSQLite` is in `Imports` and has no S3 fallback in `get_db_classes()`, which stops with a message naming RSQLite if the real S4 `SQLiteConnection` class is absent. A `S7::new_S3_class()` fallback would be worse than useless: with the real S4 `DBIConnection` default present, methods registered against the fallback lose dispatch silently and run the MySQL-flavoured `db_default` SQL, and registering the real class later does not retarget them.
* Documentation is generated by roxygen2 8.0.0. `DESCRIPTION` now declares `Config/roxygen2/version` in place of `RoxygenNote`, and every `.Rd` file was regenerated by that version. `NAMESPACE` is unchanged.

# Version 2026.8.4

## Documentation
* `README.md` now carries what the package is, installation, one quick start, and a table that routes a task to the function that does it. It also states two things the API does not do: `create_table()` drops and rebuilds a table whose columns differ from `names(field_types)`, and no method opens a transaction.
* All 11 exported functions gained a `@seealso` that says whether the introduction vignette demonstrates them. Four appear in a vignette code chunk (`DBConnection_v9`, `DBTable_v9`, `validator_field_types_blank`, `validator_field_contents_blank`); the other seven appear nowhere in the vignette, and their `@seealso` says so.
* Added three `@family` groups: auth hook functions (both address the `csdb.auth_hook` option, one writing it and one reading it), field type validators (one `db_field_types` argument, checked once inside `DBTable_v9$new()`), and field contents validators (one `data` argument, called from `insert_data()` and `upsert_data()`). `DBConnection_v9` and `DBTable_v9` are grouped as database classes: `DBTable_v9$new()` takes a `dbconfig` list of exactly the 10 arguments `DBConnection_v9$new()` accepts, and builds one.

## Bug Fixes
* `get_table_names_and_info()`: the documented PostgreSQL example connected through `RPostgres::Postgres()`. Those connections are of class `PqConnection`, and the generic has methods for `PostgreSQL` and `Microsoft SQL Server` only, so that example cannot dispatch; it errors with "no applicable method". It now connects through the `PostgreSQL Unicode` ODBC driver, which is the class the methods are written for. `RPostgres` was also absent from `Imports` and `Suggests`.
* `get_table_names_and_info()`: the `nrow` column was documented as the number of rows. It is `reltuples` from `pg_class` on PostgreSQL, which is an estimate, and the `rows` column of `sp_spaceused` on Microsoft SQL Server. Documented as reported, not as exact.
* `DBConnection_v9`: the documented PostgreSQL example used `driver = "PostgreSQL"`. Only `"PostgreSQL Unicode"` selects a PostgreSQL branch in the connection code, so `"PostgreSQL"` falls through to the generic branch, which does not pass `database`, and is then followed by `USE <db>;`. Changed to `"PostgreSQL Unicode"`.
* `validator_field_types_csfmt_rts_data_v2()`: the example vector labelled "Valid field types" returned `FALSE`, because it omitted `isoquarter` and `isoyearquarter`, which the v2 schema holds at positions 11 and 12. The example now returns `TRUE`, and a second call shows the v1 layout returning `FALSE`.
* `DBTable_v9`: the documented example called `$add_indexes(c("name", "date_created"))`, but that method takes no arguments and reads `self$indexes`. Indexes are now declared in the constructor. The same example passed `data.frame`s to `$insert_data()` and `$upsert_data()`, both of which reach `data.table` syntax (`[ , (col) := ]`, `with = FALSE`) and require a `data.table`. Changed to `data.table::data.table()`.

## Development
* `csdb_set_auth_hook()`, `DBConnection_v9` and `DBTable_v9` gained runnable examples for the parts that need no database server: setting and restoring the hook, and creating an object without connecting. Their `\dontrun{}` blocks keep the parts that need a server.
* Added `^Rplots\.pdf$` to `.Rbuildignore`.

# Version 2026.5.13

## Bug Fixes
* `DBTable_v9$nrow(use_count = TRUE)` now calls `dplyr::n()` instead of a bare `n()`. This is hygiene only: the bare call sits inside the list passed to `R6::R6Class()`, which `codetools` never walks, so it produced no `R CMD check` complaint, and `dbplyr` renders both spellings to identical SQL.
* PostgreSQL methods (`create_table`, `keep_rows_where`, `drop_table`) now quote `role_create_table` via `DBI::dbQuoteIdentifier()` when emitting `SET ROLE`. Previously the role name was interpolated raw, which broke on identifiers containing hyphens, mixed case, or reserved words (e.g. `SET ROLE token-user` -> syntax error), and was a SQL-injection vector if the value came from an env var.

# Version 2026.2.2

## New Features
* Added authentication hook system (`csdb_set_auth_hook()`, `csdb_get_auth_hook()`) to allow automatic credential refresh (e.g., Kerberos tickets) when connection fails

# Version 2026.1.28

## Bug Fixes
* Improved database connection error messages to include the original driver error details for easier debugging

# Version 2025.7.28

## Bug Fixes
* Fixed namespace loading error by importing methods::initialize generic
* Improved error handling in .onLoad() function for S7 operations
* Package now passes CRAN namespace loading requirements

# Version 2025.7.19

## Bug Fixes
* Fixed DBConnection_v9 print method to display "SSL mode" instead of "trusted connection" for PostgreSQL connections

## Development
* Added S7 package to imports in preparation for S3 to S7 method conversion to improve CRAN compliance
* S7 transition planned to reduce exported API surface while maintaining functionality through R6 classes

# Version 2025.7.17

- Updated package for CRAN submission with comprehensive improvements
- Added comprehensive documentation with examples for all exported functions
- Fixed critical CRAN compliance issues including system tool availability checks
- Added proper R6 class documentation with detailed usage examples
- Improved all validator function documentation with clear examples
- Added missing dependencies and fixed import declarations
- Updated .Rbuildignore to exclude system files and build artifacts
- Added CLAUDE.md for future development guidance
- Fixed vignette title and improved documentation quality
- All functions now pass R CMD check with only acceptable NOTEs

# Version 2025.2.15

- Including `role_create_table` in drop_table for PostgreSQL.

# Version 2024.10.25

- `role_create_table` is now included for dbconnection_v9/dbtable_v9, so that the role can be changed when creating tables in PostgreSQL.

# Version 2024.3.27

- csdb now supports PostgreSQL databases as well as MS SQL Server.

# Version 2024.3.11

- Including use_count as an argument in nrow in DBTable_v9, which is slower but more accurate.

# Version 2024.3.7

- Including confirm_insert_via_nrow in DBTable_v9. Checks nrow() before insert and after insert. If nrow() has not increased sufficiently, then attempt an upsert.


# Version 2023.12.28

- Including validator_field_types_csfmt_rts_data_v2 and validator_field_contents_csfmt_rts_data_v2.

# Version 2023.12.26

- Including georegion in validator_field_contents_csfmt_rts_data_v1.

# Version 2023.4.14

- `get_table_names_and_info` is now ordered according to `table_name`.

# Version 2023.4.12

- `get_table_names_and_nrow` is now changed to `get_table_names_and_info` and also includes size_total_gb, size_data_gb, size_index_gb.
- `info` is now included as a method for `DBTable_v9` 

# Version 2023.4.4

- `confirm_indexes` is now added to `DBTable_v9`, which confirms that the names and number of indexes in the database are the same as in the R code. It does not confirm the contents of the indexes!
- `nrow` is now added to `DBTable_v9`, which is an application of the new `get_table_names_and_nrow` function.
- `get_table_names_and_nrow` added as an exported function, that will get all the table names and the nrows from a dbconnection.

# Version 2023.4.2

- `create_table` now automatically adds the indexes.

# Version 2023.3.31

- Removing info messages from `drop_rows_where`.

# Version 2023.3.8

- connect() in DBConnection_v9 is smarter, more robust with error checking and making fewer useless calls to the db. Tries to connect twice now before throwing an error.
- autoconnection is now more robust in DBConnection_v9.

# Version 2023.2.17

- Package is created.
