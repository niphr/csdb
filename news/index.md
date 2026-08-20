# Changelog

## Version 2026.8.20

### Development

- `R/util_database.R` held 1445 code lines, and the shared CI workflow
  fails any `R/*.R` file over 1000. The S7 method assignments now live
  in four sibling files, one per group of generics:
  `util_database_load.R`, `util_database_table.R`,
  `util_database_index.R` and `util_database_rows.R`.
- The split moved whole top-level expressions and changed none of them.
  R sources `R/` in C collation order, and each new name sorts after
  `util_database.R`. Every generic and every class therefore exists
  before the method assignments run.

## Version 2026.8.16

### Bug Fixes

- `add_index()` raises when it cannot create the index. The `db_default`
  and `db_postgres` methods wrapped the work in `try(..., TRUE)` and
  returned a `try-error` object. No caller reads that object, so a
  failed index looked exactly like a created one.
- The PostgreSQL upsert asked for an index named
  `"ind" + random_uuid()`. `+` does not join strings in R, so the
  expression raises `non-numeric argument to binary operator`. R
  evaluated it inside the `glue()` inside the
  [`try()`](https://rdrr.io/r/base/try.html), so every PostgreSQL upsert
  built its temporary table with no index. The name now comes from
  [`paste0()`](https://rdrr.io/r/base/paste.html).
- The same call passed the temporary table as a
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html), and
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  cannot coerce one. `add_index()` for PostgreSQL quotes the table
  itself now, so the call passes `temp_name`, the
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html). Verified against
  PostgreSQL 16.14: the call creates an index on a temporary table that
  a [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) names.
- The PostgreSQL index DDL quotes every identifier. `add_index()` pasted
  the table and the key columns into `CREATE INDEX` in raw, and
  `drop_index()` pasted the schema and the index name into `DROP INDEX`.
  A name that holds a dot, a space or an upper case letter then produced
  SQL that PostgreSQL rejected, or that named a different object.
  `anon.MyTab` folded to `anon.mytab`, and `anon.my tab` was a syntax
  error. `add_index()` for PostgreSQL takes the table as a
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) now, so
  `DBTable_v9$add_indexes()` passes the
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) too. Verified
  against PostgreSQL 16.14 on `norsyss_data1`, schema `anon`, for three
  table names: `zz_quote_probe.dot`, `zz_quote_probeUPPER` and
  `zz_quote_probe space`. Each declared one index over the columns `a`
  and `C Odd.x`. `add_indexes()` created it, `pg_indexes` held it on
  that table with those two columns in that order, and `drop_indexes()`
  removed it. The probe left 0 tables and 0 indexes behind.
- `DBTable_v9$add_indexes()` creates each declared index exactly once.
  The method reaches `create_table()` through
  `lazy_creation_of_table()`, and `create_table()` calls `add_indexes()`
  again. Two declared indexes produced four attempts.
  `CREATE INDEX IF NOT EXISTS` and the swallowed error hid the
  duplicate.
- Each declared index now reaches the database under a name built from
  the table identity and the logical name. csdb used the caller’s
  logical name in the database, verbatim. A PostgreSQL index name is
  unique per SCHEMA, so every table in one schema that declared `ind1`
  asked for one name. `CREATE INDEX IF NOT EXISTS` answers a taken name
  with a notice and not an error. The first table won the name, and
  every later table silently got nothing. Measured on the
  `norsyss_data1` database on 2026-08-15: the table `anon_norsyss_data`
  had 87 partitions in schema `anon`, and all 87 declared `ind1` and
  `ind2`. One partition held `ind2`, and none held `ind1`.
- `add_indexes()` reads the catalogue after each create. A statement
  that returns without an error proves nothing about which table holds
  the name, and nothing about which columns the index covers. The method
  raises when the index is not on this table, and when it covers columns
  other than the declared ones. That check is defined for SQLite and for
  PostgreSQL only. See the Development note below for what any other
  backend gets.
- `drop_index()` for PostgreSQL names the schema. It emitted
  `DROP INDEX IF EXISTS {index}`, which PostgreSQL resolves through
  `search_path`. csdb creates every index on a fully specified table, so
  the index lands in that table’s schema. The drop found it only when
  that schema was on the path. The
  [`try()`](https://rdrr.io/r/base/try.html) around the call hid the
  miss. The method took a `table` argument and ignored it; it reads the
  schema from it now.
- `DBTable_v9$drop_indexes()`, and the catalogue check inside
  `$add_indexes()`, read the table from the
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) field and no
  longer from the text field beside it. Text splits on every dot, so a
  table called `an.on.tab` read as the three components `an`, `on` and
  `tab`. `$add_indexes()` created the index and then reported that the
  index was on no table at all. `drop_index()` for PostgreSQL read the
  schema as `on`. Measured on the `norsyss_data1` database on
  2026-08-15: 0 tables and 0 schemas hold a dot, so this could not fire
  there.
- `confirm_indexes()` no longer drops every index to reconcile. It
  compared the names in the database against `names(self$indexes)` with
  [`identical()`](https://rdrr.io/r/base/identical.html), and dropped
  and re-added everything on any mismatch. Those two lists differ by
  design now, so that comparison could never match again. The method
  takes one of four actions per declared index. It does nothing when the
  index is present and correct. It adds the index when the index is
  absent. It raises when the managed name covers other columns. It
  ignores any index that csdb did not name.
- The default upsert method maps the logical names in `drop_indexes` to
  the names the database holds. It builds its temporary table with
  `CREATE TEMPORARY TABLE ... LIKE`, which copies the source table’s
  index names, and then asked to drop the logical name. A rename that
  reaches creation but not dropping leaves an index that nobody can
  remove.
- `DBTable_v9$drop_all_rows_and_then_upsert_data()` and
  `DBTable_v9$drop_all_rows_and_then_insert_data()` reject four kinds of
  `newdata` before they drop a row. They are a `NULL`, an object that is
  not a `data.frame`, a row count that is unusable or unstable, and data
  that `validator_field_contents` refuses. Both methods dropped every
  row and then called the write method. The validator runs inside that
  write method. Invalid data therefore emptied the table before anything
  rejected it. Measured on 2026-08-15: a table holding 3 rows raised
  `upsert_load_data_infile not validated` and held 0 rows after.
- Both methods raise on a `NULL`, and on anything that is not a
  `data.frame`. `upsert_data()` and `insert_data()` return early on a
  `NULL` and on a zero-row frame. Both return before they reach the
  validator. A `NULL` is therefore not invalid data: the validator never
  sees it. Measured on 2026-08-15: 2 rows before the call, 0 rows after
  it, and no error at all. A validator that runs earlier does not close
  that gap, so the check sits in the two destructive methods.
- A zero-row `data.frame` that fails the validator raises, and the table
  keeps every row. Before this release the row count check returned
  first, so nothing validated that frame and nothing raised.
- A zero-row `data.frame` that passes the validator still empties the
  table, and raises nothing. `cs9::DBPartitionedTableExtended_v9` clears
  every partition this way.
- Both methods read the row count before the drop, and reuse that value
  afterwards. [`nrow()`](https://rdrr.io/r/base/nrow.html) reads
  [`dim()`](https://rdrr.io/r/base/dim.html), and a `data.frame`
  subclass can carry a [`dim()`](https://rdrr.io/r/base/dim.html) method
  that returns `NA`, `Inf`, or a different answer on each call.
  [`is.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is TRUE
  on such an object, and a permissive validator accepts it. The guard
  reads [`nrow()`](https://rdrr.io/r/base/nrow.html) twice. It rejects a
  count that is not one finite non-negative whole number, and it rejects
  two reads that disagree. A row count read after the drop raised on
  `if (n == 0)` with the table already empty.

### Development

- `tests/testthat/test-index-integrity.R` covers the fixes above. The
  csdb suite held 295 passes after that work, up from 206.

- `tests/testthat/test-destructive-order.R` covers nine kinds of
  `newdata`, for both destructive methods. That work took the csdb suite
  to 427 passes, up from 295.

- The csdb suite holds 464 passes at release, with 0 failures, 0 errors
  and 0 skips. The 37 after 427 cover the identifier quoting and the
  vignette. Measured on 2026-08-15 by
  [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
  under
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html).

- The cs9 suite holds 368 passes at release, up from 354. This csdb
  release does not change that number. `cs9 26.8.17` adds
  `tests/testthat/test-fork-ordering.R`, and that is what moves it.

- This release does NOT wrap the drop and the write in one transaction.
  A write that fails after the truncation still leaves the table empty.
  The guard removes the reasons csdb itself can see before the drop.

- A `data.frame` subclass whose
  [`dim()`](https://rdrr.io/r/base/dim.html) answers differently on a
  later call can still empty the table and then raise. `upsert_data()`
  and `insert_data()` read `newdata` again after the drop. Closing that
  needs a copy of `newdata` or a transaction, and this release does
  neither. The two reads in the guard detect a count that changes
  between them, and they do not prove that a later read agrees.

- `validator_field_contents` runs TWICE on a destructive write that
  carries rows: once in the guard before the truncation, and once inside
  `upsert_data()` or `insert_data()` after it. Measured on 2026-08-15: 2
  calls for a frame with rows, against 1 call for a plain
  `insert_data()`. A zero-row frame still costs 1 call, because the
  guard returns before the write method runs. A stateful or
  nondeterministic validator can therefore pass the first call and fail
  the second, which leaves the table empty. Measured on 2026-08-15: a
  validator that answered TRUE then FALSE took a table from 2 rows to 0.
  An expensive validator pays for both calls.

- The check accepts only `TRUE` from `validator_field_contents`, through
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html). `upsert_data()` and
  `insert_data()` keep their `if (!validated)` test, which also accepts
  `TRUE` carrying attributes. The destructive path is therefore the
  stricter of the two, and it is strict before the drop rather than
  after it.

- The `db_mssql` method keeps its `try(..., TRUE)`. Its SQL reads
  `CREATE INDEX {index} IF NOT EXISTS ON {table}`, which SQL Server does
  not accept. Its only caller passes no index name. Removing the wrapper
  there needs a SQL Server to verify against, so that work waits.

- The name in the database has three parts. They are the prefix `ix_`, a
  readable slug, and a digest. The digest is 16 hexadecimal characters
  of a version 5 UUID over the table identity and the logical name. The
  slug carries no character outside `[a-z0-9_]`. csdb cuts the slug from
  the left when the whole name would pass 63 characters, the PostgreSQL
  identifier limit. PostgreSQL truncates a longer name and reports
  nothing, and a truncated name plus `IF NOT EXISTS` is the same silent
  no-op again. Call `csdb:::index_physical_name()` to read the name for
  one table and one logical name.

- The name is collision-resistant, and it is not injective. The digest
  holds 64 bits, and the version nibble of a version 5 UUID is fixed, so
  60 bits vary. Two different tables give one name when those 60 bits
  agree. The key construction removes every STRUCTURAL collision. It
  length-prefixes the component count, then every component of the table
  name, then the logical name. No two different inputs therefore build
  one key. The `PK_{table}` rule in `add_constraint()` has structural
  collisions, because it deletes `.`, `[` and `]`. Schema `a` with table
  `bc` and schema `ab` with table `c` both give `PK_abc`.

- The table identity is the ordered name components, and never a joined
  string. `DBI::Id(schema = "a", table = "b.c")` and
  `DBI::Id(schema = "a.b", table = "c")` both join to `a.b.c`. A joined
  identity therefore gives two different tables one index name. csdb
  passes the [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) at
  every site that reads a table as an identity, so every site computes
  one name for every table name. A schema or a table name that holds a
  dot is covered: a test creates, verifies and drops an index on a table
  called `an.on.tab`. Text is still accepted for a caller that holds
  only the text form. `"anon.tab"` still names the same index as
  `DBI::Id(schema = "anon", table = "tab")`.

- Text names exactly the identity of its dot-separated pieces, and that
  is a definition rather than a guess. No mapper rule can refuse an
  ambiguous text, because every dotted text is ambiguous: `"anon.tab"`
  is equally the text form of `DBI::Id(table = "anon.tab")`. A caller
  that holds a [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) MUST
  pass the [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html), not its
  joined text.

- The name is lowercase because PostgreSQL folds an unquoted identifier
  to lowercase and SQLite does not. A lowercase name therefore reads the
  same in the source and in both catalogues. Measured on `norsyss_data1`
  on 2026-08-15: 92 lowercase `pk_` constraint names and 0 uppercase,
  while the source writes `PK_`.

- `get_index_columns()` is a new internal generic. It returns the
  columns of one index in index order. It returns `character(0)` when no
  index of that name is on that table, and `NULL` when the backend has
  no catalogue reader. The SQLite method is under test. The PostgreSQL
  method was verified by hand against the `norsyss_data1` server on
  2026-08-15. Two tables in schema `anon`, both declaring `ind1`, each
  returned `isoyearweek` for its own index and nothing for the other. No
  automated test covers that method, because the csdb suite runs on
  SQLite alone.

- Column verification is defined for SQLite and for PostgreSQL, and for
  no other backend. SQL Server and MySQL dispatch to the `db_default`
  method, which returns `NULL`. On those two backends `add_indexes()`
  creates each index and does NOT verify it. `confirm_indexes()` there
  checks the name alone, so it cannot see a change of columns. Raising
  there instead would break every index creation on a backend that no
  test in this package covers.

- The quoting fix covers index DDL, and it does NOT cover table
  creation. `add_constraint()` pastes the table and the key columns into
  its `ALTER TABLE` statement in raw. The `db_default` and the
  `db_postgres` methods both do this, and both read
  `table_name_short_for_mssql_fully_specified_for_postgres_text`.
  `create_table()` calls `add_constraint()` after it creates the table.
  A PostgreSQL table whose schema or name holds a dot, a space or an
  upper case letter is therefore created, and then the call raises. The
  table stays in place with no primary key. Measured on `norsyss_data1`
  on 2026-08-15, for `zz_quote_probe.dot`, `zz_quote_probeUPPER` and
  `zz_quote_probe space`. Each of the three tables existed afterwards,
  and each raised inside `ALTER TABLE`, not inside `CREATE TABLE`. A
  table called `zz_quote_probe_plain` succeeded, so the three failures
  are the name shapes and not the probe. The live verification of the
  index fix therefore created its three probe tables by hand, with
  quoted SQL. `create_table()` then skipped its own block, because the
  table existed and the fields matched. `add_indexes()` therefore ran
  exactly as it runs in production. `drop_rows_where()`,
  `keep_rows_where()` and `drop_table()` for PostgreSQL interpolate the
  table with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), and
  carry the same defect for the same three name shapes.

- This release does NOT repair a table that already exists.
  `add_indexes()` runs from `create_table()`, once, at creation. The 87
  `anon_norsyss_data` partitions keep their missing indexes until a
  separate migration runs. Calling `confirm_indexes()` on such a table
  adds the missing index.

- Two indexes on `norsyss_data1` keep their legacy names, and csdb no
  longer manages either one. Drop them by hand:

  ``` sql
  DROP INDEX IF EXISTS anon.ind1;  -- on anon_norsyss_providers
  DROP INDEX IF EXISTS anon.ind2;  -- on anon_norsyss_data_xxpxx_h77
  ```

  Do not run these two statements yet. Run them only after the managed
  replacement index exists on that table, and after `confirm_indexes()`
  verified it there. This release does NOT repair a table that already
  exists, so an early drop leaves both tables with no index at all.

- `Imports` carries `uuid (>= 1.1-0)`. `index_physical_name()` calls
  [`uuid::UUIDfromName()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html),
  which arrived in uuid 1.1-0. Under an older uuid every index name
  raised `could not find function "UUIDfromName"`.

- `add_indexes()` does not retry an index that failed after the table
  itself was created. `create_table()` creates the table first, then
  adds the indexes. A failure in the index step leaves a table whose
  fields already match. A later call to `lazy_creation_of_table()`
  therefore skips the creation block, sets `lazy_created_table`, and
  never adds the missing index. Call `add_indexes()` or
  `confirm_indexes()` again to add it. This release does not change that
  behaviour.

- This release carries a version one day ahead of the calendar, on
  purpose. r-universe already publishes `2026.8.15` from commit
  `89c51e13`, which lacks these fixes. A second tree under that number
  would leave two sources sharing one version.

### Documentation

- `vignettes/csdb.Rmd` is no longer precompiled.
  `vignettes/_PRECOMPILER.R` and `vignettes/csdb.Rmd.orig` are deleted,
  and `.Rbuildignore` no longer names either one.
- Every chunk in that vignette now executes during `R CMD check`. A
  chunk with `error = FALSE` turns the check red if it raises
  unexpectedly. The six chunks that demonstrate a raise carry
  `error = TRUE`, and such a chunk passes whether it raises or not.
- The vignette therefore guards against new unexpected errors. The
  expected errors and every printed value are demonstrations, and the
  test suites assert them.
- The vignette gains four sections, and a chunk that runs demonstrates
  each one. They cover connection ownership, the fork guard, index
  naming, and the destructive-method guard.
- The destructive-method section covers both methods and all four cases.
  A refused frame leaves three sentinel rows in place, which is what
  shows that the validator runs before the drop.
- [`?DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md)
  said an inherited connection “returns wrong results and reports no
  error”. It CAN return wrong results. The private comment beside
  `discard_inherited_connection()` carried the same absolute.
- The vignette lost its “Overview” section, which restated “What csdb is
  for” and the two-class split immediately below them.
- The vignette no longer attaches `magrittr`. `magrittr` is in no
  `DESCRIPTION` field, and precompilation hid that, because
  `R CMD check` never ran the chunk.
- [`?DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md)
  gains two sections. “What the object creates in the database” names
  the table, the `PK_` constraint and the physical index name. “The case
  of a constraint name on PostgreSQL” carries the 92-to-0 measurement
  recorded above.
- [`?DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md)
  said the introduction vignette builds a table against a PostgreSQL
  database. It builds one on SQLite.
- Four `\dontrun{}` example blocks now run on SQLite, under
  `\donttest{}`. They are on
  [`?DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.md),
  [`?DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
  [`?csdb_get_auth_hook`](https://niphr.github.io/csdb/reference/csdb_get_auth_hook.md)
  and
  [`?csdb_set_auth_hook`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md).
- One `\dontrun{}` block remains, on
  [`?get_table_names_and_info`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md).
  It connects to a PostgreSQL server, so it cannot run here. That page
  also gains a SQLite block that does run.
- The `drop_index` PostgreSQL comment said the schema is every component
  of the table identity except the last. The code takes the penultimate
  component, which is what PostgreSQL wants: an index lives in a schema,
  and a catalog-qualified index name is not valid. The comment is
  corrected and the code is unchanged.

## Version 2026.8.15

### Bug Fixes

- `DBConnection_v9` no longer hands out a connection that another
  process opened. A fork copies the object and the open handle, so the
  child and the parent then use one socket. PostgreSQL answers with
  wrong results and reports no error.
  [`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
  reports TRUE on such a handle, so nothing detected it before.
- The class records the process that opens each connection, and compares
  that process against the current one. `is_connected()` reports FALSE
  after a fork, `connection` returns NULL, and `autoconnection` opens a
  connection for the child.
- `disconnect()` never closes a connection that another process opened.
  Closing it would close the parent’s socket, which is the corruption
  this release prevents.
- The object keeps a reference to an inherited handle. Without that
  reference, the garbage collector runs odbc’s finalizer and closes the
  parent’s socket anyway.
- This matters more from 2026.8.14 on, because one `DBConnection_v9` now
  serves many tables. `cs9::Task$run_parallel_plans()` forks with
  `pbmcapply::pbmclapply` and passes table objects into the workers.
- Measured against the `norsyss-postgres` server on 2026-08-14: four
  forked children each got their own backend process ID and their own
  correct result. With the guard disabled, two of those four children
  returned the parent’s backend process ID, and two failed with a type
  error.
- Same-process behaviour is unchanged. The 162 checks in the csdb suite
  and the 298 checks in the cs9 suite pass without an edit.

### Development

- `tests/testthat/test-fork-safety.R` covers the fork guarantee. Layer 1
  changes the recorded process ID on a SQLite connection, and runs
  everywhere. Layer 2 forks with
  [`parallel::mcparallel`](https://rdrr.io/r/parallel/mcparallel.html),
  and skips on Windows, which has no fork.
- The csdb suite now holds 206 passes, up from 162.
- This release carries a version one day ahead of the calendar, on
  purpose. `2026.8.14` is already published from an earlier tree that
  lacks the fork guard, so a second tree under that number would leave
  two different sources sharing one version. `cs9` requires
  `csdb (>= 2026.8.15)` for the same reason: that floor names the guard.
- The `norsyss-postgres` server now allows 300 connections, measured
  2026-08-14. The 97-usable figure below described the server on
  2026-08-13, when the import failed.

## Version 2026.8.14

### New Features

- `DBTable_v9$new()` takes a `dbconnection` argument. Pass an existing
  `DBConnection_v9` and the table uses it instead of building its own.
  The argument is last, so a subclass can still forward the earlier
  seven positionally.
- `DBTable_v9$disconnect()` closes only a connection the object built
  itself. A connection passed as `dbconnection` is borrowed. The method
  leaves it open, and the caller decides when it closes.
- One `DBConnection_v9` can now serve many tables. The motivating case
  is `cs9`, which builds one `DBTable_v9` for every partition of a
  partitioned table. A table with 106 partitions therefore opened 106
  connections at once, against 97 usable slots on the `norsyss-postgres`
  server. This release only makes the sharing possible. `cs9` MUST pass
  the shared connection itself.

### Development

- `tests/testthat/test-shared-connection.R` covers the new argument. It
  asserts injection, the unchanged default, the borrowed no-op, the
  owned close, and a repeated `disconnect()`.

## Version 2026.8.6

### Licensing

- The copyright holder is now **Folkehelseinstituttet**. It read “Core
  Surveillance”, which names the package family rather than a legal
  entity.
- `DESCRIPTION` `Authors@R` now declares that holder with
  `role = "cph"`. It declared no copyright holder at all, and neither
  did any other package in the fleet. Nothing in `R CMD check` reports
  that.
- The copyright year is now 2026. It read 2023.
- `CLAUDE.md` now carries a Licensing section, so the year gets checked
  rather than silently ageing.

### Documentation

- Repository prose now follows ASD-STE100 (Simplified Technical
  English). The sweep covered the roxygen2 blocks in `R/`, both
  vignettes, and `README.md`. It changed no claim and no executable
  code. `index.md` needed no change.

- No roxygen sentence runs over 25 words. Counted per authored unit,
  which is one `@description`, `@param`, `@return`, `@seealso`,
  paragraph or Rd `\item`, the count fell from 6 to 0. The longest
  sentence fell from 36 words to 24.

- Roxygen fields and `\itemize` items now end in a full stop. Without
  one, a sentence splitter runs straight through the field boundary and
  reports a merge as one sentence. That is where the 74-word to 97-word
  readings came from, in blocks whose longest authored sentence was
  under 25 words.

- `vignettes/csdb.Rmd`, `vignettes/backends.Rmd` and `README.md` are
  also at zero sentences over 25 words. The counts before were 3, 1 and
  2.

- `vignettes/csdb.Rmd` and `vignettes/csdb.Rmd.orig` carry identical
  prose edits, so the generated file and its source stay in sync.
  `vignettes/_PRECOMPILER.R` was not re-run, and no chunk output
  changed.

- The v3 statement in the introduction vignette keeps its size. `csdb`
  CAN store a `csfmt_rts_data_v3`, with the `_blank` pair or with a
  validator of your own. What is missing is a validator that knows the
  v3 shape.

- Ornamental adjectives are gone from the reference pages: “robust” from
  `DBConnection_v9`, “sophisticated” and “comprehensive” from
  `DBTable_v9`, and “comprehensive” from
  [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md).
  The `DBTable_v9` title is now “R6 Class representing a database
  table”, parallel with `DBConnection_v9`.

- The introduction vignette opens with prose instead of with code
  output. pkgdown promotes `vignettes/csdb.Rmd` to “Get started”, and
  the first thing on that page was the `data.table` attach message:
  `Attaching package: 'data.table'` and the `%notin%` masking line. A
  new “What csdb is for” section now comes first. It says what the
  package does, splits `DBConnection_v9` (the connection) from
  `DBTable_v9` (one table), tabulates the three backends, names the
  missing v3 validator, and says where `csdb` sits in the stack. Its two
  chunks run without a database server.

- The [`library(data.table)`](https://r-datatable.com) and
  [`library(magrittr)`](https://magrittr.tidyverse.org) chunk is now
  `message = FALSE`. No chunk in the vignette uses `%>%` or bare
  `data.table` syntax, so the two attach messages announced masking that
  nothing below them relied on. The
  [`library()`](https://rdrr.io/r/base/library.html) calls themselves
  are unchanged.

- The overview states a current limitation plainly. `csdb` exports
  field-type and field-contents validators for `csfmt_rts_data_v1` and
  `csfmt_rts_data_v2` and none for `csfmt_rts_data_v3`;
  `grep("v3", getNamespaceExports("csdb"))` returns `character(0)`. That
  matters now rather than later, because `cstidy` marks v1 and v2
  deprecated in favour of `set_csfmt_rts_data_v3()`, and `csalert`’s
  pipeline ends in `ens_collapse(heal = TRUE)`, which returns a
  `csfmt_rts_data_v3`. A v3 result can still be written, with the blank
  validators or with a function of the user’s own, but nothing then
  checks its columns.

- The overview shows `is_connected()` returning `FALSE` immediately
  after `DBConnection_v9$new()` and again after `DBTable_v9$new()`. The
  second call uses a PostgreSQL configuration naming a server that is
  not running, which is the strongest form of the claim: neither
  constructor opens a connection.

- `vignettes/csdb.Rmd` was regenerated from `vignettes/csdb.Rmd.orig` by
  `vignettes/_PRECOMPILER.R`. Apart from the new section and the two
  suppressed attach messages, the only change in it is the
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html) path, which
  differs on every run.

### Bug Fixes

- `DBTable_v9$connect()` was documented as “Connect from the database”.
  It connects to the database, which is what `DBConnection_v9$connect()`
  already said.
- `DBTable_v9$drop_indexes()` was documented as “Drops all indees from
  the database table”. The word is “indexes”.
- `DBTable_v9$insert_data()` carried a stray prose line after
  `@param verbose`, so roxygen2 rendered the `verbose` argument as
  “Boolean. Inserts data into the database table”. That sentence is now
  the method’s `@description`, and `verbose` reads “Boolean.”

### Development

- `man/` was regenerated with roxygen2 8.0.0, the version `DESCRIPTION`
  declares. `NAMESPACE` is unchanged.
- Release notes for 2026.8.5 and earlier are left as they shipped. A
  changelog is a record, and rewording a released entry changes that
  record.

## Version 2026.8.5

### New Features

- SQLite is a third backend. `driver = "SQLite"` with `db` set to a file
  path connects through `RSQLite`, which is now in `Imports`. The driver
  string is matched case-insensitively, so `sqlite`, `SQLite` and
  `SQLITE` all select it; the two ODBC driver strings keep exact
  matching, because they must equal an `odbcinst.ini` entry.

- `DBConnection_v9` creates the parent directory of `db` if it does not
  exist, then opens the file with `extended_types = TRUE`. That argument
  is required, not cosmetic: without it a `DATE` column reads back as
  the integer `18262` rather than a `Date`, and
  [`validator_field_contents_csfmt_rts_data_v1()`](https://niphr.github.io/csdb/reference/validator_field_contents_csfmt_rts_data_v1.md)
  rejects it. No `USE <db>;` is issued, because the file is already the
  database.

- `DBConnection_v9$print()` shows the driver and the file path for
  SQLite, and omits server, port, user, password, SSL mode and trusted
  connection, none of which SQLite reads.

- `DBTable_v9` identifiers under SQLite are the bare table name:
  `DBI::Id(table = <table_name>)` and the plain string. `schema` is
  ignored entirely, because SQLite has no schemas.

- `create_table()` on SQLite inlines the primary key in the
  `CREATE TABLE` statement and marks every key column `NOT NULL`.
  `add_constraint()` is therefore a no-op there. SQLite has no
  `ALTER TABLE ... ADD CONSTRAINT ... PRIMARY KEY`; the statement the
  other backends use is a syntax error.

- The SQLite field-type map is closed: `TEXT`, `INTEGER`, `DOUBLE`,
  `BOOLEAN`, `DATE` and `DATETIME` are accepted and anything else is an
  error naming the column and the type. SQLite accepts any declared type
  name, so `VARCHAR(100)`, `TEXT(100)` or a misspelling would otherwise
  create a table with an unintended affinity and no warning.

- `insert_data()` on SQLite writes through
  [`DBI::dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html).
  There is no staging CSV and no external client binary: SQLite is a
  file, and `dbAppendTable()` writes 100,000 rows in about 0.02 seconds.
  The `file` argument is accepted and ignored.

- `insert_data()` on SQLite copies its argument before writing, so the
  caller’s `data.table` is not modified by reference. The three other
  backends reach `write_data_infile()`, which has always modified it in
  place.

- `upsert_data()` on SQLite stages the rows in a temporary table and
  then issues `INSERT ... ON CONFLICT (<keys>) DO UPDATE SET`, falling
  back to `DO NOTHING` when every field is a key. SQLite has neither
  `MERGE` nor `ON DUPLICATE KEY UPDATE`. Three preconditions are checked
  before any SQL is emitted, because each fails late and obscurely
  otherwise: `keys` must be non-empty, or the statement is
  `ON CONFLICT ()`; every key must be one of the fields; and `fields`
  must be exactly the table’s live columns, because
  `CREATE TABLE ... AS SELECT` discards defaults and a partial field
  list would insert NULL into every omitted column.

- `drop_all_rows()` is now an S7 generic. SQL Server and PostgreSQL keep
  the `TRUNCATE TABLE` statement they always received, unchanged; SQLite
  gets `DELETE FROM <table>`, because `TRUNCATE TABLE` is a syntax error
  there. `DELETE` leaves the primary key and every index intact, which
  matters because the SQLite `add_constraint()` cannot put a dropped
  primary key back.

- `keep_rows_where()` on SQLite emits
  `DELETE FROM <table> WHERE (<condition>) IS NOT TRUE`, not
  `NOT (<condition>)`. The two are not the same statement: `DELETE`
  removes only rows whose predicate evaluates to TRUE, and the negation
  of NULL is NULL, so a plain negation silently retains every row on
  which the condition is NULL, although `SELECT ... WHERE <condition>`
  would not have kept it. `IS NOT TRUE` folds NULL into FALSE and gives
  the exact complement. It is also a `DELETE` rather than the
  drop-and-rename the other two backends use, for the same primary-key
  reason as `drop_all_rows()`.

- `drop_rows_where()` on SQLite emits
  `DELETE FROM <table> WHERE <condition>`.

- `add_indexes()` on SQLite emits
  `CREATE INDEX IF NOT EXISTS <index> ON <table> (<keys>)`, with the
  table name unqualified. SQLite lets the index name carry a schema but
  never the table: `CREATE INDEX ind ON main.tab (a)` is
  `near ".": syntax error`. `drop_indexes()` emits
  `DROP INDEX IF EXISTS <index>`, which names the index alone, because a
  SQLite index belongs to the schema rather than to the table.

- `confirm_indexes()` on SQLite now executes no DDL when the indexes
  already match. `get_indexes()` excludes SQLite’s own index names and
  orders by `rowid`. Both are required: a `PRIMARY KEY` auto-creates
  `sqlite_autoindex_<table>_1`, which is a row in `sqlite_master`
  exactly like a user index, and `rowid` order is creation order, which
  is the order `add_indexes()` works in. `confirm_indexes()` compares
  with [`identical()`](https://rdrr.io/r/base/identical.html), so an
  extra name or a different order would drop and re-add every index on
  every call. The return value is a plain character vector for the same
  reason.

- [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md)
  has a method for `SQLiteConnection`. `nrow` is `COUNT(*)`, which is
  exact, unlike the `reltuples` estimate PostgreSQL reports and the
  `sp_spaceused` figure SQL Server reports. All three size columns are
  `NA_real_`: the `dbstat` virtual table is not compiled into the SQLite
  that `RSQLite` ships, so there is no per-table size to report, and
  `pragma page_count` describes the whole file. An empty database
  returns a zero-row table that still has all five columns.

- Both SQLite catalogue filters write the exclusion as
  `name NOT LIKE 'sqlite\_%' ESCAPE '\'`, escaping the underscore. `_`
  is a single-character wildcard in SQL `LIKE`, so the unescaped
  `'sqlite_%'` hides every name beginning “sqlite” followed by any
  character at all, not only SQLite’s own objects. A user index named
  `sqliteIdx` would never be found by `get_indexes()`, and
  `confirm_indexes()` would drop and re-add it on every call; a user
  table named `sqliteFoo` would be missing from
  [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md),
  so `DBTable_v9$nrow(use_count = FALSE)` and `DBTable_v9$info()` would
  report nothing for it.

### Known limitations

- `confirm_indexes()` compares index *names* only. An index with the
  right name and the wrong columns passes. This is the existing
  behaviour of all three backends and SQLite matches it.

### Documentation

- The introduction vignette now runs on SQLite, in a file created by
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html). It is
  precompiled from `vignettes/csdb.Rmd.orig`, and that precompilation
  used to need a live PostgreSQL database.
  [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html) defaults to
  `error = TRUE`, so on a machine without one it did not fail: it exited
  0 and wrote seven `#> Error` transcripts into the committed
  `vignettes/csdb.Rmd`, including a
  `Could not connect to database server ''`. Anyone can now rebuild the
  vignette and get the same output.
- Added `vignettes/backends.Rmd`, which puts a PostgreSQL `dbconfig` and
  a SQLite `dbconfig` side by side, runs one `DBTable_v9$new()`
  definition against each, and tabulates what a user must know: `schema`
  is ignored, the primary key is inlined at `CREATE TABLE` and cannot be
  added later, an unrecognised field type is rejected rather than passed
  through,
  [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md)
  reports an exact `COUNT(*)` and `NA` sizes, and no external client
  binary is needed. No chunk in it executes.
- `README.md`’s quick start is now the SQLite one, so it runs on a bare
  machine, and it links to both vignettes. The `$keep_rows_where()`
  caution is qualified: the copy, drop and rename it describes is the
  ODBC path, not the SQLite one.
- `index.md` and the `_pkgdown.yml` hero lede both name SQLite alongside
  PostgreSQL and SQL Server.

### Development

- Added `tests/testthat/test-sqlite-connection.R`, the first tests in
  the package that open a database connection. SQLite is a file, so they
  need no server.
- Added `tests/testthat/test-sqlite-indexes.R`. The block that proves
  `confirm_indexes()` emits no DDL reads `PRAGMA schema_version` before
  and after, not the index names: the names are identical whether the
  call did nothing or dropped and recreated every index, and
  `schema_version` increments on every schema change. A separate block
  creates an index named `sqliteBar` and a table named `sqliteFoo` and
  asserts both are visible, which is what pins the `ESCAPE` clause on
  the two catalogue filters.
- Added `tests/testthat/test-sqlite-data.R`, covering the five write and
  delete paths: type round-trip, the non-finite scrub, the caller’s
  data.table being left alone, upsert update-not-duplicate, the three
  upsert preconditions, the NULL-condition row, `drop_all_rows()`
  leaving the indexes, and identifiers that need quoting.
- The `Inf`/`NaN` to `NA` loop moved out of `write_data_infile()` into
  an internal `scrub_non_finite()`, called from there and from the
  SQLite write path. `Inf` survives
  [`DBI::dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
  and reads back as `Inf`, so without it SQLite would silently disagree
  with the two backends that write `NA`. The `POSIXt` to character
  conversion is not shared: SQLite needs a `POSIXct` to stay one, so
  that `extended_types = TRUE` round-trips it through a `DATETIME`
  column.
- `dbplyr` is in `Imports`. It always was a hard requirement and was
  never declared: `DBTable_v9$tbl()` calls
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) on a
  DBI connection, which dispatches to `dplyr:::tbl.DBIConnection()` and
  stops in `check_dbplyr()` when dbplyr is absent. Three documented
  methods go through it, `tbl()`, `print_dplyr_select()` and
  `nrow(use_count = TRUE)`, and `tbl()` is the only read path the
  package offers, so a csdb without dbplyr is write-only. The gap never
  surfaced because nothing in csdb called `tbl()` until the SQLite tests
  did; on a library without dbplyr those seven blocks error and the
  other 108 assertions pass. `Suggests` was rejected on measurement:
  dbplyr adds three packages to an `Imports` closure of 42, and the
  alternative is to make the package’s only read path optional. No csdb
  code names dbplyr, so `fix_dbplyr()` in `R/xxx_small_import_fix.R`
  holds a `dbplyr::` reference for the same reason `fix_r6()` and
  `fix_s7()` hold theirs: without it `R CMD check` reports “All declared
  Imports should be used”.
- `RSQLite` is in `Imports` and has no S3 fallback in
  `get_db_classes()`, which stops with a message naming RSQLite if the
  real S4 `SQLiteConnection` class is absent. A
  [`S7::new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.html)
  fallback would be worse than useless: with the real S4 `DBIConnection`
  default present, methods registered against the fallback lose dispatch
  silently and run the MySQL-flavoured `db_default` SQL, and registering
  the real class later does not retarget them.
- Documentation is generated by roxygen2 8.0.0. `DESCRIPTION` now
  declares `Config/roxygen2/version` in place of `RoxygenNote`, and
  every `.Rd` file was regenerated by that version. `NAMESPACE` is
  unchanged.

## Version 2026.8.4

### Documentation

- `README.md` now carries what the package is, installation, one quick
  start, and a table that routes a task to the function that does it. It
  also states two things the API does not do: `create_table()` drops and
  rebuilds a table whose columns differ from `names(field_types)`, and
  no method opens a transaction.
- All 11 exported functions gained a `@seealso` that says whether the
  introduction vignette demonstrates them. Four appear in a vignette
  code chunk (`DBConnection_v9`, `DBTable_v9`,
  `validator_field_types_blank`, `validator_field_contents_blank`); the
  other seven appear nowhere in the vignette, and their `@seealso` says
  so.
- Added three `@family` groups: auth hook functions (both address the
  `csdb.auth_hook` option, one writing it and one reading it), field
  type validators (one `db_field_types` argument, checked once inside
  `DBTable_v9$new()`), and field contents validators (one `data`
  argument, called from `insert_data()` and `upsert_data()`).
  `DBConnection_v9` and `DBTable_v9` are grouped as database classes:
  `DBTable_v9$new()` takes a `dbconfig` list of exactly the 10 arguments
  `DBConnection_v9$new()` accepts, and builds one.

### Bug Fixes

- [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md):
  the documented PostgreSQL example connected through
  `RPostgres::Postgres()`. Those connections are of class
  `PqConnection`, and the generic has methods for `PostgreSQL` and
  `Microsoft SQL Server` only, so that example cannot dispatch; it
  errors with “no applicable method”. It now connects through the
  `PostgreSQL Unicode` ODBC driver, which is the class the methods are
  written for. `RPostgres` was also absent from `Imports` and
  `Suggests`.
- [`get_table_names_and_info()`](https://niphr.github.io/csdb/reference/get_table_names_and_info.md):
  the `nrow` column was documented as the number of rows. It is
  `reltuples` from `pg_class` on PostgreSQL, which is an estimate, and
  the `rows` column of `sp_spaceused` on Microsoft SQL Server.
  Documented as reported, not as exact.
- `DBConnection_v9`: the documented PostgreSQL example used
  `driver = "PostgreSQL"`. Only `"PostgreSQL Unicode"` selects a
  PostgreSQL branch in the connection code, so `"PostgreSQL"` falls
  through to the generic branch, which does not pass `database`, and is
  then followed by `USE <db>;`. Changed to `"PostgreSQL Unicode"`.
- [`validator_field_types_csfmt_rts_data_v2()`](https://niphr.github.io/csdb/reference/validator_field_types_csfmt_rts_data_v2.md):
  the example vector labelled “Valid field types” returned `FALSE`,
  because it omitted `isoquarter` and `isoyearquarter`, which the v2
  schema holds at positions 11 and 12. The example now returns `TRUE`,
  and a second call shows the v1 layout returning `FALSE`.
- `DBTable_v9`: the documented example called
  `$add_indexes(c("name", "date_created"))`, but that method takes no
  arguments and reads `self$indexes`. Indexes are now declared in the
  constructor. The same example passed `data.frame`s to `$insert_data()`
  and `$upsert_data()`, both of which reach `data.table` syntax
  (`[ , (col) := ]`, `with = FALSE`) and require a `data.table`. Changed
  to
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

### Development

- [`csdb_set_auth_hook()`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md),
  `DBConnection_v9` and `DBTable_v9` gained runnable examples for the
  parts that need no database server: setting and restoring the hook,
  and creating an object without connecting. Their `\dontrun{}` blocks
  keep the parts that need a server.
- Added `^Rplots\.pdf$` to `.Rbuildignore`.

## Version 2026.5.13

CRAN release: 2026-05-13

### Bug Fixes

- `DBTable_v9$nrow(use_count = TRUE)` now calls
  [`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html)
  instead of a bare `n()`. This is hygiene only: the bare call sits
  inside the list passed to
  [`R6::R6Class()`](https://r6.r-lib.org/reference/R6Class.html), which
  `codetools` never walks, so it produced no `R CMD check` complaint,
  and `dbplyr` renders both spellings to identical SQL.
- PostgreSQL methods (`create_table`, `keep_rows_where`, `drop_table`)
  now quote `role_create_table` via
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
  when emitting `SET ROLE`. Previously the role name was interpolated
  raw, which broke on identifiers containing hyphens, mixed case, or
  reserved words (e.g. `SET ROLE token-user` -\> syntax error), and was
  a SQL-injection vector if the value came from an env var.

## Version 2026.2.2

CRAN release: 2026-03-31

### New Features

- Added authentication hook system
  ([`csdb_set_auth_hook()`](https://niphr.github.io/csdb/reference/csdb_set_auth_hook.md),
  [`csdb_get_auth_hook()`](https://niphr.github.io/csdb/reference/csdb_get_auth_hook.md))
  to allow automatic credential refresh (e.g., Kerberos tickets) when
  connection fails

## Version 2026.1.28

### Bug Fixes

- Improved database connection error messages to include the original
  driver error details for easier debugging

## Version 2025.7.28

### Bug Fixes

- Fixed namespace loading error by importing methods::initialize generic
- Improved error handling in .onLoad() function for S7 operations
- Package now passes CRAN namespace loading requirements

## Version 2025.7.19

### Bug Fixes

- Fixed DBConnection_v9 print method to display “SSL mode” instead of
  “trusted connection” for PostgreSQL connections

### Development

- Added S7 package to imports in preparation for S3 to S7 method
  conversion to improve CRAN compliance
- S7 transition planned to reduce exported API surface while maintaining
  functionality through R6 classes

## Version 2025.7.17

- Updated package for CRAN submission with comprehensive improvements
- Added comprehensive documentation with examples for all exported
  functions
- Fixed critical CRAN compliance issues including system tool
  availability checks
- Added proper R6 class documentation with detailed usage examples
- Improved all validator function documentation with clear examples
- Added missing dependencies and fixed import declarations
- Updated .Rbuildignore to exclude system files and build artifacts
- Added CLAUDE.md for future development guidance
- Fixed vignette title and improved documentation quality
- All functions now pass R CMD check with only acceptable NOTEs

## Version 2025.2.15

- Including `role_create_table` in drop_table for PostgreSQL.

## Version 2024.10.25

- `role_create_table` is now included for dbconnection_v9/dbtable_v9, so
  that the role can be changed when creating tables in PostgreSQL.

## Version 2024.3.27

- csdb now supports PostgreSQL databases as well as MS SQL Server.

## Version 2024.3.11

- Including use_count as an argument in nrow in DBTable_v9, which is
  slower but more accurate.

## Version 2024.3.7

- Including confirm_insert_via_nrow in DBTable_v9. Checks nrow() before
  insert and after insert. If nrow() has not increased sufficiently,
  then attempt an upsert.

## Version 2023.12.28

- Including validator_field_types_csfmt_rts_data_v2 and
  validator_field_contents_csfmt_rts_data_v2.

## Version 2023.12.26

- Including georegion in validator_field_contents_csfmt_rts_data_v1.

## Version 2023.4.14

- `get_table_names_and_info` is now ordered according to `table_name`.

## Version 2023.4.12

- `get_table_names_and_nrow` is now changed to
  `get_table_names_and_info` and also includes size_total_gb,
  size_data_gb, size_index_gb.
- `info` is now included as a method for `DBTable_v9`

## Version 2023.4.4

- `confirm_indexes` is now added to `DBTable_v9`, which confirms that
  the names and number of indexes in the database are the same as in the
  R code. It does not confirm the contents of the indexes!
- `nrow` is now added to `DBTable_v9`, which is an application of the
  new `get_table_names_and_nrow` function.
- `get_table_names_and_nrow` added as an exported function, that will
  get all the table names and the nrows from a dbconnection.

## Version 2023.4.2

- `create_table` now automatically adds the indexes.

## Version 2023.3.31

- Removing info messages from `drop_rows_where`.

## Version 2023.3.8

- connect() in DBConnection_v9 is smarter, more robust with error
  checking and making fewer useless calls to the db. Tries to connect
  twice now before throwing an error.
- autoconnection is now more robust in DBConnection_v9.

## Version 2023.2.17

- Package is created.
