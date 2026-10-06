# AGENTS.md

## Project Overview

This is `csdb`, an R package that provides an abstracted system for easily working with databases with large datasets. It's part of the csverse ecosystem of R packages for disease surveillance.

## Development Commands

### Package Building and Installation
```r
# Install package in development mode
devtools::install()

# Build package
devtools::build()

# Check package
devtools::check()

# Load package for development
devtools::load_all()
```

### Documentation
```r
# Generate documentation using roxygen2
devtools::document()

# Build vignettes. csdb.Rmd is a normal vignette and R CMD check runs its
# chunks, so there is nothing to precompile.
devtools::build_vignettes()
```

### Testing
`tests/testthat/` holds the testthat suite. No test needs a database server. A test uses SQLite, or mocks DBI and checks the SQL text, or uses no database.

```r
# Run tests
devtools::test()

# Run one test file, for example tests/testthat/test-sqlite-data.R
devtools::test(filter = "sqlite-data")
```

### CRAN Preparation and Checking

**IMPORTANT**: Always use the `--as-cran` flag when checking for CRAN submission to catch all potential issues:

```bash
# Build and check with CRAN standards
R CMD build .
R CMD check --as-cran packagename_version.tar.gz

# Alternative using devtools
devtools::check(args = c('--as-cran'))
```

**Target**: 0 NOTEs, 0 WARNINGs, 0 ERRORs for CRAN acceptance.

The `--as-cran` flag enables additional checks that CRAN uses, including:
- Stricter file structure validation
- Detection of non-standard files
- Enhanced dependency checking
- More comprehensive code analysis

## Git Commit Requirements

**CRITICAL**: When committing to this repository, commit messages must NEVER mention "claude", "ai", "Claude Code", or any AI-related terms. All commits should appear as regular human development work.

## Code Architecture

### Core Classes

The package is built around two main R6 classes:

1. **`DBConnection_v9`** (`R/r6_DBConnection_v9.R`):
   - Handles database connections
   - Supports three database drivers: SQL Server and PostgreSQL through ODBC, and SQLite through RSQLite
   - Never hands out a connection that another process opened. After a fork, the child opens its own connection
   - Manages connection configuration, authentication, and connection lifecycle

2. **`DBTable_v9`** (`R/r6_DBTable_v9.R`):
   - Represents individual database tables
   - Provides methods for data manipulation (insert, upsert, delete)
   - Handles table structure management (indexes, constraints)
   - Built on top of DBConnection_v9

### Data Validation System

The package includes a comprehensive validation system with validators for:
- Field types validation (`validator_field_types_*`)
- Field contents validation (`validator_field_contents_*`)
- Support for custom schema formats (e.g., `csfmt_rts_data_v1`, `csfmt_rts_data_v2`)

### Database Utilities

- **`get_table_names_and_info.R`**: Database-specific functions to retrieve table metadata (names, row counts, sizes)
- **`util_general.R`**: Helpers that touch no database

`util_database.R` was one file of 1445 code lines. CI now fails any file over 1000, so it is five. The split moved whole top-level expressions and changed none of them:

- **`util_database.R`**: the S7 generics and the `db_*` class objects. **Every other file here registers methods against these, and the left side of `S7::method(generic, class) <-` evaluates while the namespace is built. So this file MUST load first.** It does, because `.` sorts before `_`. A sibling named to sort earlier fails at install.
- **`util_database_index.R`**: index naming, `get_indexes`, `drop_index`, `add_index`
- **`util_database_load.R`**: `load_data_infile` and `upsert_load_data_infile`
- **`util_database_rows.R`**: `drop_all_rows`, `drop_rows_where`, `keep_rows_where`, `drop_table`
- **`util_database_table.R`**: `create_table`, `sqlite_field_types`, constraints

### Package Structure

- `R/`: Main source code
- `man/`: Generated documentation files
- `vignettes/`: Package vignettes. `csdb.Rmd` runs on SQLite, so `R CMD check` executes every chunk. A chunk with `error = FALSE` turns the check red if it raises unexpectedly. A chunk with `error = TRUE` passes whether it raises or not, and no printed value is compared against anything. The vignette therefore guards against new unexpected errors, and the test suites assert the values. `backends.Rmd` is `eval = FALSE` throughout, because it shows a PostgreSQL configuration that cannot connect during a check.
- `data/`: Package data (includes `nor_covid19_cases_by_time_location`)
- `data-raw/`: Raw data and processing scripts

## Database Support

The package supports three database backends:
- Microsoft SQL Server (via ODBC)
- PostgreSQL (via ODBC)
- SQLite (via RSQLite), which needs no server
- Each backend has specific implementations for metadata retrieval and operations

## Development Notes

- Uses R6 classes for object-oriented database interactions
- Depends on data.table for efficient data manipulation
- Uses DBI, odbc and RSQLite for database connectivity
- Uses S7 generics for the database-specific utilities in `util_database*.R`
- Includes comprehensive field validation system for data quality
- Package follows roxygen2 documentation standards
- Uses devtools workflow for development

## Licensing

This package is `MIT + file LICENSE`. Two files carry the licence and they MUST agree with each other:

- `LICENSE` holds exactly two lines, `YEAR:` and `COPYRIGHT HOLDER:`. CRAN requires that shape for `MIT + file LICENSE`. Do not put the licence text there.
- `DESCRIPTION` `Authors@R` MUST name the same holder, with `role = "cph"`.

The copyright holder for this package is **Folkehelseinstituttet**.

**Check the year at the start of each calendar year, and whenever you edit `DESCRIPTION`.** Nothing in `R CMD check` tests the copyright year, so a stale one goes unnoticed indefinitely. A fleet sweep on 2026-08-06 found years of 2021, 2023 and 2025 still in place across 15 packages. Not one package declared a `cph` role at all.

Check both in one step:

```r
readLines("LICENSE")
a <- unclass(eval(parse(text = read.dcf("DESCRIPTION")[1, "Authors@R"])))
Filter(function(p) "cph" %in% p$role, a)
```
