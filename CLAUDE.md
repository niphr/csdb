# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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

# Build vignettes
knitr::knit("vignettes/csdb.Rmd.orig", "vignettes/csdb.Rmd")
```

### Testing
The package uses testthat for testing (suggested in DESCRIPTION), though no test files are currently present in the repository.

```r
# Run tests
devtools::test()

# Run specific test
testthat::test_file("tests/testthat/test-filename.R")
```

## Git Commit Requirements

**CRITICAL**: When committing to this repository, commit messages must NEVER mention "claude", "ai", "Claude Code", or any AI-related terms. All commits should appear as regular human development work.

## Code Architecture

### Core Classes

The package is built around two main R6 classes:

1. **`DBConnection_v9`** (`R/r6_dbconnection_v9.R`): 
   - Handles database connections
   - Supports multiple database drivers (SQL Server, PostgreSQL)
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
- **`util_database.R`**: Low-level database utilities for file operations and data loading
- **`drop_rows_where.R`**: Functions for conditional row deletion

### Package Structure

- `R/`: Main source code
- `man/`: Generated documentation files
- `vignettes/`: Package vignettes (use `_PRECOMPILER.R` to build from `.Rmd.orig`)
- `data/`: Package data (includes `nor_covid19_cases_by_time_location`)
- `data-raw/`: Raw data and processing scripts

## Database Support

The package supports multiple database backends:
- Microsoft SQL Server (via ODBC)
- PostgreSQL
- Each backend has specific implementations for metadata retrieval and operations

## Development Notes

- Uses R6 classes for object-oriented database interactions
- Depends on data.table for efficient data manipulation
- Uses DBI and odbc for database connectivity
- Includes comprehensive field validation system for data quality
- Package follows roxygen2 documentation standards
- Uses devtools workflow for development