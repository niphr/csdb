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

## TODO: S3 to S7 Conversion for CRAN Compliance

### Project Goal
Convert internal S3 methods to S7 methods to reduce the exported API surface and improve CRAN compliance. Only export core user-facing functions while keeping database utilities internal.

### Current State Analysis

#### Functions to Keep Exported (Target API):
- `DBConnection_v9` (R6 class)
- `DBTable_v9` (R6 class)
- `get_table_names_and_info()` (S3 generic - user-facing)
- `nor_covid19_cases_by_time_location` (dataset)
- All validator functions:
  - `validator_field_types_blank`
  - `validator_field_types_csfmt_rts_data_v1`
  - `validator_field_types_csfmt_rts_data_v2`
  - `validator_field_contents_blank`
  - `validator_field_contents_csfmt_rts_data_v1`
  - `validator_field_contents_csfmt_rts_data_v2`

#### Functions to Convert to Internal S7 Methods:
- `load_data_infile()` + 3 S3 methods
- `upsert_load_data_infile()` + 3 S3 methods
- `create_table()` + 3 S3 methods
- `add_constraint()` + 2 S3 methods
- `drop_constraint()` + 1 S3 method
- `get_indexes()` + 2 S3 methods
- `drop_index()` + 3 S3 methods
- `add_index()` + 3 S3 methods
- `drop_rows_where()` + 2 S3 methods
- `keep_rows_where()` + 2 S3 methods
- `drop_table()` + 2 S3 methods

**Total**: 11 S3 generics and 26 S3 methods to convert

### Implementation Plan

#### Phase 1: Environment Setup ✅ COMPLETED
1. **Update R version** ✅ DONE - R 4.5.0 supports S7
2. **Install S7 package** ✅ DONE
3. **Add S7 to DESCRIPTION** ✅ DONE - S7 in Imports
4. **Create `.onLoad()` function** ✅ DONE - Proper S7 registration with error handling

#### Phase 2: S7 Conversion
1. **Convert S3 generics to S7 generics**:
   ```r
   # Example conversion:
   load_data_infile <- new_generic("load_data_infile", "connection")
   ```

2. **Define S7 classes for database connections**:
   ```r
   # Wrap existing S3 classes
   db_mssql <- new_S3_class("Microsoft SQL Server")
   db_postgres <- new_S3_class("PostgreSQL")
   ```

3. **Register S7 methods**:
   ```r
   method(load_data_infile, db_mssql) <- function(connection, ...) {
     # Implementation
   }
   ```

4. **Update `.onLoad()` with method registration**:
   ```r
   .onLoad <- function(libname, pkgname) {
     S7::methods_register()
   }
   ```

#### Phase 3: Cleanup
1. **Remove @export tags** from converted functions
2. **Update NAMESPACE** (remove S3method() entries)
3. **Update documentation** to mark functions as internal
4. **Test all database operations** through R6 classes

#### Phase 4: Validation
1. **Run `R CMD check --as-cran`** to verify compliance
2. **Test PostgreSQL and SQL Server operations**
3. **Validate public API functionality**
4. **Run comprehensive tests**

### Technical Benefits

#### CRAN Compliance:
- **Reduced public API**: 6 exports vs 14 current exports
- **Cleaner namespace**: No exported S3 methods
- **Better maintainability**: Internal methods easier to modify
- **Future-proof**: S7 is designed as S3/S4 successor

#### Functional Benefits:
- **No breaking changes**: Public API unchanged
- **Maintained functionality**: All database operations work identically
- **Better organization**: Cleaner internal structure
- **S3 interoperability**: S7 provides excellent S3 compatibility

### Risk Assessment

#### Low Risk:
- S7 is CRAN-approved and stable
- Excellent S3 interoperability via `new_S3_class()`
- No changes to public API
- Internal functions remain fully functional

#### Mitigation:
- Thorough testing of method dispatch
- Comprehensive validation before deployment
- Fallback plan: revert to S3 if issues arise

### Prerequisites

1. **R version >= 4.0.0** (check with `R.version.string`)
2. **S7 package installation**: `install.packages("S7")`
3. **Understanding of S7 syntax**: Review S7 documentation
4. **Backup current working state** before conversion

### Success Criteria

1. **CRAN check passes** ✅ COMPLETED: `R CMD check --as-cran` with 0 errors/warnings
2. **Public API unchanged**: All user-facing functions work identically
3. **Internal methods functional**: Database operations work through R6 classes
4. **Cleaner exports**: Only 6 exported functions vs 14 current
5. **Future-proof architecture**: Ready for S7 ecosystem adoption

### Recent Progress (v2025.7.28)

- ✅ **Fixed namespace loading error**: Added `importFrom(methods,initialize)` to NAMESPACE
- ✅ **Improved .onLoad() function**: Added proper error handling for S7 operations
- ✅ **CRAN compliance**: Package now passes all namespace loading requirements
- ✅ **No breaking changes**: Public API remains unchanged

### Next Steps

1. **Update R version** to latest stable release
2. **Install S7 package** and dependencies
3. **Create feature branch** for conversion work
4. **Begin Phase 1 implementation** with environment setup