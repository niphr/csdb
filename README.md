# csdb <a href="https://niphr.github.io/csdb/"><img src="man/figures/logo.png" align="right" width="120" /></a>

[![CRAN status](https://www.r-pkg.org/badges/version/csdb)](https://cran.r-project.org/package=csdb)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/csdb)](https://cran.r-project.org/package=csdb)

## Overview

[csdb](https://niphr.github.io/csdb/) gives you two R6 classes for one ODBC
database. `DBConnection_v9` holds the connection settings and opens or closes
the connection. `DBTable_v9` owns a single table: its columns, keys, indexes and
validators. Both classes connect through `odbc`. The read, write and index
methods are specialised for Microsoft SQL Server and for PostgreSQL.

## Installation

``` r
install.packages("csdb")

# development version
pak::pak("niphr/csdb")
```

## Quick start

``` r
cfg <- list(
  driver = "PostgreSQL Unicode",
  server = "localhost",
  port = 5432,
  db = "mydb",
  schema = "public",
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

tab <- csdb::DBTable_v9$new(
  dbconfig = cfg,
  table_name = "cases",
  field_types = c(
    "location_code" = "TEXT",
    "date" = "DATE",
    "cases_n" = "INTEGER"
  ),
  keys = c("location_code", "date"),
  indexes = list("ind1" = c("location_code", "date"))
)

tab$insert_data(d) # d must be a data.table
tab$tbl() # lazy reference for dplyr; needs dbplyr
```

`$new()` opens nothing. The first method that reaches the database connects, and
`$insert_data()`, `$upsert_data()` and `$tbl()` create the table when it is
absent. Two cautions. When a table of that name exists and its columns differ
from `names(field_types)`, `$create_table()` drops it and builds it again, which
discards the rows. And csdb opens no transaction anywhere: `$keep_rows_where()`,
for one, sends its copy, its drop and its rename as three separate statements.

## Which function do I want?

| I want to ... | Use |
|---|---|
| hold settings, then open and close one connection | `DBConnection_v9` |
| insert, upsert and delete rows, and manage indexes | `DBTable_v9` |
| re-authenticate after the first failed connection attempt | `csdb_set_auth_hook()`, `csdb_get_auth_hook()` |
| read the row counts and sizes the database reports per table | `get_table_names_and_info()` |
| accept any field types | `validator_field_types_blank()` |
| accept any data | `validator_field_contents_blank()` |
| require the first 16 field types (v1) or 18 (v2) to match `csfmt_rts_data` | `validator_field_types_csfmt_rts_data_v1()`, `validator_field_types_csfmt_rts_data_v2()` |
| require valid `granularity_time`, `granularity_geo`, `border` and `sex` values, and a `Date` in `date` | `validator_field_contents_csfmt_rts_data_v1()`, `validator_field_contents_csfmt_rts_data_v2()` |

The type validator runs once, inside `DBTable_v9$new()`. The contents validator
runs inside `$insert_data()` and `$upsert_data()`, unless the data is `NULL` or
has no rows, which both methods return on first.

Reference pages, the changelog and the introduction vignette are at
<https://niphr.github.io/csdb/>.
