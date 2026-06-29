# Get table names, number of rows, and size information

Retrieves comprehensive information about database tables including
their names, row counts, and storage size metrics. This function
provides database-specific implementations for different database
systems.

## Usage

``` r
get_table_names_and_info(connection)
```

## Arguments

- connection:

  A database connection object (e.g., from
  [`dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html))

## Value

A data.table containing table information with columns:

- table_name:

  Character. Name of the table

- nrow:

  Numeric. Number of rows in the table

- size_total_gb:

  Numeric. Total size of the table in gigabytes

- size_data_gb:

  Numeric. Size of data in gigabytes

- size_index_gb:

  Numeric. Size of indexes in gigabytes

## Examples

``` r
if (FALSE) { # \dontrun{
# Microsoft SQL Server example
con <- DBI::dbConnect(odbc::odbc(), 
                      driver = "ODBC Driver 17 for SQL Server",
                      server = "localhost", 
                      database = "mydb")
table_info <- get_table_names_and_info(con)
print(table_info)
DBI::dbDisconnect(con)

# PostgreSQL example  
con <- DBI::dbConnect(RPostgres::Postgres(),
                      host = "localhost",
                      dbname = "mydb",
                      user = "user")
table_info <- get_table_names_and_info(con)
print(table_info)
DBI::dbDisconnect(con)
} # }
```
