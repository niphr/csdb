# Field types validator for csfmt_rts_data_v1 schema

Validates that field types conform to the csfmt_rts_data_v1 schema
specification. This validator ensures that the first 16 fields match the
expected structure for real-time surveillance data format version 1.

## Usage

``` r
validator_field_types_csfmt_rts_data_v1(db_field_types)
```

## Arguments

- db_field_types:

  A named character vector of database field types

## Value

TRUE if field types are valid for csfmt_rts_data_v1, FALSE otherwise

## Examples

``` r
# Valid field types for csfmt_rts_data_v1
valid_fields <- c(
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
  "cases_n" = "INTEGER"
)
validator_field_types_csfmt_rts_data_v1(valid_fields)
#> [1] TRUE

# Invalid field types (wrong structure)
invalid_fields <- c("id" = "INTEGER", "name" = "TEXT")
validator_field_types_csfmt_rts_data_v1(invalid_fields)
#> [1] FALSE
```
