# Field types validator for csfmt_rts_data_v2 schema

Validates that field types conform to the csfmt_rts_data_v2 schema
specification. This validator ensures that the first 18 fields match the
expected structure for real-time surveillance data format version 2.

## Usage

``` r
validator_field_types_csfmt_rts_data_v2(db_field_types)
```

## Arguments

- db_field_types:

  A named character vector of database field types

## Value

TRUE if field types are valid for csfmt_rts_data_v2, FALSE otherwise

## Examples

``` r
# Valid field types for csfmt_rts_data_v2 (includes additional fields)
valid_fields_v2 <- c(
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
  "tag_outcome" = "TEXT",
  "tag_type" = "TEXT",
  "cases_n" = "INTEGER"
)
validator_field_types_csfmt_rts_data_v2(valid_fields_v2)
#> [1] FALSE
```
