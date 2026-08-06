# Field types validator for csfmt_rts_data_v1 schema

Checks that field types conform to the csfmt_rts_data_v1 schema
specification. The validator checks the first 16 entries of
`db_field_types` against the expected structure of that schema.

## Usage

``` r
validator_field_types_csfmt_rts_data_v1(db_field_types)
```

## Arguments

- db_field_types:

  A named character vector of database field types.

## Value

TRUE if field types are valid for csfmt_rts_data_v1, FALSE otherwise.

## See also

[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
which takes this as its `validator_field_types` argument and calls it
once, while the object is being created. The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not use this validator.

Other field type validators:
[`validator_field_types_blank()`](https://niphr.github.io/csdb/reference/validator_field_types_blank.md),
[`validator_field_types_csfmt_rts_data_v2()`](https://niphr.github.io/csdb/reference/validator_field_types_csfmt_rts_data_v2.md)

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
