# Field types validator for csfmt_rts_data_v2 schema

Checks that field types conform to the csfmt_rts_data_v2 schema
specification. The validator checks the first 18 entries of
`db_field_types` against the expected structure of that schema.

## Usage

``` r
validator_field_types_csfmt_rts_data_v2(db_field_types)
```

## Arguments

- db_field_types:

  A named character vector of database field types.

## Value

TRUE if field types are valid for csfmt_rts_data_v2, FALSE otherwise.

## See also

[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
which takes this as its `validator_field_types` argument and calls it
once, while the object is being created. The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not use this validator.

Other field type validators:
[`validator_field_types_blank()`](https://niphr.github.io/csdb/reference/validator_field_types_blank.md),
[`validator_field_types_csfmt_rts_data_v1()`](https://niphr.github.io/csdb/reference/validator_field_types_csfmt_rts_data_v1.md)

## Examples

``` r
# Valid field types for csfmt_rts_data_v2. The first 18 must match the
# schema, which unlike v1 carries isoquarter and isoyearquarter.
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
  "isoquarter" = "INTEGER",
  "isoyearquarter" = "TEXT",
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
#> [1] TRUE

# The v1 layout is not valid for v2: it has no isoquarter
validator_field_types_csfmt_rts_data_v2(valid_fields_v2[-c(11, 12)])
#> [1] FALSE
```
