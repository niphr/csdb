# Blank field types validator

A pass-through validator that accepts any field types without
validation. This is useful as a placeholder when no specific field type
validation is needed.

## Usage

``` r
validator_field_types_blank(db_field_types)
```

## Arguments

- db_field_types:

  A named character vector of database field types

## Value

Always returns TRUE

## See also

The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
which passes this to `DBTable_v9$new()` as its `validator_field_types`
argument.
[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md)
calls the field type validator once, while the object is being created.

Other field type validators:
[`validator_field_types_csfmt_rts_data_v1()`](https://niphr.github.io/csdb/reference/validator_field_types_csfmt_rts_data_v1.md),
[`validator_field_types_csfmt_rts_data_v2()`](https://niphr.github.io/csdb/reference/validator_field_types_csfmt_rts_data_v2.md)

## Examples

``` r
# This validator always returns TRUE regardless of input
field_types <- c("id" = "INTEGER", "name" = "TEXT", "date" = "DATE")
validator_field_types_blank(field_types)
#> [1] TRUE

# Works with any field types
other_types <- c("value" = "DOUBLE", "status" = "BOOLEAN")
validator_field_types_blank(other_types)
#> [1] TRUE
```
