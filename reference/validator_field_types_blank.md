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
