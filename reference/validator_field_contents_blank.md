# Blank data contents validator

A pass-through validator that accepts any data without validation. This
is useful as a placeholder when no specific data content validation is
needed.

## Usage

``` r
validator_field_contents_blank(data)
```

## Arguments

- data:

  A data.frame or data.table containing the data to validate

## Value

Always returns TRUE

## Examples

``` r
# This validator always returns TRUE regardless of input
test_data <- data.frame(id = 1:3, name = c("A", "B", "C"), value = c(10, 20, 30))
validator_field_contents_blank(test_data)
#> [1] TRUE

# Works with any data structure
empty_data <- data.frame()
validator_field_contents_blank(empty_data)
#> [1] TRUE
```
