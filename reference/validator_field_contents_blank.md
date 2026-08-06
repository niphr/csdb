# Blank data contents validator

A pass-through validator that accepts any data without validation. Use
it as a placeholder when you need no check on the data contents.

## Usage

``` r
validator_field_contents_blank(data)
```

## Arguments

- data:

  A data.frame or data.table containing the data to validate.

## Value

Always returns TRUE.

## See also

The introduction vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
which passes this to `DBTable_v9$new()` as its
`validator_field_contents` argument.
[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md)
calls the field contents validator from its `insert_data()` and
`upsert_data()` methods.

Other field contents validators:
[`validator_field_contents_csfmt_rts_data_v1()`](https://niphr.github.io/csdb/reference/validator_field_contents_csfmt_rts_data_v1.md),
[`validator_field_contents_csfmt_rts_data_v2()`](https://niphr.github.io/csdb/reference/validator_field_contents_csfmt_rts_data_v2.md)

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
