# Field contents validator for csfmt_rts_data_v2 schema

Validates that data contents conform to the csfmt_rts_data_v2 schema
specification. This validator checks that granularity_time,
granularity_geo, border, sex, and date fields contain valid values
according to the surveillance data format requirements for version 2.

## Usage

``` r
validator_field_contents_csfmt_rts_data_v2(data)
```

## Arguments

- data:

  A data.frame or data.table containing the data to validate

## Value

TRUE if data is valid for csfmt_rts_data_v2, FALSE otherwise (with error
attribute)

## See also

[`DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.md),
which takes this as its `validator_field_contents` argument and calls it
from its `insert_data()` and `upsert_data()` methods. The introduction
vignette,
[`vignette("csdb", package = "csdb")`](https://niphr.github.io/csdb/articles/csdb.md),
does not use this validator.

Other field contents validators:
[`validator_field_contents_blank()`](https://niphr.github.io/csdb/reference/validator_field_contents_blank.md),
[`validator_field_contents_csfmt_rts_data_v1()`](https://niphr.github.io/csdb/reference/validator_field_contents_csfmt_rts_data_v1.md)

## Examples

``` r
# Valid data for csfmt_rts_data_v2 (all required columns present)
valid_data_v2 <- data.frame(
  granularity_time = c("date", "isoyearweek", "total"),
  granularity_geo = c("nation", "county", "municip"),
  border = c("2020", "2020", "2020"),
  sex = c("total", "total", "total"),
  date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01")),
  stringsAsFactors = FALSE
)
validator_field_contents_csfmt_rts_data_v2(valid_data_v2)
#> [1] TRUE

# Invalid data (unrecognised granularity_geo value)
invalid_data_v2 <- data.frame(
  granularity_time = "date",
  granularity_geo = "invalid_geo",
  border = "2020",
  sex = "total",
  date = as.Date("2020-01-01"),
  stringsAsFactors = FALSE
)
validator_field_contents_csfmt_rts_data_v2(invalid_data_v2)
#> [1] FALSE
#> attr(,"var")
#> [1] "granularity_geo"
```
