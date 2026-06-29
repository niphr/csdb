# Field contents validator for csfmt_rts_data_v2 schema

Validates that data contents conform to the csfmt_rts_data_v2 schema
specification. This validator checks that granularity_time and
granularity_geo fields contain valid values according to the
surveillance data format requirements for version 2.

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

## Examples

``` r
# Valid data for csfmt_rts_data_v2
valid_data_v2 <- data.frame(
  granularity_time = c("date", "isoyearweek", "total"),
  granularity_geo = c("nation", "county", "municip"),
  stringsAsFactors = FALSE
)
validator_field_contents_csfmt_rts_data_v2(valid_data_v2)
#> [1] FALSE
#> attr(,"var")
#> [1] "date"
```
