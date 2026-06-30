# Field contents validator for csfmt_rts_data_v1 schema

Validates that data contents conform to the csfmt_rts_data_v1 schema
specification. This validator checks that granularity_time,
granularity_geo, border, sex, and date fields contain valid values
according to the surveillance data format requirements.

## Usage

``` r
validator_field_contents_csfmt_rts_data_v1(data)
```

## Arguments

- data:

  A data.frame or data.table containing the data to validate

## Value

TRUE if data is valid for csfmt_rts_data_v1, FALSE otherwise (with error
attribute)

## Examples

``` r
# Valid data for csfmt_rts_data_v1 (all required columns present)
valid_data <- data.frame(
  granularity_time = c("date", "isoyearweek", "total"),
  granularity_geo = c("nation", "county", "municip"),
  border = c("2020", "2020", "2020"),
  sex = c("total", "total", "total"),
  date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01")),
  stringsAsFactors = FALSE
)
validator_field_contents_csfmt_rts_data_v1(valid_data)
#> [1] TRUE

# Invalid data (unrecognised granularity_geo value)
invalid_data <- data.frame(
  granularity_time = "date",
  granularity_geo = "invalid_geo",
  border = "2020",
  sex = "total",
  date = as.Date("2020-01-01"),
  stringsAsFactors = FALSE
)
validator_field_contents_csfmt_rts_data_v1(invalid_data)
#> [1] FALSE
#> attr(,"var")
#> [1] "granularity_geo"
```
