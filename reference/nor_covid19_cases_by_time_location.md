# Covid-19 data for PCR-confirmed cases in Norway (nation and county)

This data comes from the Norwegian Surveillance System for Communicable
Diseases (MSIS). The date corresponds to when the PCR-test was taken.

## Usage

``` r
nor_covid19_cases_by_time_location
```

## Format

A csfmt_rts_data_v1 with 11028 rows and 18 variables:

- granularity_time:

  day/isoweek

- granularity_geo:

  nation, county

- country_iso3:

  nor

- location_code:

  norge, 11 counties

- border:

  2020

- age:

  total

- isoyear:

  Isoyear of event

- isoweek:

  Isoweek of event

- isoyearweek:

  Isoyearweek of event

- season:

  Season of event

- seasonweek:

  Seasonweek of event

- calyear:

  Calyear of event

- calmonth:

  Calmonth of event

- calyearmonth:

  Calyearmonth of event

- date:

  Date of event

- covid19_cases_testdate_n:

  Number of confirmed covid19 cases

- covid19_cases_testdate_pr100000:

  Number of confirmed covid19 cases per 100.000 population

## Source

<https://github.com/folkehelseinstituttet/surveillance_data/blob/master/covid19/_DOCUMENTATION_data_covid19_msis_by_time_location.txt>

## Details

The raw number of cases and cases per 100.000 population are recorded.

This data was extracted on 2022-05-04.

## Examples

``` r
head(nor_covid19_cases_by_time_location)
#>    granularity_time granularity_geo country_iso3 location_code border    age
#>              <char>          <char>       <char>        <char>  <int> <char>
#> 1:              day          county          nor  county_nor03   2020  total
#> 2:              day          county          nor  county_nor03   2020  total
#> 3:              day          county          nor  county_nor03   2020  total
#> 4:              day          county          nor  county_nor03   2020  total
#> 5:              day          county          nor  county_nor03   2020  total
#> 6:              day          county          nor  county_nor03   2020  total
#>       sex isoyear isoweek isoyearweek    season seasonweek calyear calmonth
#>    <char>   <int>   <int>      <char>    <char>      <num>   <int>    <int>
#> 1:  total    2020       8     2020-08 2019/2020         31    2020        2
#> 2:  total    2020       8     2020-08 2019/2020         31    2020        2
#> 3:  total    2020       8     2020-08 2019/2020         31    2020        2
#> 4:  total    2020       9     2020-09 2019/2020         32    2020        2
#> 5:  total    2020       9     2020-09 2019/2020         32    2020        2
#> 6:  total    2020       9     2020-09 2019/2020         32    2020        2
#>    calyearmonth       date covid19_cases_testdate_n
#>          <char>     <Date>                    <int>
#> 1:     2020-M02 2020-02-21                        0
#> 2:     2020-M02 2020-02-22                        0
#> 3:     2020-M02 2020-02-23                        0
#> 4:     2020-M02 2020-02-24                        0
#> 5:     2020-M02 2020-02-25                        0
#> 6:     2020-M02 2020-02-26                        2
#>    covid19_cases_testdate_pr100000
#>                              <num>
#> 1:                       0.0000000
#> 2:                       0.0000000
#> 3:                       0.0000000
#> 4:                       0.0000000
#> 5:                       0.0000000
#> 6:                       0.2883947
nrow(nor_covid19_cases_by_time_location)
#> [1] 11028
```
