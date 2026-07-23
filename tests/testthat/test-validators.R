# The field-type and field-contents validators are the part of csdb that runs
# without a database, and they are the gate every table write passes through.

test_that("the blank validators accept anything", {
  expect_true(validator_field_types_blank(c(x = "TEXT")))
  expect_true(validator_field_types_blank(NULL))
  expect_true(validator_field_contents_blank(data.frame(x = 1)))
  expect_true(validator_field_contents_blank(NULL))
})

# --- field types -------------------------------------------------------------

csfmt_v1_types <- function() {
  c(
    "granularity_time" = "TEXT", "granularity_geo" = "TEXT",
    "country_iso3" = "TEXT", "location_code" = "TEXT",
    "border" = "INTEGER", "age" = "TEXT", "sex" = "TEXT",
    "isoyear" = "INTEGER", "isoweek" = "INTEGER", "isoyearweek" = "TEXT",
    "season" = "TEXT", "seasonweek" = "DOUBLE",
    "calyear" = "INTEGER", "calmonth" = "INTEGER", "calyearmonth" = "TEXT",
    "date" = "DATE"
  )
}

test_that("the csfmt_rts_data_v1 field types are accepted", {
  expect_true(validator_field_types_csfmt_rts_data_v1(csfmt_v1_types()))
})

test_that("extra trailing fields are allowed after the required 16", {
  # only the first 16 are prescribed; a table may carry its own measures after
  expect_true(validator_field_types_csfmt_rts_data_v1(
    c(csfmt_v1_types(), "deaths_n" = "INTEGER")
  ))
})

test_that("a non-character argument is rejected rather than erroring", {
  expect_false(validator_field_types_csfmt_rts_data_v1(list(a = "TEXT")))
  expect_false(validator_field_types_csfmt_rts_data_v1(1:16))
})

test_that("too few fields are rejected", {
  expect_false(validator_field_types_csfmt_rts_data_v1(csfmt_v1_types()[1:15]))
  expect_false(validator_field_types_csfmt_rts_data_v1(character()))
})

test_that("a wrong type in a required field is rejected", {
  bad <- csfmt_v1_types()
  bad[["border"]] <- "TEXT"          # prescribed as INTEGER
  expect_false(validator_field_types_csfmt_rts_data_v1(bad))
})

test_that("the required fields are order-sensitive", {
  swapped <- csfmt_v1_types()
  swapped[1:2] <- swapped[2:1]
  names(swapped)[1:2] <- names(csfmt_v1_types())[2:1]
  expect_false(validator_field_types_csfmt_rts_data_v1(swapped))
})

# --- field contents ----------------------------------------------------------

valid_contents <- function(...) {
  d <- data.frame(
    granularity_time = c("date", "isoyearweek", "total"),
    granularity_geo = c("nation", "county", "municip"),
    border = c("2020", "2020", "2020"),
    sex = c("total", "total", "total"),
    date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-01")),
    stringsAsFactors = FALSE
  )
  ow <- list(...)
  for (nm in names(ow)) d[[nm]] <- ow[[nm]]
  d
}

test_that("well-formed contents validate", {
  expect_true(validator_field_contents_csfmt_rts_data_v1(valid_contents()))
})

test_that("an unknown granularity_time is rejected and names the field", {
  bad <- validator_field_contents_csfmt_rts_data_v1(
    valid_contents(granularity_time = c("fortnight", "fortnight", "fortnight"))
  )
  expect_false(bad)
  expect_equal(attr(bad, "var"), "granularity_time")
})

test_that("an unknown granularity_geo is rejected and names the field", {
  bad <- validator_field_contents_csfmt_rts_data_v1(
    valid_contents(granularity_geo = c("parish", "parish", "parish"))
  )
  expect_false(bad)
  expect_equal(attr(bad, "var"), "granularity_geo")
})

test_that("an event granularity_time is accepted (anchored ^event)", {
  expect_true(validator_field_contents_csfmt_rts_data_v1(
    valid_contents(granularity_time = c("event", "event", "event"))
  ))
})
