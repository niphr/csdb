# Profiles that hold no data, and the values that simulate_from_profile()
# draws for a column the profile does not summarise.

#' A profile that holds only a schema
#'
#' Builds a `csdb_profile` in which every column is `schema_only`. Use it with
#' [simulate_from_profile()] for a table that has no real profile.
#'
#' @param field_types A named character vector of csdb field types: `TEXT`,
#'   `INTEGER`, `DOUBLE`, `BOOLEAN`, `DATE` or `DATETIME`.
#' @param keys A character vector of key column names, or `NULL`.
#' @return A `csdb_profile`. Each column has `type`, `sensitivity`
#'   `"schema_only"` and `missing_share` 0. `n_rows` is 0, and
#'   `provenance$source` is `"schema"`.
#' @family simulation functions
#' @examples
#' p <- profile_schema(c(id = "INTEGER", sex = "TEXT"), keys = "id")
#' simulate_from_profile(p, n = 5)
#' @export
profile_schema <- function(field_types, keys = NULL) {
  field_types <- profile_check_field_types(field_types)
  columns <- profile_named(names(field_types), function(x) {
    return(list(
      type = field_types[[x]],
      sensitivity = "schema_only",
      missing_share = 0
    ))
  })
  provenance <- list(
    format = 1L,
    source = "schema",
    query = NULL,
    driver = NULL,
    profiled_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    k = NULL,
    noise = NULL,
    max_levels = NULL,
    csdb_version = as.character(utils::packageVersion("csdb"))
  )
  return(new_profile(
    provenance = provenance,
    field_types = field_types,
    keys = profile_check_keys(keys, field_types),
    n_rows = 0L,
    columns = columns,
    group_sizes = list(),
    crosstabs = list(),
    dependencies = list(),
    notes = character(0)
  ))
}

# The first and the last day that a schema-only DATE or DATETIME can take.
simulate_schema_days <- as.Date(c("2020-01-01", "2024-12-31"))

# Values for the rows `idx` of a column with no summary. A key column takes
# its row number, so a single-column key is unique at any n.
simulate_schema_column <- function(column, type, idx, is_key) {
  if (is_key && type %in% c("TEXT", "INTEGER")) {
    return(simulate_index_values(column, type, idx))
  }
  m <- length(idx)
  return(switch(
    type,
    TEXT = paste0(column, "_", sample.int(10L, m, replace = TRUE)),
    INTEGER = sample(0:100, m, replace = TRUE),
    DOUBLE = runif(m, 0, 1),
    BOOLEAN = sample(c(TRUE, FALSE), m, replace = TRUE),
    DATE = simulate_uniform_days(m),
    DATETIME = simulate_datetime(simulate_uniform_days(m))
  ))
}

# "<column>_<i>" for TEXT, i for INTEGER, and the number i for DOUBLE.
simulate_index_values <- function(column, type, i) {
  return(switch(
    type,
    TEXT = paste0(column, "_", i),
    INTEGER = as.integer(i),
    DOUBLE = as.numeric(i),
    stop(
      "Column ",
      column,
      " of type ",
      type,
      " cannot hold generated identifiers.",
      call. = FALSE
    )
  ))
}

simulate_uniform_days <- function(m) {
  span <- as.numeric(simulate_schema_days)
  return(.Date(span[1] + sample.int(span[2] - span[1] + 1L, m, TRUE) - 1))
}

# A POSIXct in UTC at a uniform whole second within each day of `days`.
simulate_datetime <- function(days) {
  seconds <- floor(runif(length(days), 0, 86400))
  return(.POSIXct(as.numeric(days) * 86400 + seconds, tz = "UTC"))
}
