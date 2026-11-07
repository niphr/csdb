#' The synthetic data contract of one table
#'
#' A `SyntheticTable` holds the contract of one table and its fixed synthetic
#' datasets. Every data.table that a `SyntheticTable` returns passed
#' `$validate()`.
#'
#' @details
#' A generator is a `function(profile, n, seed, world)` that returns a
#' data.table. `generator = NULL` uses [simulate_from_profile()] on `profile`,
#' or on [profile_schema()] of `field_types` and `keys` when `profile` is
#' `NULL`.
#'
#' `$validate(d)` checks `d` in this order, and stops at the first check that
#' fails. The error names the check.
#'
#' 1. `columns`: `d` is a data.table, and its column names and order equal
#'    `names(field_types)`.
#' 2. `types`: each column has the R type of its field type. These are
#'    `TEXT` character, `INTEGER` integer, `DOUBLE` double, `BOOLEAN`
#'    logical, `DATE` Date and `DATETIME` POSIXct.
#' 3. `keys`: no two rows have the same values of `keys`.
#' 4. `validator_field_contents`: `validator_field_contents(d)` is `TRUE`.
#' 5. `invariant '<name>'`: each invariant returns `TRUE`. A character
#'    return is the error message.
#'
#' The directory `dir` holds the datasets of the table:
#'
#' - `datasets.yaml`: one entry for each recipe, with `name`, `kind`, `n`,
#'   `seed`, `description`, `overrides` and `md5`.
#' - `handmade/<name>.csv`: one handmade dataset. Each column is read as
#'   text and converted to its field type. An empty cell is `NA`.
#' - `profile.yaml`: optional. [synthetic_table_from_dir()] reads it.
#'   `$new()` does not read it.
#'
#' A dataset name is lower snake case. The name `default` is the generator
#' output at `n = 1000` and `seed = 1`.
#'
#' The md5 of a recipe comes from [tools::md5sum()] of the data that
#' [data.table::fwrite()] writes with `eol = "\n"`. The columns are in
#' `field_types` order, the rows are sorted by every column, dates are
#' ISO 8601 and `NA` is empty.
#'
#' `$dataset()` regenerates a recipe with `world = NULL`, and stops with an
#' error that names the dataset when the md5 does not match. A non-`NULL`
#' `world` is the one exception. The recipe is then generated with `world`
#' and validated, but its md5 is not checked, and a message says so.
#'
#' @examples
#' st <- SyntheticTable$new(
#'   name = "people",
#'   field_types = c(id = "INTEGER", sex = "TEXT"),
#'   keys = "id",
#'   invariants = list(
#'     sex_known = function(d) if (all(!is.na(d$sex))) TRUE else "sex is missing."
#'   )
#' )
#' st$random(n = 5, seed = 1)
#' @md
#' @export
SyntheticTable <- R6::R6Class(
  "SyntheticTable",
  public = list(
    #' @description
    #' Create a new SyntheticTable.
    #' @param name The name of the table.
    #' @param field_types A named character vector of csdb field types.
    #' @param keys A character vector of key column names, or `NULL`.
    #' @param validator_field_contents A function of a data.table that
    #'   returns `TRUE` for valid contents.
    #' @param invariants A named list of `function(d)`. Each returns `TRUE`,
    #'   or a message that says why `d` fails.
    #' @param profile A `csdb_profile`, or `NULL`.
    #' @param generator A `function(profile, n, seed, world)` that returns a
    #'   data.table, or `NULL` for [simulate_from_profile()].
    #' @param dir The directory of the datasets, or `NULL` for no datasets.
    initialize = function(
      name,
      field_types,
      keys = NULL,
      validator_field_contents = validator_field_contents_blank,
      invariants = list(),
      profile = NULL,
      generator = NULL,
      dir = NULL
    ) {
      private$spec <- synthetic_spec(
        name = name,
        field_types = field_types,
        keys = keys,
        validator_field_contents = validator_field_contents,
        invariants = invariants,
        profile = profile,
        generator = generator,
        dir = dir
      )
    },

    #' @description
    #' Check a data.table against the contract.
    #' @param d A data.table.
    #' @return `d`, invisibly. The method stops at the first check that
    #'   fails, and the error names it.
    validate = function(d) {
      return(synthetic_validate(private$spec, d))
    },

    #' @description
    #' Generate and validate new data.
    #' @param n The number of rows.
    #' @param seed The seed. `NULL` draws a seed and reports it in a
    #'   message, so you can replay a failure.
    #' @param world `NULL`, or a list that the generator receives.
    #' @return A validated data.table.
    random = function(n = 1000L, seed = NULL, world = NULL) {
      return(synthetic_random(private$spec, n, seed, world))
    },

    #' @description
    #' Get a fixed dataset by name.
    #' @param name The name of the dataset.
    #' @param world `NULL`, or a list that the generator receives. A
    #'   non-`NULL` `world` turns off the md5 check.
    #' @return A validated data.table.
    dataset = function(name, world = NULL) {
      return(synthetic_dataset(private$spec, name, world))
    },

    #' @description
    #' List the fixed datasets.
    #' @return A data.table with `name`, `kind` (`recipe` or `handmade`),
    #'   `n`, `seed` and `description`. A handmade dataset has `NA` in `n`,
    #'   `seed` and `description`.
    list_datasets = function() {
      return(synthetic_list(private$spec))
    },

    #' @description
    #' Add a recipe and its md5 to `datasets.yaml`.
    #' @param name The name of the dataset, lower snake case and new.
    #' @param n The number of rows.
    #' @param seed The seed.
    #' @param description One string that says what the dataset is for.
    #' @param overrides A named list that maps a column to one fixed value.
    #'   The value replaces the column after generation.
    #' @return The validated data.table, invisibly.
    add_dataset = function(name, n, seed, description, overrides = list()) {
      return(synthetic_add(private$spec, name, n, seed, description, overrides))
    },

    #' @description
    #' Regenerate every recipe with `world = NULL` and compare its md5.
    #' @return A data.table with `name` and `pass`.
    check_hashes = function() {
      return(synthetic_check_hashes(private$spec))
    }
  ),
  active = list(
    #' @field name The name of the table.
    name = function() private$spec$name,
    #' @field field_types The field types of the table.
    field_types = function() private$spec$field_types,
    #' @field keys The key columns of the table.
    keys = function() private$spec$keys,
    #' @field dir The directory of the datasets, or `NULL`.
    dir = function() private$spec$dir
  ),
  private = list(
    spec = NULL
  )
)
