# The work behind SyntheticTable: the contract checks, the dataset hash and
# the catalogue of fixed datasets in a directory.

#' Build a SyntheticTable from a directory
#'
#' Reads `profile.yaml` from `dir` when it exists, and builds a
#' [SyntheticTable] whose datasets live in `dir`.
#'
#' @param dir The directory of the table. It holds `profile.yaml` (optional),
#'   `datasets.yaml` and `handmade/<name>.csv`.
#' @param name The name of the table.
#' @param field_types A named character vector of csdb field types.
#' @param keys A character vector of key column names, or `NULL`.
#' @param ... Other arguments to `SyntheticTable$new()`, such as
#'   `validator_field_contents`, `invariants` or `generator`. Do not give
#'   `profile` or `dir`.
#' @return A [SyntheticTable].
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' st <- synthetic_table_from_dir(dir, "people", c(id = "INTEGER"), "id")
#' st$list_datasets()
#' @md
#' @export
synthetic_table_from_dir <- function(dir, name, field_types, keys = NULL, ...) {
  if (!dir.exists(dir)) {
    stop("dir does not exist: ", dir, call. = FALSE)
  }
  path <- file.path(dir, "profile.yaml")
  profile <- NULL
  if (file.exists(path)) {
    profile <- read_profile(path)
  }
  return(SyntheticTable$new(
    name = name,
    field_types = field_types,
    keys = keys,
    profile = profile,
    dir = dir,
    ...
  ))
}

# spec ----

synthetic_spec <- function(
  name,
  field_types,
  keys,
  validator_field_contents,
  invariants,
  profile,
  generator,
  dir
) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("name must be one non-empty string.", call. = FALSE)
  }
  field_types <- profile_check_field_types(field_types)
  if (is.null(profile)) {
    profile <- profile_schema(field_types, keys)
  }
  if (is.null(generator)) {
    generator <- simulate_from_profile
  }
  synthetic_check_function(validator_field_contents, "validator_field_contents")
  synthetic_check_function(generator, "generator")
  return(list(
    name = name,
    field_types = field_types,
    keys = profile_check_keys(keys, field_types),
    validator_field_contents = validator_field_contents,
    invariants = synthetic_check_invariants(invariants),
    profile = profile,
    generator = generator,
    dir = synthetic_check_dir(dir)
  ))
}

synthetic_check_function <- function(f, what) {
  if (!is.function(f)) {
    stop(what, " must be a function.", call. = FALSE)
  }
  return(invisible(f))
}

synthetic_check_invariants <- function(invariants) {
  if (length(invariants) == 0) {
    return(list())
  }
  nms <- names(invariants)
  if (!is.list(invariants) || is.null(nms) || any(!nzchar(nms))) {
    stop("invariants must be a named list of functions.", call. = FALSE)
  }
  for (x in nms) {
    synthetic_check_function(invariants[[x]], paste0("invariant ", x))
  }
  return(invariants)
}

synthetic_check_dir <- function(dir) {
  if (is.null(dir)) {
    return(NULL)
  }
  if (!is.character(dir) || length(dir) != 1 || !nzchar(dir)) {
    stop("dir must be NULL or one non-empty string.", call. = FALSE)
  }
  return(dir)
}

synthetic_check_name <- function(name) {
  ok <- is.character(name) && length(name) == 1 && !is.na(name)
  if (!ok || !grepl("^[a-z][a-z0-9_]*$", name)) {
    stop(
      "A dataset name must be lower snake case: ",
      paste0(name, collapse = " "),
      call. = FALSE
    )
  }
  return(invisible(name))
}

synthetic_check_seed <- function(seed) {
  ok <- is.numeric(seed) && length(seed) == 1 && !is.na(seed)
  if (!ok || seed != round(seed) || abs(seed) > .Machine$integer.max) {
    stop("seed must be one whole number.", call. = FALSE)
  }
  return(as.integer(seed))
}

# validate ----

# Stops at the first check that `d` fails, and names that check. `label` names
# where `d` came from, or is NULL.
synthetic_validate <- function(spec, d, label = NULL) {
  fail <- function(check, detail) {
    stop(
      "SyntheticTable ",
      spec$name,
      if (!is.null(label)) paste0(", ", label),
      ": failed the ",
      check,
      " check. ",
      detail,
      call. = FALSE
    )
  }
  checks <- list(
    columns = synthetic_check_columns,
    types = synthetic_check_types,
    keys = synthetic_check_unique_keys,
    validator_field_contents = synthetic_check_contents
  )
  for (check in names(checks)) {
    detail <- checks[[check]](spec, d)
    if (!is.null(detail)) {
      fail(check, detail)
    }
  }
  for (x in names(spec$invariants)) {
    detail <- synthetic_check_invariant(spec$invariants[[x]], d)
    if (!is.null(detail)) {
      fail(paste0("invariant '", x, "'"), detail)
    }
  }
  return(invisible(d))
}

# Each check returns NULL when `d` passes, or the reason it fails.
synthetic_check_columns <- function(spec, d) {
  if (!data.table::is.data.table(d)) {
    return(paste0("The data is a ", class(d)[1], ", not a data.table."))
  }
  expected <- names(spec$field_types)
  if (!identical(names(d), expected)) {
    return(paste0(
      "The columns are (",
      paste0(names(d), collapse = ", "),
      "), not (",
      paste0(expected, collapse = ", "),
      ")."
    ))
  }
  return(NULL)
}

synthetic_type_ok <- list(
  TEXT = function(x) is.character(x),
  INTEGER = function(x) is.integer(x) && !is.object(x),
  DOUBLE = function(x) is.double(x) && !is.object(x),
  BOOLEAN = function(x) is.logical(x),
  DATE = function(x) inherits(x, "Date"),
  DATETIME = function(x) inherits(x, "POSIXct")
)

synthetic_check_types <- function(spec, d) {
  ft <- spec$field_types
  bad <- names(ft)[
    !vapply(
      names(ft),
      function(x) synthetic_type_ok[[ft[[x]]]](d[[x]]),
      logical(1)
    )
  ]
  if (length(bad) == 0) {
    return(NULL)
  }
  return(paste0(
    "These columns have the wrong R type: ",
    paste0(bad, " (", ft[bad], ")", collapse = ", "),
    "."
  ))
}

synthetic_check_unique_keys <- function(spec, d) {
  if (length(spec$keys) == 0 || anyDuplicated(d, by = spec$keys) == 0) {
    return(NULL)
  }
  return(paste0(
    "The rows are not unique on the keys ",
    paste0(spec$keys, collapse = ", "),
    "."
  ))
}

synthetic_check_contents <- function(spec, d) {
  result <- spec$validator_field_contents(d)
  if (isTRUE(result)) {
    return(NULL)
  }
  var <- attr(result, "var")
  if (is.null(var)) {
    return("validator_field_contents(d) did not return TRUE.")
  }
  return(paste0("validator_field_contents(d) failed on ", var, "."))
}

synthetic_check_invariant <- function(f, d) {
  result <- f(d)
  if (isTRUE(result)) {
    return(NULL)
  }
  if (is.character(result) && length(result) > 0) {
    return(paste0(result, collapse = " "))
  }
  return("The invariant did not return TRUE.")
}

# generate ----

synthetic_generate <- function(spec, n, seed, world, overrides = list()) {
  d <- spec$generator(spec$profile, n, seed, world)
  return(synthetic_apply_overrides(spec, d, overrides))
}

synthetic_random <- function(spec, n, seed, world) {
  n <- profile_check_count(n, "n", 1)
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
    message(
      "SyntheticTable ",
      spec$name,
      ": random() drew seed ",
      seed,
      ". Replay it with $random(n = ",
      n,
      ", seed = ",
      seed,
      ")."
    )
  }
  d <- synthetic_generate(spec, n, synthetic_check_seed(seed), world)
  return(synthetic_validate(spec, d, "random()"))
}

# The overrides, each converted to its column's type. Each is one value.
synthetic_check_overrides <- function(spec, overrides) {
  if (length(overrides) == 0) {
    return(list())
  }
  nms <- names(overrides)
  if (!is.list(overrides) || is.null(nms) || any(!nzchar(nms))) {
    stop("overrides must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(nms, names(spec$field_types))
  if (length(unknown) > 0) {
    stop(
      "overrides names unknown column(s): ",
      paste0(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  out <- profile_named(nms, function(x) {
    if (length(overrides[[x]]) != 1) {
      stop("The override of ", x, " must be one value.", call. = FALSE)
    }
    return(profile_as_type(overrides[[x]], spec$field_types[[x]]))
  })
  return(out)
}

synthetic_apply_overrides <- function(spec, d, overrides) {
  overrides <- synthetic_check_overrides(spec, overrides)
  if (length(overrides) == 0 || !data.table::is.data.table(d)) {
    return(d)
  }
  d <- data.table::copy(d)
  for (x in names(overrides)) {
    data.table::set(d, j = x, value = rep(overrides[[x]], nrow(d)))
  }
  return(d)
}

# hash ----

# The md5 of `d` written as CSV: columns in field_types order, then any other
# column, rows sorted by every column, dates in ISO 8601, NA empty, LF.
synthetic_hash <- function(d, field_types) {
  columns <- c(
    intersect(names(field_types), names(d)),
    setdiff(names(d), names(field_types))
  )
  x <- data.table::copy(data.table::as.data.table(d))
  data.table::setcolorder(x, columns)
  data.table::setorderv(x, columns)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(x, path, eol = "\n", na = "", dateTimeAs = "ISO")
  return(unname(tools::md5sum(path)))
}

# catalogue ----

synthetic_yaml_path <- function(spec) {
  return(file.path(spec$dir, "datasets.yaml"))
}

# The recipes in datasets.yaml, as a list of entries.
synthetic_read_recipes <- function(spec) {
  if (is.null(spec$dir) || !file.exists(synthetic_yaml_path(spec))) {
    return(list())
  }
  entries <- yaml::read_yaml(synthetic_yaml_path(spec))
  if (length(entries) == 0) {
    return(list())
  }
  names(entries) <- vapply(entries, function(e) e$name, character(1))
  return(entries)
}

synthetic_write_recipes <- function(spec, entries) {
  dir.create(spec$dir, recursive = TRUE, showWarnings = FALSE)
  text <- yaml::as.yaml(unname(entries), precision = 17)
  con <- file(synthetic_yaml_path(spec), open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con, sep = "", useBytes = TRUE)
  return(invisible(NULL))
}

# The handmade CSV files, named by dataset.
synthetic_handmade_paths <- function(spec) {
  if (is.null(spec$dir)) {
    return(character(0))
  }
  paths <- list.files(
    file.path(spec$dir, "handmade"),
    pattern = "\\.csv$",
    full.names = TRUE
  )
  names(paths) <- sub("\\.csv$", "", basename(paths))
  for (x in names(paths)) {
    synthetic_check_name(x)
  }
  return(paths)
}

synthetic_list <- function(spec) {
  recipes <- synthetic_read_recipes(spec)
  handmade <- names(synthetic_handmade_paths(spec))
  out <- data.table::data.table(
    name = c(names(recipes), handmade),
    kind = c(rep("recipe", length(recipes)), rep("handmade", length(handmade))),
    n = c(
      vapply(recipes, function(e) as.integer(e$n), 1L),
      rep(NA_integer_, length(handmade))
    ),
    seed = c(
      vapply(recipes, function(e) as.integer(e$seed), 1L),
      rep(NA_integer_, length(handmade))
    ),
    description = c(
      vapply(recipes, function(e) as.character(e$description), ""),
      rep(NA_character_, length(handmade))
    )
  )
  dup <- unique(out$name[duplicated(out$name)])
  if (length(dup) > 0) {
    stop(
      "SyntheticTable ",
      spec$name,
      " has a recipe and a handmade dataset named ",
      paste0(dup, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(out)
}

# A value for YAML: a date or a datetime as ISO 8601 text.
synthetic_encode_value <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  return(x)
}

synthetic_add <- function(spec, name, n, seed, description, overrides) {
  if (is.null(spec$dir)) {
    stop("add_dataset() needs a SyntheticTable with a dir.", call. = FALSE)
  }
  synthetic_check_name(name)
  n <- profile_check_count(n, "n", 1)
  seed <- synthetic_check_seed(seed)
  if (name == "default" && (n != 1000L || seed != 1L)) {
    stop("The dataset 'default' is n = 1000, seed = 1.", call. = FALSE)
  }
  if (name %in% synthetic_list(spec)$name) {
    stop(
      "SyntheticTable ",
      spec$name,
      " already has a dataset named ",
      name,
      ".",
      call. = FALSE
    )
  }
  overrides <- synthetic_check_overrides(spec, overrides)
  d <- synthetic_generate(spec, n, seed, NULL, overrides)
  synthetic_validate(spec, d, paste0("dataset '", name, "'"))
  entry <- list(
    name = name,
    kind = "recipe",
    n = n,
    seed = seed,
    description = as.character(description),
    overrides = lapply(overrides, synthetic_encode_value),
    md5 = synthetic_hash(d, spec$field_types)
  )
  recipes <- synthetic_read_recipes(spec)
  recipes[[name]] <- entry
  synthetic_write_recipes(spec, recipes)
  return(invisible(d))
}

# The recipe regenerated with world = NULL, and whether its hash matches.
synthetic_recipe_hash_ok <- function(spec, entry) {
  d <- synthetic_generate(spec, entry$n, entry$seed, NULL, entry$overrides)
  ok <- identical(synthetic_hash(d, spec$field_types), entry$md5)
  return(list(d = d, ok = ok))
}

synthetic_dataset <- function(spec, name, world) {
  label <- paste0("dataset '", name, "'")
  handmade <- synthetic_handmade_paths(spec)
  if (name %in% names(handmade)) {
    d <- synthetic_read_csv(handmade[[name]], spec$field_types)
    return(synthetic_validate(spec, d, label))
  }
  entry <- synthetic_read_recipes(spec)[[name]]
  if (is.null(entry)) {
    stop(
      "SyntheticTable ",
      spec$name,
      " has no dataset named ",
      name,
      ".",
      call. = FALSE
    )
  }
  if (!is.null(world)) {
    message(
      "SyntheticTable ",
      spec$name,
      ", ",
      label,
      ": world is not NULL, so the hash is not checked."
    )
    d <- synthetic_generate(spec, entry$n, entry$seed, world, entry$overrides)
    return(synthetic_validate(spec, d, label))
  }
  result <- synthetic_recipe_hash_ok(spec, entry)
  if (!result$ok) {
    stop(synthetic_hash_error(spec, name), call. = FALSE)
  }
  return(synthetic_validate(spec, result$d, label))
}

synthetic_hash_error <- function(spec, name) {
  return(paste0(
    "SyntheticTable ",
    spec$name,
    ", dataset '",
    name,
    "': the regenerated data does not match the md5 in datasets.yaml. ",
    "The generator, the profile or the overrides changed."
  ))
}

synthetic_check_hashes <- function(spec) {
  recipes <- synthetic_read_recipes(spec)
  pass <- vapply(
    recipes,
    function(e) synthetic_recipe_hash_ok(spec, e)$ok,
    logical(1)
  )
  return(data.table::data.table(
    name = names(recipes),
    pass = unname(pass)
  ))
}

# A handmade CSV with each column converted to its field type. Every column
# is read as text, and an empty cell is NA.
synthetic_read_csv <- function(path, field_types) {
  d <- data.table::fread(
    path,
    colClasses = "character",
    na.strings = "",
    encoding = "UTF-8"
  )
  for (x in intersect(names(field_types), names(d))) {
    data.table::set(d, j = x, value = profile_as_type(d[[x]], field_types[[x]]))
  }
  return(d)
}
