# YAML encoding of a csdb_profile.
#
# A plain yaml round trip loses a Date (it comes back numeric), names,
# character(0) (it comes back as list()), the class, and digits past the
# seventh. So the writer encodes explicitly and the reader decodes each field
# by its name and by its column's type. Each value of `levels`, `days`,
# `weeks` and `breaks` is written as text.

#' Write a profile to a YAML file
#'
#' Writes a `csdb_profile` as YAML. [read_profile()] reads the file back to
#' an object identical to `profile`.
#'
#' @param profile A `csdb_profile`, from [profile_query()].
#' @param path The file to write. The function overwrites it.
#' @return `path`, invisibly.
#' @family profile functions
#' @examples
#' d <- data.frame(sex = rep(c("female", "male"), 6))
#' p <- profile_data(d, c(sex = "TEXT"), columns = c(sex = "full"))
#' f <- write_profile(p, tempfile(fileext = ".yaml"))
#' cat(readLines(f), sep = "\n")
#' @export
write_profile <- function(profile, path) {
  if (!inherits(profile, "csdb_profile")) {
    stop("profile must be a csdb_profile.", call. = FALSE)
  }
  text <- yaml::as.yaml(profile_encode(profile), precision = 17)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con, sep = "", useBytes = TRUE)
  return(invisible(path))
}

#' Read a profile from a YAML file
#'
#' Reads a file that [write_profile()] wrote. The function decodes each value
#' by its field name and by the `type` of its column.
#'
#' @param path The YAML file to read.
#' @return A `csdb_profile`. `identical(read_profile(write_profile(p, f)), p)`
#'   is `TRUE`.
#' @family profile functions
#' @examples
#' d <- data.frame(day = as.Date("2024-01-01") + rep(0:1, each = 6))
#' p <- profile_data(d, c(day = "DATE"), columns = c(day = "full"))
#' f <- write_profile(p, tempfile(fileext = ".yaml"))
#' identical(read_profile(f), p)
#' @export
read_profile <- function(path) {
  text <- readLines(path, encoding = "UTF-8", warn = FALSE)
  return(profile_decode(yaml::yaml.load(paste0(text, collapse = "\n"))))
}

# encode ----

profile_encode <- function(profile) {
  return(list(
    provenance = profile$provenance,
    field_types = as.list(profile$field_types),
    keys = profile$keys,
    n_rows = profile$n_rows,
    columns = lapply(profile$columns, profile_encode_column),
    group_sizes = profile$group_sizes,
    crosstabs = lapply(
      profile$crosstabs,
      profile_encode_crosstab,
      field_types = profile$field_types
    ),
    dependencies = profile$dependencies,
    notes = profile$notes
  ))
}

# The field type in which a value field of a column is written.
profile_value_type <- function(name, type) {
  return(switch(
    name,
    levels = type,
    days = "DATE",
    weeks = "DATE",
    breaks = "DOUBLE",
    NULL
  ))
}

profile_encode_column <- function(entry) {
  for (name in names(entry)) {
    type <- profile_value_type(name, entry$type)
    if (!is.null(type)) {
      entry[[name]] <- profile_encode_values(entry[[name]], type)
    }
  }
  return(entry)
}

profile_encode_crosstab <- function(entry, field_types) {
  for (name in names(entry$levels)) {
    entry$levels[[name]] <- profile_encode_values(
      entry$levels[[name]],
      field_types[[name]]
    )
  }
  return(entry)
}

profile_encode_values <- function(x, type) {
  return(switch(
    type,
    TEXT = as.character(x),
    INTEGER = as.character(x),
    DOUBLE = profile_format_double(x),
    BOOLEAN = as.character(x),
    DATE = format(x, "%Y-%m-%d"),
    DATETIME = format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))
}

# The shortest of 15 or 17 significant digits that gives back the same double.
profile_format_double <- function(x) {
  s <- sprintf("%.15g", x)
  lossy <- as.numeric(s) != x
  s[lossy] <- sprintf("%.17g", x[lossy])
  return(s)
}

# decode ----

profile_decode <- function(x) {
  provenance <- profile_decode_provenance(x$provenance)
  field_types <- unlist(x$field_types)
  return(new_profile(
    provenance = provenance,
    field_types = field_types,
    keys = profile_decode_text(x$keys),
    n_rows = as.integer(x$n_rows),
    columns = profile_named(
      names(x$columns),
      function(name) profile_decode_column(x$columns[[name]])
    ),
    group_sizes = profile_named(
      names(x$group_sizes),
      function(name) profile_decode_group_size(x$group_sizes[[name]])
    ),
    crosstabs = lapply(
      x$crosstabs,
      profile_decode_crosstab,
      field_types = field_types
    ),
    dependencies = lapply(x$dependencies, profile_decode_dependency),
    notes = profile_decode_text(x$notes)
  ))
}

profile_decode_provenance <- function(x) {
  if (!identical(x$format, 1L)) {
    stop("This file is not a csdb profile of format 1.", call. = FALSE)
  }
  noise <- NULL
  if (!is.null(x$noise)) {
    noise <- list(epsilon = as.numeric(x$noise$epsilon))
  }
  return(list(
    format = 1L,
    source = profile_decode_optional_text(x$source),
    query = as.character(x$query),
    driver = as.character(x$driver),
    profiled_at = as.character(x$profiled_at),
    k = as.integer(x$k),
    noise = noise,
    max_levels = as.integer(x$max_levels),
    csdb_version = as.character(x$csdb_version)
  ))
}

profile_decode_text <- function(x) {
  return(as.character(unlist(x)))
}

profile_decode_optional_text <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  return(as.character(x))
}

profile_decode_count <- function(x) {
  return(as.integer(unlist(x)))
}

profile_decode_values <- function(x, type) {
  return(profile_as_type(profile_decode_text(x), type))
}

profile_decode_column <- function(entry) {
  type <- as.character(entry$type)
  return(profile_named(
    names(entry),
    function(name) profile_decode_field(name, entry[[name]], type)
  ))
}

profile_decode_field <- function(name, x, type) {
  value_type <- profile_value_type(name, type)
  if (!is.null(value_type)) {
    return(profile_decode_values(x, value_type))
  }
  if (name == "missing_share" && !is.character(x)) {
    return(as.numeric(x))
  }
  if (name %in% c("n", "n_distinct", "suppressed_n")) {
    return(profile_decode_count(x))
  }
  if (name %in% c("type", "sensitivity", "missing_share")) {
    return(as.character(x))
  }
  stop("Unknown field in a profile column: ", name, call. = FALSE)
}

# Copy suppressed_n from `x` to `out` when `x` holds it.
profile_decode_suppressed <- function(out, x) {
  if (!is.null(x$suppressed_n)) {
    out$suppressed_n <- as.integer(x$suppressed_n)
  }
  return(out)
}

profile_decode_group_size <- function(x) {
  out <- list(
    column = as.character(x$column),
    size = profile_decode_count(x$size),
    n = profile_decode_count(x$n)
  )
  return(profile_decode_suppressed(out, x))
}

profile_decode_crosstab <- function(x, field_types) {
  levels <- profile_named(
    names(x$levels),
    function(name) profile_decode_values(x$levels[[name]], field_types[[name]])
  )
  out <- list(
    columns = profile_decode_text(x$columns),
    levels = levels,
    n = profile_decode_count(x$n)
  )
  return(profile_decode_suppressed(out, x))
}

profile_decode_dependency <- function(x) {
  return(list(
    columns = profile_decode_text(x$columns),
    holds = as.logical(x$holds)
  ))
}
