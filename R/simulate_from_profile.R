# The default generator: a synthetic table drawn from a csdb_profile.

#' Simulate a table from a profile
#'
#' Draws `n` synthetic rows from a `csdb_profile`. The rows follow the
#' published summaries and hold no row of the profiled data.
#'
#' @param profile A `csdb_profile`, from [profile_query()], [read_profile()]
#'   or [profile_schema()].
#' @param n The number of rows, a whole number of at least 1. `NULL` gives
#'   1000 rows, the default.
#' @param seed The seed for [set.seed()]. The function restores the random
#'   state of the caller on exit.
#' @param world `NULL`, or a named list that maps a column to its allowed
#'   values. The function converts the values to the column's type. It
#'   ignores, without a message, each element that names no column of
#'   `profile$field_types`, so one world can serve several tables.
#'
#' @details
#' The R type of each column follows its field type: `TEXT` character,
#' `INTEGER` integer, `DOUBLE` double, `BOOLEAN` logical, `DATE` Date and
#' `DATETIME` POSIXct in UTC.
#'
#' A column takes its values from its own summary:
#'
#' - `levels` or `days`: a draw of the published values, weighted by `n`.
#'   The suppressed cells generate nothing. A `DATETIME` drawn from `days`
#'   gets a uniform whole second within its UTC day.
#' - `weeks`, `months` or `quarters`: a cell drawn by `n`, then a uniform
#'   day within it. A cell of `quarters` ends with the quarter in
#'   `quarters_to` when the profile holds `quarters_to`.
#' - `breaks`: a bin drawn by `n`, then a uniform value within the bin. An
#'   `INTEGER` value is a uniform whole number within the bin. A column with
#'   `nonnegative` `TRUE` draws no value below 0.
#' - `n_distinct` of a `TEXT` column: `"<column>_<i>"`, with `i` uniform in
#'   1 to `n_distinct`.
#' - `missing_share`: `NA` at that rate. A share of `"suppressed"` gives no
#'   `NA`. A key column gets no `NA`.
#'
#' A `schema_only` or `exclude` column, and a column whose summary has no
#' published cell, gets schema values. A `TEXT` or `INTEGER` key column gets
#' `"<column>_<i>"` or `i` for row `i`, so a single-column key is unique at
#' any `n`. Other columns get schema values by type:
#'
#' - `TEXT`: `"<column>_<i>"`, with `i` from 1 to 10.
#' - `INTEGER`: a whole number from 0 to 100.
#' - `DOUBLE`: a uniform value from 0 to 1.
#' - `BOOLEAN`: `TRUE` or `FALSE`. A `BOOLEAN` with `n_distinct` only also
#'   gets these values.
#' - `DATE` and `DATETIME`: a uniform day from 2020-01-01 to 2024-12-31. A
#'   `DATETIME` gets a uniform whole second within that day.
#'
#' The joint structure comes after the columns, in this order:
#'
#' 1. Each crosstab draws its two columns jointly by the cell counts. This
#'    replaces the values of both columns.
#' 2. Each group size draws a range by `n`, then a uniform row count from
#'    `size_from` to `size_to`. The last range has no `size_to` and draws
#'    up to `2 * size_from`. It draws row counts until they cover all rows,
#'    gives each group a fresh identifier, and truncates the last group.
#'    With no published range, each group has 1 row.
#' 3. Each dependency that holds gives every value of its first column one
#'    value of the second column. The function assigns the values of the
#'    first column from the most rows to the fewest. Each goes to the value
#'    of the second column that is furthest below its drawn row count. This
#'    keeps the drawn frequencies of the second column close.
#'
#' Before it draws, the function computes the key space. That is the
#' product, over the key columns, of the number of values each column can
#' take. When `n` is larger, it stops with an error that names the keys, the
#' key space and `n`. A key column can take:
#'
#' - a `world` column: its allowed values.
#' - `levels` or `days`: the values with a count above 0. A `DATETIME` from
#'   `days` can take every second of those days.
#' - `weeks`, `months` or `quarters`: every day of the cells with a count
#'   above 0. A `DATETIME` can take every second of those days.
#' - `breaks`: an `INTEGER` takes every whole number of the bins with a
#'   count above 0, from 0 up when `nonnegative` is `TRUE`. A `DOUBLE` has
#'   no limit.
#' - `n_distinct` of a `TEXT` column: `n_distinct` values.
#' - a group-size column: no limit, because each group gets a fresh
#'   identifier.
#' - schema values: no limit for a `TEXT`, `INTEGER` or `DOUBLE` key
#'   column, and 2 for `BOOLEAN`. A `DATE` takes the 1827 days from
#'   2020-01-01 to 2024-12-31, and a `DATETIME` every second of those days.
#'
#' A crosstab replaces its columns, so a key column of a crosstab can take
#' the values that it has in the cells left after `world`. The key space
#' does not count crosstab pairs or dependencies, so it is an upper bound.
#'
#' The function redraws each row whose keys repeat an earlier row. After 20
#' redraws it stops with an error that names the keys and `n`. A redrawn row
#' keeps the values of its group-size columns.
#'
#' A `world` column takes its values from `world` only. A value that the
#' column's `levels` or `days` also hold keeps its published weight. Every
#' other value gets the mean of those weights, or all values get equal
#' weights when the profile holds none of them. A crosstab keeps only the
#' cells whose values `world` allows, and has no effect when no cell is left.
#' A group-size column takes its identifiers from `world`, without
#' replacement where `world` has enough values.
#'
#' @return A data.table with `n` rows and the columns of
#'   `profile$field_types`, in that order.
#' @family simulation functions
#' @examples
#' d <- data.frame(
#'   sex = rep(c("female", "male"), 10),
#'   age = rep(c(4L, 15L, 38L, 52L, 71L), 4)
#' )
#' p <- profile_data(
#'   d,
#'   c(sex = "TEXT", age = "INTEGER"),
#'   columns = c(sex = "full", age = "coarsen")
#' )
#' simulate_from_profile(p, n = 5, seed = 2)
#' @md
#' @export
simulate_from_profile <- function(
  profile,
  n = 1000L,
  seed = 1L,
  world = NULL
) {
  if (!inherits(profile, "csdb_profile")) {
    stop("profile must be a csdb_profile.", call. = FALSE)
  }
  if (is.null(n)) {
    n <- 1000L
  }
  n <- profile_check_count(n, "n", 1)
  world <- simulate_check_world(world, profile$field_types)
  simulate_check_key_space(profile, n, world)
  return(simulate_with_seed(seed, function() {
    return(simulate_table(profile, n, world))
  }))
}

# Sets the seed, runs f(), and puts back the caller's .Random.seed.
simulate_with_seed <- function(seed, f) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- NULL
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
  }
  on.exit(simulate_restore_seed(had_seed, old_seed, genv), add = TRUE)
  set.seed(seed)
  return(f())
}

simulate_restore_seed <- function(had_seed, old_seed, genv) {
  if (had_seed) {
    assign(".Random.seed", old_seed, envir = genv)
  } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    rm(".Random.seed", envir = genv)
  }
  return(invisible(NULL))
}

# The allowed values of each world column, converted to the column's type.
# An element that names no column of field_types is ignored, so one world
# serves every table.
simulate_check_world <- function(world, field_types) {
  if (is.null(world)) {
    return(list())
  }
  if (!is.list(world) || is.null(names(world)) || any(!nzchar(names(world)))) {
    stop("world must be NULL or a named list.", call. = FALSE)
  }
  columns <- intersect(names(world), names(field_types))
  return(profile_named(columns, function(x) {
    return(simulate_check_allowed(world[[x]], field_types[[x]], x))
  }))
}

simulate_check_allowed <- function(allowed, type, column) {
  allowed <- unique(profile_as_type(allowed, type))
  allowed <- allowed[!is.na(allowed)]
  if (length(allowed) == 0) {
    stop("world gives no value for column ", column, ".", call. = FALSE)
  }
  return(allowed)
}

simulate_table <- function(profile, n, world) {
  d <- simulate_rows(profile, seq_len(n), world)
  d <- simulate_dependencies(d, profile)
  return(simulate_unique_keys(d, profile, world))
}

# The rows `idx`, with every step except dependencies and key redraws.
simulate_rows <- function(profile, idx, world) {
  m <- length(idx)
  field_types <- profile$field_types
  values <- profile_named(names(field_types), function(x) {
    return(simulate_column(profile, x, idx, world))
  })
  for (crosstab in profile$crosstabs) {
    values <- simulate_crosstab(values, crosstab, m, world)
  }
  for (group in profile$group_sizes) {
    values[[group$column]] <- simulate_group_ids(
      group,
      field_types[[group$column]],
      m,
      world[[group$column]]
    )
  }
  values <- simulate_missing(values, profile, m)
  values <- profile_named(names(field_types), function(x) {
    return(profile_as_type(values[[x]], field_types[[x]]))
  })
  return(data.table::as.data.table(values))
}

simulate_column <- function(profile, column, idx, world) {
  type <- profile$field_types[[column]]
  entry <- profile$columns[[column]]
  m <- length(idx)
  if (!is.null(world[[column]])) {
    return(simulate_world(world[[column]], entry, m))
  }
  values <- simulate_summary(entry, column, type, m)
  if (is.null(values)) {
    is_key <- column %in% profile$keys
    values <- simulate_schema_column(column, type, idx, is_key)
  }
  return(values)
}

# Values from the column's summary, or NULL when it has no published cell.
simulate_summary <- function(entry, column, type, m) {
  if (!is.null(entry$breaks)) {
    return(simulate_bins(entry, type, m))
  }
  unit <- simulate_period_unit(entry)
  if (!is.na(unit)) {
    return(simulate_periods(entry, unit, type, m))
  }
  if (!is.null(entry$n_distinct)) {
    return(simulate_n_distinct(entry, column, type, m))
  }
  table <- simulate_frequency(entry)
  if (is.null(table)) {
    return(NULL)
  }
  values <- simulate_draw(table$values, table$n, m)
  if (!is.null(entry$days) && type == "DATETIME") {
    values <- simulate_datetime(values)
  }
  return(values)
}

# The counts as weights: a suppressed cell (NA) weighs 0.
simulate_weights <- function(n) {
  w <- as.numeric(n)
  w[is.na(w)] <- 0
  return(w)
}

# The published values and weights of `levels` or `days`, or NULL.
simulate_frequency <- function(entry) {
  values <- entry$levels
  if (is.null(values)) {
    values <- entry$days
  }
  w <- simulate_weights(entry$n)
  if (is.null(values) || sum(w) == 0) {
    return(NULL)
  }
  return(list(values = values, n = w))
}

simulate_draw <- function(values, w, m) {
  return(values[sample.int(length(values), m, replace = TRUE, prob = w)])
}

simulate_bins <- function(entry, type, m) {
  w <- simulate_weights(entry$n)
  if (sum(w) == 0) {
    return(NULL)
  }
  i <- sample.int(length(w), m, replace = TRUE, prob = w)
  return(simulate_in_bins(entry, i, type))
}

# A uniform value within each bin of `i`. A nonnegative column draws no
# value below 0.
simulate_in_bins <- function(entry, i, type) {
  m <- length(i)
  lo <- entry$breaks[i]
  hi <- entry$breaks[i + 1]
  if (isTRUE(entry$nonnegative)) {
    lo <- pmax(lo, 0)
  }
  if (type == "INTEGER") {
    # The whole numbers of [lo, hi) run from ceiling(lo) to ceiling(hi) - 1.
    return(as.integer(floor(runif(m, ceiling(lo), ceiling(hi)))))
  }
  return(runif(m, lo, hi))
}

# The field of a summary that holds weeks, months or quarters, or NA.
simulate_period_unit <- function(entry) {
  units <- c("weeks", "months", "quarters")
  return(units[units %in% names(entry)][1])
}

# The cells of a period summary as days [lo, hi). A merged run of quarters
# ends after the quarter in `quarters_to`.
simulate_period_cells <- function(entry, unit) {
  lo <- as.numeric(entry[[unit]])
  last <- lo
  if (unit == "quarters" && !is.null(entry$quarters_to)) {
    last <- as.numeric(entry$quarters_to)
  }
  return(list(lo = lo, hi = profile_period_next(last, unit)))
}

# A period drawn by n, then a uniform day within it.
simulate_periods <- function(entry, unit, type, m) {
  w <- simulate_weights(entry$n)
  if (sum(w) == 0) {
    return(NULL)
  }
  cells <- simulate_period_cells(entry, unit)
  i <- sample.int(length(w), m, replace = TRUE, prob = w)
  width <- cells$hi[i] - cells$lo[i]
  days <- .Date(cells$lo[i] + floor(runif(m) * width))
  if (type == "DATETIME") {
    return(simulate_datetime(days))
  }
  return(days)
}

simulate_n_distinct <- function(entry, column, type, m) {
  if (type != "TEXT" || entry$n_distinct < 1) {
    return(NULL)
  }
  return(paste0(column, "_", sample.int(entry$n_distinct, m, replace = TRUE)))
}

# A comparable text key for values of any field type.
simulate_key <- function(x) {
  return(as.character(unclass(x)))
}

simulate_world <- function(allowed, entry, m) {
  return(simulate_draw(allowed, simulate_world_weights(allowed, entry), m))
}

# Published weights for values the profile holds, their mean for the rest.
simulate_world_weights <- function(allowed, entry) {
  table <- simulate_frequency(entry)
  w <- rep(NA_real_, length(allowed))
  if (!is.null(table)) {
    w <- table$n[match(simulate_key(allowed), simulate_key(table$values))]
  }
  if (all(is.na(w) | w == 0)) {
    return(rep(1, length(allowed)))
  }
  w[is.na(w)] <- mean(w, na.rm = TRUE)
  return(w)
}

simulate_crosstab <- function(values, crosstab, m, world) {
  keep <- simulate_weights(crosstab$n) > 0
  for (x in intersect(crosstab$columns, names(world))) {
    allowed <- simulate_key(world[[x]])
    keep <- keep & simulate_key(crosstab$levels[[x]]) %in% allowed
  }
  if (!any(keep)) {
    return(values)
  }
  cells <- which(keep)
  i <- cells[sample.int(length(cells), m, TRUE, prob = crosstab$n[cells])]
  for (x in crosstab$columns) {
    values[[x]] <- crosstab$levels[[x]][i]
  }
  return(values)
}

# Group identifiers: sizes drawn by n until they cover m rows, truncated.
simulate_group_ids <- function(group, type, m, allowed) {
  sizes <- rep_len(simulate_group_sizes(group, m), m)
  groups <- which(cumsum(sizes) >= m)[1]
  ids <- rep(seq_len(groups), sizes[seq_len(groups)])[seq_len(m)]
  if (is.null(allowed)) {
    return(simulate_index_values(group$column, type, ids))
  }
  pick <- sample.int(length(allowed), groups, groups > length(allowed))
  return(allowed[pick][ids])
}

# m row counts: a range drawn by n, then a uniform whole number within it.
# With no published range, each group has 1 row.
simulate_group_sizes <- function(group, m) {
  w <- simulate_weights(group$n)
  if (sum(w) == 0) {
    return(1L)
  }
  i <- sample.int(length(w), m, replace = TRUE, prob = w)
  to <- simulate_size_to(group)
  span <- to[i] - group$size_from[i] + 1
  return(as.integer(group$size_from[i] + floor(runif(m) * span)))
}

# The last row count of each range. The last range has no published end,
# and draws up to twice its size_from.
simulate_size_to <- function(group) {
  m <- length(group$size_from)
  if (m == 0) {
    return(integer(0))
  }
  return(c(group$size_to[seq_len(m - 1)], 2L * group$size_from[m]))
}

simulate_missing <- function(values, profile, m) {
  columns <- setdiff(names(profile$columns), profile$keys)
  for (x in columns) {
    share <- profile$columns[[x]]$missing_share
    if (is.numeric(share) && share > 0) {
      values[[x]][runif(m) < share] <- NA
    }
  }
  return(values)
}

simulate_dependencies <- function(d, profile) {
  for (dependency in profile$dependencies) {
    if (isTRUE(dependency$holds)) {
      b <- dependency$columns[2]
      value <- simulate_dependent(d[[dependency$columns[1]]], d[[b]])
      data.table::set(d, j = b, value = value)
    }
  }
  return(d)
}

# One value of `b` for each value of `a`. The values of `a` with the most rows
# go first, each to the value of `b` with the largest unfilled row count.
simulate_dependent <- function(a, b) {
  a_values <- unique(a)
  a_group <- match(a, a_values)
  a_size <- tabulate(a_group, length(a_values))
  b_values <- b[!duplicated(b)]
  b_left <- tabulate(match(b, b_values), length(b_values))
  assigned <- integer(length(a_values))
  for (g in order(-a_size)) {
    j <- which.max(b_left)
    assigned[g] <- j
    b_left[j] <- b_left[j] - a_size[g]
  }
  return(b_values[assigned[a_group]])
}

simulate_unique_keys <- function(d, profile, world) {
  keys <- profile$keys
  if (length(keys) == 0) {
    return(d)
  }
  for (attempt in 0:20) {
    dup <- which(duplicated(d, by = keys))
    if (length(dup) == 0) {
      return(d)
    }
    if (attempt < 20) {
      d <- simulate_redraw(d, dup, profile, world)
    }
  }
  stop(
    "simulate_from_profile() could not draw ",
    nrow(d),
    " rows unique on the keys ",
    paste0(keys, collapse = ", "),
    " in 20 redraws.",
    call. = FALSE
  )
}

# Replaces the rows `dup` with new rows. Group-size columns keep their values.
simulate_redraw <- function(d, dup, profile, world) {
  new <- simulate_rows(profile, dup, world)
  for (group in profile$group_sizes) {
    data.table::set(new, j = group$column, value = d[[group$column]][dup])
  }
  columns <- names(d)
  d[dup, (columns) := as.list(new)]
  return(simulate_dependencies(d, profile))
}

# key space ----

# Stops before any draw when `n` exceeds the number of distinct key rows that
# the profile and the world can give.
simulate_check_key_space <- function(profile, n, world) {
  if (length(profile$keys) == 0) {
    return(invisible(NULL))
  }
  sizes <- simulate_key_sizes(profile, world)
  space <- prod(sizes)
  if (n > space) {
    stop(
      "simulate_from_profile() cannot draw ",
      n,
      " rows unique on the keys ",
      paste0(profile$keys, collapse = ", "),
      ": the key space holds ",
      format(space, scientific = FALSE),
      " combinations (",
      paste0(
        names(sizes),
        " ",
        format(sizes, scientific = FALSE, trim = TRUE),
        collapse = ", "
      ),
      "), and n is ",
      n,
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The number of values that each key column can take, in the order in which
# simulate_rows() sets them: column, crosstab, group size.
simulate_key_sizes <- function(profile, world) {
  keys <- profile$keys
  sizes <- vapply(
    keys,
    function(x) simulate_column_space(profile, x, world),
    numeric(1)
  )
  names(sizes) <- keys
  for (crosstab in profile$crosstabs) {
    sizes <- simulate_crosstab_space(sizes, crosstab, world)
  }
  for (group in profile$group_sizes) {
    if (group$column %in% keys) {
      sizes[[group$column]] <- simulate_group_space(world[[group$column]])
    }
  }
  return(sizes)
}

simulate_column_space <- function(profile, column, world) {
  if (!is.null(world[[column]])) {
    return(as.numeric(length(world[[column]])))
  }
  type <- profile$field_types[[column]]
  space <- simulate_summary_space(profile$columns[[column]], type)
  if (is.na(space)) {
    space <- simulate_schema_key_space(type)
  }
  return(space)
}

# The number of values that simulate_summary() can draw, or NA when the
# summary has no published cell.
simulate_summary_space <- function(entry, type) {
  if (!is.null(entry$breaks)) {
    return(simulate_bins_space(entry, type))
  }
  unit <- simulate_period_unit(entry)
  if (!is.na(unit)) {
    return(simulate_periods_space(entry, unit, type))
  }
  if (!is.null(entry$n_distinct)) {
    ok <- type == "TEXT" && entry$n_distinct >= 1
    return(if (ok) as.numeric(entry$n_distinct) else NA_real_)
  }
  return(simulate_frequency_space(entry, type))
}

# The values of `levels` or `days` with a count above 0. A DATETIME from
# `days` takes every second of those days.
simulate_frequency_space <- function(entry, type) {
  table <- simulate_frequency(entry)
  if (is.null(table)) {
    return(NA_real_)
  }
  space <- as.numeric(sum(table$n > 0))
  if (!is.null(entry$days) && type == "DATETIME") {
    space <- space * 86400
  }
  return(space)
}

simulate_bins_space <- function(entry, type) {
  w <- simulate_weights(entry$n)
  if (sum(w) == 0) {
    return(NA_real_)
  }
  if (type != "INTEGER") {
    return(Inf)
  }
  i <- which(w > 0)
  lo <- entry$breaks[i]
  if (isTRUE(entry$nonnegative)) {
    lo <- pmax(lo, 0)
  }
  # simulate_in_bins() draws ceiling(lo) to ceiling(hi) - 1, and at least one
  # value from a bin.
  return(sum(pmax(ceiling(entry$breaks[i + 1]) - ceiling(lo), 1)))
}

simulate_periods_space <- function(entry, unit, type) {
  w <- simulate_weights(entry$n)
  if (sum(w) == 0) {
    return(NA_real_)
  }
  cells <- simulate_period_cells(entry, unit)
  days <- sum((cells$hi - cells$lo)[w > 0])
  if (type == "DATETIME") {
    return(days * 86400)
  }
  return(days)
}

# The schema values of a key column: row numbers for TEXT and INTEGER.
simulate_schema_key_space <- function(type) {
  days <- as.numeric(diff(simulate_schema_days)) + 1
  return(switch(
    type,
    TEXT = Inf,
    INTEGER = Inf,
    DOUBLE = Inf,
    BOOLEAN = 2,
    DATE = days,
    DATETIME = days * 86400
  ))
}

# A crosstab replaces both of its columns with the values of its cells that
# world allows. With no such cell it has no effect.
simulate_crosstab_space <- function(sizes, crosstab, world) {
  keep <- simulate_weights(crosstab$n) > 0
  for (x in intersect(crosstab$columns, names(world))) {
    keep <- keep &
      simulate_key(crosstab$levels[[x]]) %in% simulate_key(world[[x]])
  }
  if (!any(keep)) {
    return(sizes)
  }
  for (x in intersect(crosstab$columns, names(sizes))) {
    sizes[[x]] <- as.numeric(length(unique(crosstab$levels[[x]][keep])))
  }
  return(sizes)
}

# Fresh identifiers have no limit. World identifiers are its allowed values.
simulate_group_space <- function(allowed) {
  if (is.null(allowed)) {
    return(Inf)
  }
  return(as.numeric(length(allowed)))
}
