# Compares two csdb_profile objects. Each table of counts is judged against
# the sampling noise of a table of its size.

#' Compare two profiles
#'
#' Measures how far each summary of a synthetic profile is from the same
#' summary of a real profile. It judges each distance against the sampling
#' noise of a synthetic table of that size.
#'
#' @param real The `csdb_profile` of the real data.
#' @param synth The `csdb_profile` of the synthetic data. Profile the
#'   synthetic data with the same `columns`, `k` and `max_levels` as the real
#'   data.
#' @param margin The amount by which a distance can exceed the noise of its
#'   table and still pass.
#' @param n_boot The number of bootstrap draws for each table.
#' @param seed The seed for [set.seed()]. The function restores the random
#'   state of the caller on exit.
#'
#' @details
#' Each row compares one summary of `real` with the same summary of
#' `synth`. The tables of counts are `levels`, `days`, `weeks`, `months`,
#' `quarters`, `bins`, `crosstab` and `group_sizes`. The distance of two
#' tables is the total variation distance of their shares. Each table is
#' normalised over its own published cells, so suppressed cells take no
#' part.
#'
#' The function aligns the cells of the two tables first:
#'
#' - Ordered cells are days, weeks, months, runs of quarters, bins, ranges of
#'   group sizes, and the levels of a number or a time. Cells of the two
#'   tables that overlap join one aligned cell. So each aligned cell is a
#'   union of whole cells of both tables. It takes the coarser resolution of
#'   the two at that place. The last range of group sizes has no end.
#' - Unordered cells are the `levels` of a `TEXT` or `BOOLEAN` column and the
#'   cells of a crosstab. They align by value.
#'
#' An aligned cell that only one table holds counts in the distance. Merge
#' suppression drops no ordered row. The simulator draws no row in a cell
#' that `real` suppressed.
#'
#' The threshold of a table comes from `n_boot` draws:
#'
#' 1. Each draw is a multinomial sample from the real shares. Its size is
#'    the size of the synthetic table: its rows, suppressed cells included,
#'    or its identifiers for `group_sizes`.
#' 2. An ordered draw puts each row at a uniform value within its real cell,
#'    as [simulate_from_profile()] does.
#' 3. The function applies the `k` merge of [profile_query()] to the draw,
#'    with the `k` and `max_levels` of `real`. Unordered cells below `k` are
#'    removed.
#' 4. The threshold is the 95th percentile of the distances of the draws
#'    from `real`, plus `margin`.
#'
#' The other rows:
#'
#' - `missing_share`: the absolute difference of the two shares. The draws
#'   are binomial, of `synth$n_rows` rows at the real share. The function
#'   skips a column where either share is `"suppressed"`.
#' - `n_distinct`: 0 when the synthetic `n_distinct` is in the range that
#'   `synth$n_rows` uniform draws from the real `n_distinct` values can
#'   reach. Otherwise the absolute difference from the nearest end of that
#'   range, divided by that end. The range runs from the expected number of
#'   distinct values in those draws up to the smaller of the real
#'   `n_distinct` and `synth$n_rows`. Both ends are rounded down to a
#'   multiple of the `k` of `synth`, as the profile rounds `n_distinct`.
#'   The threshold is `margin`.
#' - `dependency`: 0 when both profiles agree on `holds`, otherwise 1. The
#'   threshold is 0.
#'
#' A summary of `real` that `synth` lacks, or holds in a form that does not
#' align, gets distance 1 and threshold `margin`. A `schema_only` column has
#' only its `missing_share` compared. The draws do not model the noise that
#' `noise` adds to a profile.
#'
#' @return A data.table with the columns `column`, `property`, `distance`,
#'   `threshold` and `pass`. `pass` is `distance <= threshold`. A crosstab or
#'   a dependency names its columns as `"a,b"`.
#' @family simulation functions
#' @examples
#' d <- data.frame(sex = rep(c("female", "male"), 500))
#' p <- profile_data(d, c(sex = "TEXT"), columns = c(sex = "full"))
#' sim <- simulate_from_profile(p, n = 1000)
#' s <- profile_data(sim, c(sex = "TEXT"), columns = c(sex = "full"))
#' profile_compare(p, s)
#' @export
profile_compare <- function(
  real,
  synth,
  margin = 0.05,
  n_boot = 200L,
  seed = 1L
) {
  if (!inherits(real, "csdb_profile") || !inherits(synth, "csdb_profile")) {
    stop("real and synth must be csdb_profile objects.", call. = FALSE)
  }
  if (!is.numeric(margin) || length(margin) != 1 || is.na(margin)) {
    stop("margin must be one number.", call. = FALSE)
  }
  setup <- compare_setup(real, margin, n_boot)
  rows <- simulate_with_seed(seed, function() {
    return(compare_rows(real, synth, setup))
  })
  out <- data.table::rbindlist(c(list(compare_row(character(0))), rows))
  out$pass <- out$distance <= out$threshold
  return(out)
}

# The margin, the number of draws, and the k and max_levels of `real`.
compare_setup <- function(real, margin, n_boot) {
  k <- real$provenance$k
  if (is.null(k)) {
    k <- 1L
  }
  max_levels <- real$provenance$max_levels
  if (is.null(max_levels)) {
    max_levels <- 50L
  }
  return(list(
    margin = margin,
    n_boot = profile_check_count(n_boot, "n_boot", 1),
    ctx = list(k = k, noise = NULL),
    max_levels = max_levels
  ))
}

compare_rows <- function(real, synth, setup) {
  return(c(
    lapply(names(real$columns), function(x) {
      return(compare_column(
        real$columns[[x]],
        synth$columns[[x]],
        x,
        synth,
        setup
      ))
    }),
    lapply(
      real$crosstabs,
      compare_crosstab,
      synth = synth$crosstabs,
      setup = setup
    ),
    lapply(names(real$group_sizes), function(x) {
      return(compare_group_size(
        real$group_sizes[[x]],
        synth$group_sizes[[x]],
        setup
      ))
    }),
    lapply(real$dependencies, compare_dependency, synth = synth$dependencies)
  ))
}

compare_row <- function(
  column,
  property = character(0),
  distance = numeric(0),
  threshold = numeric(0)
) {
  return(data.table::data.table(
    column = column,
    property = property,
    distance = as.numeric(distance),
    threshold = as.numeric(threshold)
  ))
}

# The 95th percentile of the bootstrap distances, plus the margin.
compare_threshold <- function(distances, setup) {
  return(stats::quantile(distances, 0.95, names = FALSE) + setup$margin)
}

# columns ----

# `profile` is the synthetic profile, for its n_rows and k.
compare_column <- function(real, synth, column, profile, setup) {
  rows <- list(compare_missing(real, synth, column, profile$n_rows, setup))
  kind <- compare_kind(real)
  if (!is.null(kind)) {
    row <- compare_summary(real, synth, column, kind, profile, setup)
    rows <- c(rows, list(row))
  }
  return(data.table::rbindlist(rows))
}

compare_missing <- function(real, synth, column, n_rows, setup) {
  r <- real$missing_share
  s <- synth$missing_share
  if (!is.numeric(r) || !is.numeric(s)) {
    return(NULL)
  }
  draws <- stats::rbinom(setup$n_boot, n_rows, r) / max(n_rows, 1)
  threshold <- compare_threshold(abs(draws - r), setup)
  return(compare_row(column, "missing_share", abs(r - s), threshold))
}

# The form of a column's summary, or NULL for schema_only.
compare_kind <- function(entry) {
  kinds <- c(
    "levels",
    "days",
    "weeks",
    "months",
    "quarters",
    "breaks",
    "n_distinct"
  )
  kind <- kinds[kinds %in% names(entry)][1]
  if (is.na(kind)) {
    return(NULL)
  }
  if (kind == "breaks") {
    return("bins")
  }
  return(kind)
}

compare_summary <- function(real, synth, column, kind, profile, setup) {
  if (kind == "n_distinct") {
    distance <- 1
    if (identical(compare_kind(synth), kind)) {
      distance <- compare_n_distinct(real$n_distinct, synth$n_distinct, profile)
    }
    return(compare_row(column, kind, distance, setup$margin))
  }
  return(compare_table(
    column,
    kind,
    compare_cells(real),
    compare_cells(synth),
    compare_size(synth),
    compare_column_draw(real, setup),
    setup
  ))
}

# The distance of a synthetic n_distinct from the range that n_rows uniform
# draws from `real` distinct values can reach, both ends rounded down to a
# multiple of k.
compare_n_distinct <- function(real, synth, profile) {
  n_rows <- profile$n_rows
  k <- profile$provenance$k
  if (is.null(k)) {
    k <- 1
  }
  expected <- 0
  if (real >= 1) {
    expected <- real * (1 - (1 - 1 / real)^n_rows)
  }
  ends <- floor(c(expected, min(real, n_rows)) / k) * k
  if (synth >= ends[1] && synth <= ends[2]) {
    return(0)
  }
  end <- ends[which.min(abs(synth - ends))]
  return(abs(synth - end) / max(end, 1))
}

# The number of rows or identifiers that a synthetic table counts.
compare_size <- function(entry) {
  if (is.null(entry)) {
    return(0L)
  }
  n <- sum(simulate_weights(entry$n)) + sum(entry$suppressed_n)
  return(as.integer(round(n)))
}

# joint structure ----

compare_same_pair <- function(entries, columns) {
  for (entry in entries) {
    if (identical(as.character(entry$columns), as.character(columns))) {
      return(entry)
    }
  }
  return(NULL)
}

compare_crosstab <- function(real, synth, setup) {
  column <- paste0(real$columns, collapse = ",")
  other <- compare_same_pair(synth, real$columns)
  synth_cells <- NULL
  if (!is.null(other)) {
    synth_cells <- compare_key_cells(compare_cell_keys(other$levels), other$n)
  }
  return(compare_table(
    column,
    "crosstab",
    compare_key_cells(compare_cell_keys(real$levels), real$n),
    synth_cells,
    compare_size(other),
    NULL,
    setup
  ))
}

compare_cell_keys <- function(levels) {
  return(paste(
    simulate_key(levels[[1]]),
    simulate_key(levels[[2]]),
    sep = "\t"
  ))
}

compare_group_size <- function(real, synth, setup) {
  cells <- compare_size_cells(real)
  synth_cells <- NULL
  if (!is.null(synth)) {
    synth_cells <- compare_size_cells(synth)
  }
  return(compare_table(
    real$column,
    "group_sizes",
    cells,
    synth_cells,
    compare_size(synth),
    compare_size_draw(real, setup),
    setup
  ))
}

compare_dependency <- function(real, synth) {
  column <- paste0(real$columns, collapse = ",")
  s <- compare_same_pair(synth, real$columns)
  agree <- !is.null(s) && identical(isTRUE(real$holds), isTRUE(s$holds))
  return(compare_row(column, "dependency", as.numeric(!agree), 0))
}

# tables of counts ----

# One row for a table: its distance and its bootstrap threshold. `draw`
# turns the counts of one draw in the real cells into the cells of the
# drawn table, after the k merge. It is NULL for unordered cells.
compare_table <- function(column, property, cells, synth, size, draw, setup) {
  if (is.null(synth) || !identical(cells$ordered, synth$ordered)) {
    return(compare_row(column, property, 1, setup$margin))
  }
  distances <- compare_boot(cells, size, draw, setup)
  return(compare_row(
    column,
    property,
    compare_distance(cells, synth),
    compare_threshold(distances, setup)
  ))
}

# The distances from `cells` of n_boot multinomial draws of `size` rows.
compare_boot <- function(cells, size, draw, setup) {
  w <- cells$n
  if (sum(w) == 0) {
    return(0)
  }
  counts <- stats::rmultinom(setup$n_boot, size, w)
  if (!cells$ordered) {
    return(compare_unordered_boot(w / sum(w), counts, setup$ctx$k))
  }
  return(vapply(
    seq_len(setup$n_boot),
    function(j) {
      return(compare_distance(cells, draw(counts[, j])))
    },
    numeric(1)
  ))
}

# Unordered draws keep the real cells. A cell below k is removed.
compare_unordered_boot <- function(p, counts, k) {
  counts[counts < k] <- 0
  totals <- colSums(counts)
  q <- counts / rep(pmax(totals, 1), each = nrow(counts))
  distances <- colSums(abs(q - p)) / 2
  distances[totals == 0] <- 1
  return(distances)
}

# The total variation distance of two tables of cells, after alignment.
compare_distance <- function(a, b) {
  p <- compare_shares(a$n)
  q <- compare_shares(b$n)
  if (is.null(p) || is.null(q)) {
    return(as.numeric(!is.null(p) || !is.null(q)))
  }
  groups <- c(a$key, b$key)
  if (a$ordered) {
    groups <- compare_components(c(a$lo, b$lo), c(a$hi, b$hi))
  }
  return(sum(abs(rowsum(c(p, -q), groups))) / 2)
}

# Counts as shares of their published total, or NULL when there are none.
compare_shares <- function(n) {
  if (sum(n) == 0) {
    return(NULL)
  }
  return(n / sum(n))
}

# A group number for each range [lo, hi). Ranges that overlap, directly or
# through other ranges, share a group.
compare_components <- function(lo, hi) {
  o <- order(lo, hi)
  reach <- cummax(hi[o])
  starts <- c(TRUE, lo[o][-1] >= reach[-length(o)])
  groups <- integer(length(o))
  groups[o] <- cumsum(starts)
  return(groups)
}

# cells ----

compare_interval_cells <- function(lo, hi, n) {
  return(list(
    ordered = TRUE,
    lo = as.numeric(lo),
    hi = as.numeric(hi),
    n = simulate_weights(n)
  ))
}

# One value is a range too narrow to reach any other value or break.
compare_point_cells <- function(x, n) {
  x <- as.numeric(x)
  return(compare_interval_cells(x, x + pmax(abs(x), 1) * 1e-12, n))
}

compare_key_cells <- function(key, n) {
  return(list(ordered = FALSE, key = key, n = simulate_weights(n)))
}

# The cells of a column's summary, or NULL when it has no table of counts.
# Dates and times are in days.
compare_cells <- function(entry, type = entry$type) {
  kind <- compare_kind(entry)
  if (is.null(kind) || kind == "n_distinct") {
    return(NULL)
  }
  if (kind == "levels") {
    return(compare_level_cells(entry$levels, entry$n, type))
  }
  if (kind == "bins") {
    b <- entry$breaks
    m <- length(b)
    return(compare_interval_cells(b[-m], b[-1], entry$n))
  }
  if (kind == "days") {
    days <- as.numeric(entry$days)
    return(compare_interval_cells(days, days + 1, entry$n))
  }
  cells <- simulate_period_cells(entry, kind)
  return(compare_interval_cells(cells$lo, cells$hi, entry$n))
}

compare_level_cells <- function(levels, n, type) {
  if (type %in% c("TEXT", "BOOLEAN")) {
    return(compare_key_cells(simulate_key(levels), n))
  }
  x <- as.numeric(levels)
  if (type == "DATETIME") {
    x <- x / 86400
  }
  return(compare_point_cells(x, n))
}

# The last range has no end, so it runs to Inf.
compare_size_cells <- function(group) {
  hi <- c(group$size_to + 1, Inf)[seq_along(group$size_from)]
  return(compare_interval_cells(group$size_from, hi, group$n))
}

# draws ----

# The function that turns the counts of one draw into drawn cells, or NULL
# for unordered levels.
compare_column_draw <- function(real, setup) {
  kind <- compare_kind(real)
  type <- real$type
  if (kind == "levels" && type %in% c("TEXT", "BOOLEAN")) {
    return(NULL)
  }
  if (kind == "levels" && type == "DATETIME") {
    return(function(counts) {
      return(compare_datetime_draw(real, counts, setup))
    })
  }
  if (kind %in% c("levels", "bins")) {
    return(function(counts) {
      values <- compare_numeric_values(real, counts)
      return(compare_cells(compare_numeric_summary(values, real, setup), type))
    })
  }
  units <- profile_date_units
  if (real$sensitivity == "coarsen") {
    units <- units[-1]
  }
  cells <- compare_cells(real)
  return(function(counts) {
    days <- compare_uniform(cells, counts)
    entry <- profile_date_cells(days, rep(1, length(days)), units, setup$ctx)
    return(compare_cells(entry))
  })
}

# A uniform whole number in [lo, hi) of its cell for each counted row.
compare_uniform <- function(cells, counts) {
  i <- rep(seq_along(counts), counts)
  lo <- cells$lo[i]
  return(lo + floor(stats::runif(length(i)) * (cells$hi[i] - lo)))
}

# The values of a numeric draw, as simulate_from_profile() draws them.
compare_numeric_values <- function(real, counts) {
  i <- rep(seq_along(counts), counts)
  if (!is.null(real$levels)) {
    return(as.numeric(real$levels)[i])
  }
  return(as.numeric(simulate_in_bins(real, i, real$type)))
}

# The summary profile_query() gives numeric values: levels when the column
# is full and every level holds k, otherwise merged bins.
compare_numeric_summary <- function(values, real, setup) {
  if (real$sensitivity == "full") {
    levels <- profile_sum_by(values, rep(1, length(values)))
    few <- length(levels$keys) <= setup$max_levels
    if (few && all(levels$n >= setup$ctx$k)) {
      return(list(levels = levels$keys, n = levels$n))
    }
  }
  if (length(values) == 0) {
    return(list(breaks = numeric(0), n = integer(0)))
  }
  breaks <- profile_breaks(min(values), max(values))
  n <- tabulate(findInterval(values, breaks), length(breaks) - 1)
  return(profile_merge_bins(breaks, n, setup$ctx$k))
}

# Levels of a time when every drawn level holds k, otherwise merged days.
compare_datetime_draw <- function(real, counts, setup) {
  present <- counts > 0
  if (all(counts[present] >= setup$ctx$k)) {
    return(compare_level_cells(
      real$levels[present],
      counts[present],
      "DATETIME"
    ))
  }
  days <- floor(as.numeric(real$levels) / 86400)
  entry <- profile_date_cells(days, counts, profile_date_units, setup$ctx)
  return(compare_cells(entry))
}

# Draws row counts as simulate_from_profile() does: the last range runs up
# to twice its size_from.
compare_size_draw <- function(real, setup) {
  cells <- compare_interval_cells(
    real$size_from,
    simulate_size_to(real) + 1,
    real$n
  )
  return(function(counts) {
    sizes <- compare_uniform(cells, counts)
    agg <- profile_sum_by(sizes, rep(1, length(sizes)))
    return(compare_size_cells(profile_size_ranges(agg$keys, agg$n, setup$ctx)))
  })
}
