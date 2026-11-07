# The profile engine: a disclosure-controlled summary of a table.
#
# profile_query() runs only aggregate queries (R/profile_sql.R) and applies
# suppression in R. write_profile() and read_profile() (R/profile_yaml.R)
# move a profile to and from YAML.

profile_sensitivity_levels <- c("full", "coarsen", "schema_only", "exclude")
profile_field_type_names <- c(
  "TEXT",
  "INTEGER",
  "DOUBLE",
  "BOOLEAN",
  "DATE",
  "DATETIME"
)

#' Profile the result of a SQL query
#'
#' Summarises the rows of a query as a `csdb_profile`, under disclosure
#' control. Every statistic comes from an aggregate query, and the profile
#' keeps no count below `k`.
#'
#' @param connection A DBI connection, or a [DBConnection_v9]. For a
#'   `DBConnection_v9` the function uses `$autoconnection`.
#' @param sql The query to profile, as one string. The function runs it as the
#'   subquery `SELECT ... FROM (<sql>) q`. The query is published with the
#'   profile as `provenance$query`, so a caller MUST NOT put an identifier
#'   literal, such as a person number, in it.
#' @param columns A named character vector or named list that gives the
#'   sensitivity of each column: `"full"`, `"coarsen"`, `"schema_only"` or
#'   `"exclude"`. A column that `columns` does not name is `"schema_only"`.
#' @param field_types A named character vector of csdb field types: `TEXT`,
#'   `INTEGER`, `DOUBLE`, `BOOLEAN`, `DATE` or `DATETIME`. With `NULL`, the
#'   function infers the types from the R classes of a zero-row fetch.
#' @param keys A character vector of key column names, or `NULL`.
#' @param k The smallest count that the profile can hold.
#' @param noise `NULL`, or `list(epsilon = e)`. With `epsilon`, the function
#'   adds Laplace noise of scale `1 / e` to each cell count and to each count
#'   of missing values. It adds the noise before suppression, then rounds each
#'   count and floors it at 0. Call `set.seed()` first for a reproducible
#'   profile.
#' @param max_levels The largest number of distinct values for which a `full`
#'   column publishes its values.
#' @param group_sizes A named list, for example `list(rows_per_id = "Id")`.
#'   Each element names one column. The profile holds how many values of that
#'   column have each row count.
#' @param crosstabs A list of pairs of column names, for example
#'   `list(c("sex", "age_group"))`. Both columns MUST be `"full"`.
#' @param dependencies A list of pairs of column names. For each pair the
#'   profile holds whether the first column determines the second.
#' @param source A label for the data source, or `NULL`. The profile
#'   publishes it.
#'
#' @details
#' Each column of `field_types` gets a summary from its sensitivity:
#'
#' - `schema_only`: `type`, `sensitivity` and `missing_share`. All other
#'   summaries hold these three fields too. An `INTEGER` or `DOUBLE` column
#'   also holds `nonnegative`, which is `TRUE` when its SQL minimum is at
#'   least 0.
#' - `exclude`: no entry in `columns`. The column is in `field_types` only.
#' - `full` `TEXT` or `BOOLEAN`, at most `max_levels` distinct values:
#'   `levels` and `n`.
#' - `full` `INTEGER`, `DOUBLE` or `DATETIME`, at most `max_levels` distinct
#'   values: `levels` and `n` when every level holds at least `k` rows.
#'   Otherwise the column takes the summary that it has above `max_levels`.
#' - `full` `TEXT`, above `max_levels`: `n_distinct`, rounded down to a
#'   multiple of `k`.
#' - `full` `DATE`: counts per day, as `days` and `n`.
#' - `full` `DATETIME`: `levels` and `n`, as above, otherwise counts per UTC
#'   day as `days` and `n`. The function truncates each level to the whole
#'   second.
#' - `coarsen` `DATE` or `DATETIME`: counts per ISO week, as `weeks` and `n`.
#'   The Monday of each week is its key. A `DATETIME` falls in the week of
#'   its UTC day.
#' - `INTEGER` and `DOUBLE`, `full` above `max_levels` or `coarsen`: `breaks`
#'   and `n`, for the bins `[breaks[i], breaks[i + 1])`.
#' - `coarsen` `TEXT` or `BOOLEAN`: an error that names the column.
#'
#' The breaks come from `pretty(c(min, max), n = 20)`. An end break that
#' equals the observed minimum or maximum moves one step outward. The
#' function removes an inner break that equals either extreme. So no break
#' equals an observed extreme.
#'
#' Suppression keeps every published count at `k` or above. On an ordered
#' dimension, a cell below `k` merges with its neighbours:
#'
#' - Days merge into ISO weeks, then calendar months, then calendar
#'   quarters. Weeks merge into months, then quarters. The column takes the
#'   finest of these resolutions at which every cell holds `k`. The field
#'   name gives the resolution: `days`, `weeks`, `months` or `quarters`.
#'   Each key is the first day of its cell. A week merges into the month of
#'   its Monday.
#' - When a quarter holds fewer than `k` rows, adjacent quarters merge into
#'   runs. Each run takes quarters in order until it holds `k`. A last run
#'   below `k` joins the run before it. `quarters` then holds the first
#'   quarter of each run and `quarters_to` the last.
#' - Bins merge into runs in the same way. A merged bin keeps the outer
#'   breaks of its run, so no break is added.
#' - Group sizes merge into runs in the same way. Each run is published as
#'   `size_from`, `size_to` and `n`, the number of values with a row count
#'   in that range. The ranges tile the row counts from 1 up with no gap.
#'   The first range starts at 1. Every other `size_from` is an observed row
#'   count, the smallest in its run. Each `size_to` is one below the next
#'   `size_from`. The last range has no `size_to`, so it runs from its
#'   `size_from` up.
#' - A first run that holds only the smallest row count merges with the
#'   next run. A last run that holds only the largest merges with the run
#'   before it. So no exact smallest or largest row count is published.
#'
#' A table whose present values number fewer than `k` publishes no cell.
#' Unordered cells, which are the `levels` of a `TEXT` or `BOOLEAN` column
#' and the cells of a crosstab, do not merge. Each cell below `k` is
#' removed. A column sums its removed counts into `suppressed_n`, and it
#' keeps `suppressed_n` only when that sum is at least `k`. With `noise`,
#' the function adds the noise to the finest cells, before it merges or
#' removes any cell.
#'
#' `missing_share` is `"suppressed"` when the count of missing values, or
#' the count of values present, is from 1 to `k - 1`. `n_rows` holds no
#' noise. A query with fewer than `k` rows is an error.
#'
#' A crosstab counts the rows where both columns hold a value. Each of its
#' columns MUST have `levels` or be a `DATE`. A group-size range is a range
#' of row counts, so it can be below `k`. Only `n` must reach `k`. A
#' dependency holds when each value of the first column, `NULL` included,
#' goes with one value of the second. The profile stores no mapping.
#'
#' @return A list of class `csdb_profile`, with the elements `provenance`,
#'   `field_types`, `keys`, `n_rows`, `columns`, `group_sizes`, `crosstabs`,
#'   `dependencies` and `notes`. `provenance` holds `format`, `source`,
#'   `query`, `driver`, `profiled_at` (UTC, ISO 8601), `k`, `noise`,
#'   `max_levels` and `csdb_version`. `driver` is the class of the DBI
#'   connection.
#' @family profile functions
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
#' DBI::dbWriteTable(con, "visits", data.frame(
#'   id = rep(1:20, each = 3),
#'   sex = rep(c("female", "male"), 30),
#'   age = rep(c(4, 15, 38, 52, 71, 90), 10)
#' ))
#' p <- profile_query(
#'   con,
#'   "SELECT * FROM visits",
#'   columns = c(sex = "full", age = "coarsen", id = "exclude"),
#'   group_sizes = list(rows_per_id = "id")
#' )
#' p$columns$sex
#' p$columns$age$breaks
#' DBI::dbDisconnect(con)
#' @export
profile_query <- function(
  connection,
  sql,
  columns = NULL,
  field_types = NULL,
  keys = NULL,
  k = 5L,
  noise = NULL,
  max_levels = 50L,
  group_sizes = NULL,
  crosstabs = NULL,
  dependencies = NULL,
  source = NULL
) {
  con <- profile_dbi(connection)
  sql <- profile_check_sql(sql)
  if (is.null(field_types)) {
    field_types <- sql_field_types(con, sql)
  }
  field_types <- profile_check_field_types(field_types)
  sensitivity <- profile_sensitivity(columns, field_types)
  ctx <- profile_context(
    con,
    sql,
    field_types,
    sensitivity,
    k,
    noise,
    max_levels
  )
  profiled <- names(sensitivity)[sensitivity != "exclude"]
  column_profiles <- profile_named(profiled, function(x) profile_column(ctx, x))
  return(new_profile(
    provenance = profile_provenance(ctx, sql, con, source),
    field_types = field_types,
    keys = profile_check_keys(keys, field_types),
    n_rows = ctx$n_rows,
    columns = column_profiles,
    group_sizes = profile_group_sizes(ctx, group_sizes),
    crosstabs = profile_crosstabs(ctx, crosstabs, column_profiles),
    dependencies = profile_dependencies(ctx, dependencies),
    notes = character(0)
  ))
}

#' Profile a DBTable_v9
#'
#' Profiles every row of a [DBTable_v9] with [profile_query()]. The table
#' supplies the query, the field types and the keys.
#'
#' @param table A [DBTable_v9].
#' @param columns The sensitivity of each column, as in [profile_query()].
#' @param ... Other arguments to [profile_query()]. Do not give `connection`,
#'   `sql`, `field_types` or `keys`.
#' @return A `csdb_profile`, as [profile_query()] returns it. The query is
#'   `dbplyr::sql_render(table$tbl())`.
#' @family profile functions
#' @examples
#' tab <- DBTable_v9$new(
#'   dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
#'   table_name = "visits",
#'   field_types = c(id = "INTEGER", sex = "TEXT"),
#'   keys = "id",
#'   validator_field_types = validator_field_types_blank,
#'   validator_field_contents = validator_field_contents_blank
#' )
#' tab$create_table()
#' tab$insert_data(data.table::data.table(
#'   id = 1:12,
#'   sex = rep(c("female", "male"), 6)
#' ))
#' p <- profile_table(tab, columns = c(sex = "full"))
#' p$columns$sex
#' tab$dbconnection$disconnect()
#' @export
profile_table <- function(table, columns = NULL, ...) {
  sql <- as.character(dbplyr::sql_render(table$tbl()))
  return(profile_query(
    connection = table$dbconnection,
    sql = sql,
    columns = columns,
    field_types = table$field_types,
    keys = table$keys,
    ...
  ))
}

#' Profile a data frame
#'
#' Writes `d` to an in-memory SQLite database and profiles it with
#' [profile_query()]. Use it to profile data that is already in R.
#'
#' @param d A data frame that holds every column of `field_types`.
#' @param field_types A named character vector of csdb field types, as in
#'   [profile_query()]. Columns of `d` that it does not name are not written.
#' @param ... Other arguments to [profile_query()]. Do not give `connection`,
#'   `sql` or `field_types`.
#' @return A `csdb_profile`, as [profile_query()] returns it. The query
#'   selects every column of the in-memory table `profile_data`.
#' @family profile functions
#' @examples
#' d <- data.frame(
#'   day = as.Date("2024-01-01") + rep(0:3, each = 5),
#'   cases = rep(c(2L, 7L, 9L, 12L), each = 5)
#' )
#' p <- profile_data(
#'   d,
#'   field_types = c(day = "DATE", cases = "INTEGER"),
#'   columns = c(day = "full", cases = "full")
#' )
#' p$columns$day
#' @export
profile_data <- function(d, field_types, ...) {
  field_types <- profile_check_field_types(field_types)
  missing <- setdiff(names(field_types), names(d))
  if (length(missing) > 0) {
    stop(
      "d lacks the column(s) named in field_types: ",
      paste0(missing, collapse = ", "),
      call. = FALSE
    )
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sqlite_types <- unname(sqlite_field_types[field_types])
  names(sqlite_types) <- names(field_types)
  DBI::dbWriteTable(
    con,
    "profile_data",
    as.data.frame(d)[names(field_types)],
    field.types = sqlite_types
  )
  sql <- paste0("SELECT * FROM ", profile_quote(con, "profile_data"))
  return(profile_query(con, sql, field_types = field_types, ...))
}

# The constructor that profile_query() and read_profile() share, so both
# build the elements in one order.
new_profile <- function(
  provenance,
  field_types,
  keys,
  n_rows,
  columns,
  group_sizes,
  crosstabs,
  dependencies,
  notes
) {
  return(structure(
    list(
      provenance = provenance,
      field_types = field_types,
      keys = keys,
      n_rows = n_rows,
      columns = columns,
      group_sizes = group_sizes,
      crosstabs = crosstabs,
      dependencies = dependencies,
      notes = notes
    ),
    class = "csdb_profile"
  ))
}

# A named list from `f` applied to each of `x`. An empty `x` gives list(),
# which YAML gives back unchanged.
profile_named <- function(x, f) {
  out <- lapply(x, f)
  if (length(out) > 0) {
    names(out) <- x
  }
  return(out)
}

# argument checks ----

profile_dbi <- function(connection) {
  if (inherits(connection, "DBConnection_v9")) {
    return(connection$autoconnection)
  }
  if (!inherits(connection, "DBIConnection")) {
    stop(
      "connection must be a DBI connection or a DBConnection_v9.",
      call. = FALSE
    )
  }
  return(connection)
}

profile_check_sql <- function(sql) {
  sql <- as.character(sql)
  if (length(sql) != 1 || is.na(sql) || !nzchar(sql)) {
    stop("sql must be one non-empty string.", call. = FALSE)
  }
  return(sql)
}

profile_check_field_types <- function(field_types) {
  field_types <- unlist(field_types)
  if (is.null(names(field_types)) || any(!nzchar(names(field_types)))) {
    stop("field_types must name every column.", call. = FALSE)
  }
  bad <- !field_types %in% profile_field_type_names
  if (any(bad)) {
    stop(
      "Unknown field type(s): ",
      paste0(
        names(field_types)[bad],
        " (",
        field_types[bad],
        ")",
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  return(field_types)
}

profile_check_keys <- function(keys, field_types) {
  keys <- as.character(keys)
  unknown <- setdiff(keys, names(field_types))
  if (length(unknown) > 0) {
    stop(
      "keys names unknown column(s): ",
      paste0(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  return(keys)
}

profile_check_count <- function(x, name, minimum) {
  if (length(x) != 1 || is.na(x) || x < minimum || x != round(x)) {
    stop(
      name,
      " must be one whole number of at least ",
      minimum,
      ".",
      call. = FALSE
    )
  }
  return(as.integer(x))
}

profile_check_noise <- function(noise) {
  if (is.null(noise)) {
    return(NULL)
  }
  epsilon <- noise$epsilon
  if (
    !is.numeric(epsilon) ||
      length(epsilon) != 1 ||
      is.na(epsilon) ||
      epsilon <= 0
  ) {
    stop(
      "noise must be NULL or list(epsilon = e), with e above 0.",
      call. = FALSE
    )
  }
  return(list(epsilon = as.numeric(epsilon)))
}

# The sensitivity of every column of field_types. An undeclared column is
# schema_only.
profile_sensitivity <- function(columns, field_types) {
  out <- rep("schema_only", length(field_types))
  names(out) <- names(field_types)
  columns <- unlist(columns)
  unknown <- setdiff(names(columns), names(field_types))
  if (length(unknown) > 0) {
    stop(
      "columns names unknown column(s): ",
      paste0(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  bad <- !columns %in% profile_sensitivity_levels
  if (any(bad)) {
    stop(
      "Unknown sensitivity: ",
      paste0(names(columns)[bad], " (", columns[bad], ")", collapse = ", "),
      call. = FALSE
    )
  }
  out[names(columns)] <- columns
  profile_check_coarsen(out, field_types)
  return(out)
}

profile_check_coarsen <- function(sensitivity, field_types) {
  bad <- sensitivity == "coarsen" & field_types %in% c("TEXT", "BOOLEAN")
  if (any(bad)) {
    stop(
      "coarsen applies to DATE, DATETIME, INTEGER and DOUBLE columns only. ",
      "Column(s): ",
      paste0(
        names(field_types)[bad],
        " (",
        field_types[bad],
        ")",
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  return(invisible(sensitivity))
}

# The settings and the row counts that every summary uses.
profile_context <- function(
  con,
  sql,
  field_types,
  sensitivity,
  k,
  noise,
  max_levels
) {
  k <- profile_check_count(k, "k", 1)
  profiled <- names(sensitivity)[sensitivity != "exclude"]
  counts <- sql_missing_counts(con, sql, profiled)
  if (counts$n_rows < k) {
    stop(
      "The query returns fewer than k rows, so no profile can be published.",
      call. = FALSE
    )
  }
  return(list(
    con = con,
    sql = sql,
    field_types = field_types,
    sensitivity = sensitivity,
    k = k,
    noise = profile_check_noise(noise),
    max_levels = profile_check_count(max_levels, "max_levels", 1),
    n_rows = counts$n_rows,
    missing = counts$missing
  ))
}

profile_provenance <- function(ctx, sql, con, source) {
  if (!is.null(source)) {
    source <- as.character(source)
  }
  return(list(
    format = 1L,
    source = source,
    query = sql,
    driver = class(con)[1],
    profiled_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    k = ctx$k,
    noise = ctx$noise,
    max_levels = ctx$max_levels,
    csdb_version = as.character(utils::packageVersion("csdb"))
  ))
}

# suppression ----

# Laplace noise of scale 1 / epsilon, added, rounded and floored at 0.
profile_noise <- function(n, noise) {
  n <- as.numeric(n)
  if (!is.null(noise) && length(n) > 0) {
    u <- runif(length(n), -0.5, 0.5)
    n <- n - sign(u) * log(1 - 2 * abs(u)) / noise$epsilon
  }
  return(as.integer(pmax(round(n), 0)))
}

# Keep the cells with a count of at least k. `keys` is a list of vectors, one
# per key column. The removed counts go to suppressed_n when they sum to k.
profile_suppress <- function(keys, n, ctx) {
  n <- profile_noise(n, ctx$noise)
  keep <- n >= ctx$k
  out <- list(keys = lapply(keys, function(x) x[keep]), n = n[keep])
  suppressed <- sum(n[!keep])
  if (suppressed >= ctx$k) {
    out$suppressed_n <- suppressed
  }
  return(out)
}

# Sum the counts of equal keys, and sort the keys in C-locale order.
profile_aggregate <- function(keys, n) {
  n <- as.numeric(n)
  if (length(n) == 0) {
    return(list(keys = keys, n = n))
  }
  o <- do.call(order, c(unname(keys), list(method = "radix")))
  keys <- lapply(keys, function(x) x[o])
  first <- !duplicated(as.data.frame(keys, col.names = seq_along(keys)))
  sums <- rowsum(n[o], cumsum(first), reorder = FALSE)[, 1]
  return(list(keys = lapply(keys, function(x) x[first]), n = unname(sums)))
}

# The published share of missing values.
profile_missing_share <- function(n_missing, ctx) {
  m <- min(profile_noise(n_missing, ctx$noise), ctx$n_rows)
  present <- ctx$n_rows - m
  if (profile_is_small(m, ctx$k) || profile_is_small(present, ctx$k)) {
    return("suppressed")
  }
  return(m / ctx$n_rows)
}

profile_is_small <- function(n, k) {
  return(n > 0 && n < k)
}

# R values of a field type ----

profile_as_type <- function(x, type) {
  return(switch(
    type,
    TEXT = as.character(x),
    INTEGER = as.integer(x),
    DOUBLE = as.numeric(x),
    BOOLEAN = as.logical(x),
    DATE = profile_as_date(x),
    DATETIME = profile_as_datetime(x)
  ))
}

# A Date with double storage, from a Date, a POSIXct, a number of days or
# ISO 8601 text.
profile_as_date <- function(x) {
  if (inherits(x, "POSIXct")) {
    x <- as.Date(x, tz = "UTC")
  }
  return(.Date(as.numeric(as.Date(x))))
}

# A POSIXct in UTC, from a POSIXct, a number of seconds or ISO 8601 text.
profile_as_datetime <- function(x) {
  if (is.character(x)) {
    x <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  return(.POSIXct(as.numeric(x), tz = "UTC"))
}

# per column ----

profile_column <- function(ctx, column) {
  entry <- list(
    type = ctx$field_types[[column]],
    sensitivity = ctx$sensitivity[[column]],
    missing_share = profile_missing_share(ctx$missing[[column]], ctx)
  )
  if (entry$type %in% c("INTEGER", "DOUBLE")) {
    entry$nonnegative <- profile_nonnegative(ctx, column)
  }
  summary <- switch(
    entry$sensitivity,
    schema_only = list(),
    full = profile_full(ctx, column),
    coarsen = profile_coarsen(ctx, column)
  )
  return(c(entry, summary))
}

# TRUE when the SQL minimum of a numeric column is at least 0. A column with
# no value gives FALSE.
profile_nonnegative <- function(ctx, column) {
  lo <- sql_range(ctx$con, ctx$sql, column)[["lo"]]
  return(isTRUE(lo >= 0))
}

profile_full <- function(ctx, column) {
  type <- ctx$field_types[[column]]
  if (type == "DATE") {
    return(profile_dates(ctx, column, profile_date_units))
  }
  n_distinct <- sql_count_distinct(ctx$con, ctx$sql, column)
  if (n_distinct <= ctx$max_levels) {
    return(profile_levels(ctx, column))
  }
  return(profile_above_max_levels(ctx, column, n_distinct))
}

profile_above_max_levels <- function(ctx, column, n_distinct) {
  return(switch(
    ctx$field_types[[column]],
    TEXT = profile_n_distinct(ctx, n_distinct),
    BOOLEAN = profile_n_distinct(ctx, n_distinct),
    DATETIME = profile_dates(ctx, column, profile_date_units),
    profile_bins(ctx, column)
  ))
}

profile_coarsen <- function(ctx, column) {
  type <- ctx$field_types[[column]]
  if (type %in% c("DATE", "DATETIME")) {
    return(profile_dates(ctx, column, profile_date_units[-1]))
  }
  return(profile_bins(ctx, column))
}

# The values of one column, converted to its type, with their row counts.
profile_values <- function(ctx, column) {
  r <- sql_value_counts(ctx$con, ctx$sql, column)
  return(list(values = profile_typed_values(ctx, column, r[[1]]), n = r$n))
}

profile_cells <- function(ctx, values, n, name) {
  agg <- profile_aggregate(list(values), n)
  cells <- profile_suppress(agg$keys, agg$n, ctx)
  out <- list(cells$keys[[1]], cells$n)
  names(out) <- c(name, "n")
  out$suppressed_n <- cells$suppressed_n
  return(out)
}

# Levels of an ordered type do not lose a small cell. When a level is below
# k, the column takes the summary it has above max_levels.
profile_levels <- function(ctx, column) {
  v <- profile_values(ctx, column)
  type <- ctx$field_types[[column]]
  if (type %in% c("TEXT", "BOOLEAN")) {
    return(profile_cells(ctx, v$values, v$n, "levels"))
  }
  agg <- profile_aggregate(list(v$values), v$n)
  n <- profile_noise(agg$n, ctx$noise)
  if (all(n >= ctx$k)) {
    return(list(levels = agg$keys[[1]], n = n))
  }
  return(profile_above_max_levels(ctx, column, NA))
}

profile_dates <- function(ctx, column, units) {
  v <- profile_values(ctx, column)
  return(profile_date_cells(profile_as_date(v$values), v$n, units, ctx))
}

# date resolutions ----

# The resolutions of a date summary, finest first. Each name is the field
# that holds the first day of each published cell.
profile_date_units <- c("days", "weeks", "months", "quarters")

# Counts at the finest of `units` at which every cell holds k, or merged
# quarters. The noise goes on the cells of the first unit.
profile_date_cells <- function(days, n, units, ctx) {
  base <- profile_sum_by(profile_period_start(days, units[1]), n)
  counts <- profile_noise(base$n, ctx$noise)
  for (unit in units) {
    cells <- profile_sum_by(profile_period_start(base$keys, unit), counts)
    if (all(cells$n >= ctx$k)) {
      out <- list(.Date(cells$keys), cells$n)
      names(out) <- c(unit, "n")
      return(out)
    }
  }
  runs <- profile_run_cells(cells$keys, cells$n, ctx$k)
  return(list(
    quarters = .Date(runs$from),
    quarters_to = .Date(runs$to),
    n = runs$n
  ))
}

# The sum of `n` for each distinct number in `keys`, in increasing order.
profile_sum_by <- function(keys, n) {
  keys <- as.numeric(keys)
  u <- sort(unique(keys))
  if (length(u) == 0) {
    return(list(keys = numeric(0), n = integer(0)))
  }
  sums <- rowsum(as.numeric(n), match(keys, u))
  return(list(keys = u, n = as.integer(sums[, 1])))
}

# The first day of the period of each day, as a number of days.
profile_period_start <- function(days, unit) {
  days <- as.numeric(days)
  if (unit == "days" || length(days) == 0) {
    return(days)
  }
  if (unit == "weeks") {
    # 1970-01-01 was a Thursday, so (days + 3) %% 7 is 0 on a Monday.
    return(days - (days + 3) %% 7)
  }
  lt <- as.POSIXlt(.Date(days))
  lt$mday <- 1L
  if (unit == "quarters") {
    lt$mon <- (lt$mon %/% 3L) * 3L
  }
  return(as.numeric(as.Date(lt)))
}

# The first day after the period that starts on each of `starts`.
profile_period_next <- function(starts, unit) {
  starts <- as.numeric(starts)
  if (unit == "days") {
    return(starts + 1)
  }
  if (unit == "weeks") {
    return(starts + 7)
  }
  if (length(starts) == 0) {
    return(starts)
  }
  lt <- as.POSIXlt(.Date(starts))
  lt$mon <- lt$mon + ifelse(unit == "quarters", 3L, 1L)
  return(as.numeric(as.Date(lt)))
}

# Ordered cells merge into runs. A run takes cells in order until it holds
# k. A last run below k joins the run before it. So a run below k is left
# only when all cells together hold fewer than k.
profile_runs <- function(n, k) {
  runs <- integer(length(n))
  run <- 1L
  held <- 0
  for (i in seq_along(n)) {
    runs[i] <- run
    held <- held + n[i]
    if (held >= k && i < length(n)) {
      run <- run + 1L
      held <- 0
    }
  }
  if (held < k && run > 1L) {
    runs[runs == run] <- run - 1L
  }
  return(runs)
}

# The first key, the last key and the count of each run of ordered cells.
# A run below k is not kept.
profile_run_cells <- function(keys, n, k, runs = profile_runs(n, k)) {
  if (length(n) == 0) {
    return(list(from = keys, to = keys, n = integer(0)))
  }
  sums <- as.integer(rowsum(as.numeric(n), runs, reorder = FALSE)[, 1])
  keep <- sums >= k
  return(list(
    from = keys[!duplicated(runs)][keep],
    to = keys[!duplicated(runs, fromLast = TRUE)][keep],
    n = sums[keep]
  ))
}

profile_n_distinct <- function(ctx, n_distinct) {
  return(list(n_distinct = as.integer(floor(n_distinct / ctx$k) * ctx$k)))
}

# bins ----

profile_bins <- function(ctx, column) {
  range <- sql_range(ctx$con, ctx$sql, column)
  if (is.na(range[["lo"]])) {
    return(list(breaks = numeric(0), n = integer(0)))
  }
  breaks <- profile_breaks(range[["lo"]], range[["hi"]])
  n <- profile_noise(
    sql_bin_counts(ctx$con, ctx$sql, column, breaks),
    ctx$noise
  )
  return(profile_merge_bins(breaks, n, ctx$k))
}

# Adjacent bins merge into runs that hold k each. A merged bin keeps the
# outer breaks of its run. Fewer than k values in all bins give no bin.
profile_merge_bins <- function(breaks, n, k) {
  if (sum(n) < k) {
    return(list(breaks = numeric(0), n = integer(0)))
  }
  runs <- profile_runs(n, k)
  sums <- as.integer(rowsum(as.numeric(n), runs, reorder = FALSE)[, 1])
  first <- which(!duplicated(runs))
  return(list(breaks = c(breaks[first], breaks[length(breaks)]), n = sums))
}

# Breaks from pretty() that never equal the observed minimum or maximum.
profile_breaks <- function(lo, hi) {
  breaks <- pretty(c(lo, hi), n = 20)
  breaks <- profile_drop_inner_extremes(breaks, lo, hi)
  return(profile_extend_breaks(breaks, lo, hi))
}

# pretty() can put an extreme on an inner break, for example when lo == hi.
profile_drop_inner_extremes <- function(breaks, lo, hi) {
  inner <- seq_along(breaks) > 1 & seq_along(breaks) < length(breaks)
  return(breaks[!(inner & breaks %in% c(lo, hi))])
}

# pretty() returns a round extreme exactly: c(0, 100) gives breaks 0 to 100.
# Move such an end break one step outward.
profile_extend_breaks <- function(breaks, lo, hi) {
  m <- length(breaks)
  step <- breaks[2] - breaks[1]
  if (breaks[1] >= lo) {
    breaks[1] <- breaks[1] - step
  }
  if (breaks[m] <= hi) {
    breaks[m] <- breaks[m] + step
  }
  return(breaks)
}

# joint structure ----

profile_check_pair <- function(pair, field_types, what) {
  pair <- as.character(pair)
  if (
    length(pair) != 2 ||
      pair[1] == pair[2] ||
      !all(pair %in% names(field_types))
  ) {
    stop(
      "Each element of ",
      what,
      " must name two different columns of field_types.",
      call. = FALSE
    )
  }
  return(pair)
}

profile_group_sizes <- function(ctx, group_sizes) {
  if (length(group_sizes) == 0) {
    return(list())
  }
  if (is.null(names(group_sizes)) || any(!nzchar(names(group_sizes)))) {
    stop("group_sizes must be a named list.", call. = FALSE)
  }
  out <- lapply(group_sizes, function(column) profile_group_size(ctx, column))
  return(out)
}

profile_group_size <- function(ctx, column) {
  if (length(column) != 1 || !column %in% names(ctx$field_types)) {
    stop(
      "Each element of group_sizes must name one column of field_types.",
      call. = FALSE
    )
  }
  r <- sql_group_sizes(ctx$con, ctx$sql, column)
  agg <- profile_aggregate(list(profile_as_count(r$size)), r$n)
  ranges <- profile_size_ranges(agg$keys[[1]], agg$n, ctx)
  return(c(list(column = column), ranges))
}

# Adjacent row counts merge into ranges that hold k values each. The ranges
# tile from 1 up: each ends one below the start of the next, and the last
# has no end. So no exact smallest or largest row count is published.
profile_size_ranges <- function(sizes, n, ctx) {
  n <- profile_noise(n, ctx$noise)
  runs <- profile_end_runs(profile_runs(n, ctx$k))
  cells <- profile_run_cells(as.integer(sizes), n, ctx$k, runs)
  size_from <- c(1L, cells$from[-1])[seq_along(cells$n)]
  return(list(
    size_from = size_from,
    size_to = size_from[-1] - 1L,
    n = cells$n
  ))
}

# A first run of the smallest row count alone would end on that count, and
# a last run of the largest alone would start on it. Each such run merges
# with its neighbour.
profile_end_runs <- function(runs) {
  m <- length(runs)
  if (m > 1 && sum(runs == runs[m]) == 1) {
    runs[m] <- runs[m - 1]
  }
  if (length(unique(runs)) > 1 && sum(runs == runs[1]) == 1) {
    runs[1] <- runs[2]
  }
  return(match(runs, unique(runs)))
}

profile_crosstabs <- function(ctx, crosstabs, column_profiles) {
  return(lapply(
    crosstabs,
    profile_crosstab,
    ctx = ctx,
    column_profiles = column_profiles
  ))
}

profile_crosstab <- function(ctx, pair, column_profiles) {
  pair <- profile_check_pair(pair, ctx$field_types, "crosstabs")
  for (column in pair) {
    profile_check_crosstab_column(ctx, column, column_profiles[[column]])
  }
  r <- sql_value_counts(ctx$con, ctx$sql, pair)
  keys <- lapply(1:2, function(i) profile_typed_values(ctx, pair[i], r[[i]]))
  agg <- profile_aggregate(keys, r$n)
  cells <- profile_suppress(agg$keys, agg$n, ctx)
  levels <- cells$keys
  names(levels) <- pair
  out <- list(columns = pair, levels = levels, n = cells$n)
  out$suppressed_n <- cells$suppressed_n
  return(out)
}

profile_check_crosstab_column <- function(ctx, column, entry) {
  ok <- ctx$sensitivity[[column]] == "full" &&
    (!is.null(entry$levels) || ctx$field_types[[column]] == "DATE")
  if (!ok) {
    stop(
      "A crosstab column must be full, and must have levels or be a DATE. Column: ",
      column,
      call. = FALSE
    )
  }
  return(invisible(column))
}

# Query values converted to the column's type. A DATETIME is truncated to
# the whole second.
profile_typed_values <- function(ctx, column, x) {
  values <- profile_as_type(x, ctx$field_types[[column]])
  if (inherits(values, "POSIXct")) {
    values <- .POSIXct(floor(as.numeric(values)), tz = "UTC")
  }
  return(values)
}

profile_dependencies <- function(ctx, dependencies) {
  return(lapply(dependencies, function(pair) profile_dependency(ctx, pair)))
}

profile_dependency <- function(ctx, pair) {
  pair <- profile_check_pair(pair, ctx$field_types, "dependencies")
  n_first <- sql_count_distinct(ctx$con, ctx$sql, pair[1], present = FALSE)
  n_pair <- sql_count_distinct(ctx$con, ctx$sql, pair, present = FALSE)
  return(list(columns = pair, holds = n_pair == n_first))
}
