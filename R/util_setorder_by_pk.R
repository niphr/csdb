#' Order a data.table by the primary key of a table
#'
#' `setorder_by_pk()` orders `d` by reference. It sorts first on the primary
#' key columns of `table`, and then on every other column of `d`.
#'
#' @details
#' A database does not define the order of the rows that a query returns.
#' PostgreSQL can return the same rows in a different order on each call.
#' Code that reads `d[1]`, `d[.N]` or `tail(d, 1)` after `dplyr::collect()`
#' therefore gets an arbitrary row. Call `setorder_by_pk()` on the result to
#' make that row deterministic.
#'
#' The sort columns are, in this order:
#'
#' 1. the columns of `table$keys` that `d` has, in the order of `table$keys`.
#' 2. every other column of `d`, in the column order of `d`.
#'
#' The function ignores a column of `table$keys` that `d` does not have. With
#' `table = NULL`, or with `table$keys` equal to `NULL`, the function sorts on
#' every column of `d`. Each column sorts in ascending order, with `NA` first.
#' A list column or a complex column is no sort column, because
#' [data.table::setorderv()] cannot sort it.
#'
#' The function matches the key by column name only. The result is always in
#' one fixed order. It is the primary key order only while the key columns
#' keep their names and values.
#'
#' For a query that starts at [DBTable_v9]`$tbl()`, use
#' [collect_and_sort_by_pk()]. It collects and sorts in one step.
#'
#' @param d A data.table. The function changes the row order of `d` by
#'   reference. A data.frame or a tibble stops with an error, because the
#'   function cannot change it by reference. Convert it first with
#'   [data.table::setDT()].
#' @param table `NULL`, or an object with a field `keys` that holds the primary
#'   key as a character vector. A [DBTable_v9] has this field. A plain
#'   `list(keys = c("b", "a"))` also works.
#' @return `d`, sorted. The return value is the same object as the argument
#'   `d`, so the function works at the end of a pipe and in an assignment.
#' @examples
#' d <- data.table::data.table(
#'   a = c(2L, 1L, 2L, 1L),
#'   b = c("y", "y", "x", "x"),
#'   v = c(4, 3, 2, 1)
#' )
#' setorder_by_pk(d, list(keys = c("b", "a")))
#' d
#'
#' # Without a table, the function sorts on every column.
#' setorder_by_pk(d)
#' @md
#' @family row order
#' @export
setorder_by_pk <- function(d, table = NULL) {
  if (!data.table::is.data.table(d)) {
    stop(
      "`d` must be a data.table, but it has class ",
      paste(class(d), collapse = "/"),
      ". Convert it first with data.table::setDT().",
      call. = FALSE
    )
  }
  keys <- if (is.null(table)) NULL else table[["keys"]]
  if (!is.null(keys) && !is.character(keys)) {
    stop(
      "`table$keys` must be a character vector or NULL, but it has class ",
      paste(class(keys), collapse = "/"),
      ".",
      call. = FALSE
    )
  }
  # setorderv() cannot sort a list or a complex column, so those are no
  # sort columns. A database result never has one.
  sortable <- names(d)[vapply(
    d,
    function(x) !is.list(x) && !is.complex(x),
    logical(1)
  )]
  pk <- unique(keys[keys %in% sortable])
  cols <- c(pk, setdiff(sortable, pk))
  if (length(cols) > 0) {
    data.table::setorderv(d, cols, na.last = FALSE)
  }
  return(d)
}

#' Collect a query and order it by the primary key of its table
#'
#' `collect_and_sort_by_pk()` replaces [dplyr::collect()] for a query that
#' starts at [DBTable_v9]`$tbl()`. It returns a data.table, ordered by the
#' primary key of that table and then by every other column.
#'
#' @details
#' PostgreSQL returns rows in no fixed order. The same query can return the
#' same rows in a different order on each call. Code that reads `d[1]`,
#' `d[.N]` or `tail(d, 1)` after `dplyr::collect()` therefore gets an arbitrary
#' row. `collect_and_sort_by_pk()` makes that row deterministic.
#'
#' [DBTable_v9]`$tbl()` attaches the primary key of the table to the lazy table
#' as the attribute `csdb_pk`. These dbplyr verbs keep the attribute on their
#' result: `filter()`, `select()`, `mutate()`, `group_by()`, `summarise()`,
#' `arrange()`, `distinct()`, `head()`, and `left_join()` and `inner_join()` on
#' the `x` side.
#'
#' The function sorts with [setorder_by_pk()]. The primary key columns that
#' the result has come first, in primary key order. Every other column follows,
#' in column order. A primary key column that a `select()` or a `summarise()`
#' removed is not a sort column.
#'
#' The attribute holds column names. After a `rename()` or a `mutate()` of a
#' primary key column, the order is still fixed, but it is no longer the
#' primary key order.
#'
#' The sort runs after the collect. It cannot choose which rows a `head()` or
#' another limit inside the query returns. Put a `dplyr::arrange()` before the
#' limit when the rows matter.
#'
#' The function stops with an error when `x` carries no primary key. It does
#' not fall back to an unsorted collect. These are the causes:
#'
#' - the query did not start at [DBTable_v9]`$tbl()`.
#' - the table has no primary key, because its `keys` is `NULL`.
#' - a step dropped the attribute, for example [dplyr::compute()], or a join
#'   where the csdb table is on the `y` side.
#'
#' For a result without a primary key, for example from [DBI::dbGetQuery()],
#' use [setorder_by_pk()] with `table = NULL`.
#'
#' @param x A lazy table that starts at [DBTable_v9]`$tbl()`.
#' @return A data.table with the rows of `x`, sorted as described in Details.
#' @examples
#' \donttest{
#' # SQLite needs no server, so this block runs anywhere.
#' my_table <- DBTable_v9$new(
#'   dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
#'   table_name = "my_data_table",
#'   field_types = c("id" = "INTEGER", "week" = "INTEGER", "n" = "DOUBLE"),
#'   keys = c("id", "week")
#' )
#' my_table$insert_data(data.table::data.table(
#'   id = c(2L, 1L, 2L, 1L),
#'   week = c(2L, 2L, 1L, 1L),
#'   n = c(4, 3, 2, 1)
#' ))
#'
#' d <- my_table$tbl() |>
#'   dplyr::filter(id == 1L) |>
#'   collect_and_sort_by_pk()
#'
#' # The last row is the last week of id 1, on every database.
#' d[.N]
#'
#' my_table$disconnect()
#' }
#' @md
#' @family row order
#' @export
collect_and_sort_by_pk <- function(x) {
  pk <- attr(x, "csdb_pk", exact = TRUE)
  if (is.null(pk)) {
    stop(
      "`x` carries no primary key, so collect_and_sort_by_pk() cannot sort ",
      "it. The query did not start at a csdb table's $tbl(), the table has ",
      "no keys, or a step such as compute() or a join with the csdb table ",
      "on the y side dropped the key. Use dplyr::collect() and then ",
      "setorder_by_pk() instead.",
      call. = FALSE
    )
  }
  d <- dplyr::collect(x)
  data.table::setDT(d)
  return(setorder_by_pk(d, list(keys = pk)))
}
