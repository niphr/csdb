# DBTable_v9$tbl() attaches the primary key to the lazy table as the
# attribute csdb_pk, and collect_and_sort_by_pk() reads it after the dbplyr
# verbs that a consumer puts between tbl() and the collect.
#
# The rows go in shuffled, so a collect without a sort returns them in an
# order that differs from the key order (b, a) and from the column order
# (a, b, v).
pk_table <- function(keys = c("b", "a"), .local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "pk_tab",
    field_types = c(a = "TEXT", b = "INTEGER", v = "DOUBLE"),
    keys = keys
  )
  suppressMessages(tab$connect())
  withr::defer(tab$disconnect(), envir = .local_envir)
  suppressMessages(tab$insert_data(data.table::data.table(
    a = c("y", "x", "y", "x", "z"),
    b = c(2L, 2L, 1L, 1L, 1L),
    v = c(5, 4, 3, 2, 1)
  )))
  return(tab)
}

# The key order (b, a) of the five rows, as the values of v.
v_by_pk <- c(2, 3, 1, 4, 5)

test_that("collect_and_sort_by_pk returns a data.table ordered by the pk", {
  tab <- pk_table()
  d <- collect_and_sort_by_pk(tab$tbl())

  expect_true(data.table::is.data.table(d))
  expect_identical(d$b, c(1L, 1L, 1L, 2L, 2L))
  expect_identical(d$a, c("x", "y", "z", "x", "y"))
  expect_identical(d$v, v_by_pk)
})

test_that("tbl() attaches the keys of the table as the attribute csdb_pk", {
  tab <- pk_table()
  expect_identical(attr(tab$tbl(), "csdb_pk"), c("b", "a"))
})

test_that("the pk survives filter()", {
  tab <- pk_table()
  q <- tab$tbl() |> dplyr::filter(v > 1)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  expect_identical(collect_and_sort_by_pk(q)$v, c(2, 3, 4, 5))
})

test_that("the pk survives select()", {
  tab <- pk_table()
  q <- tab$tbl() |> dplyr::select(v, a, b)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  d <- collect_and_sort_by_pk(q)
  expect_identical(names(d), c("v", "a", "b"))
  expect_identical(d$v, v_by_pk)
})

test_that("the pk survives mutate()", {
  tab <- pk_table()
  q <- tab$tbl() |> dplyr::mutate(w = -v)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  expect_identical(collect_and_sort_by_pk(q)$w, -v_by_pk)
})

test_that("the pk survives group_by() and summarise()", {
  tab <- pk_table()
  q <- tab$tbl() |>
    dplyr::group_by(b) |>
    dplyr::summarise(s = sum(v, na.rm = TRUE))
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  d <- collect_and_sort_by_pk(q)
  expect_identical(d$b, c(1L, 2L))
  expect_identical(d$s, c(6, 9))
})

test_that("the pk survives arrange(), and the pk order wins over it", {
  tab <- pk_table()
  q <- tab$tbl() |> dplyr::arrange(dplyr::desc(v))
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  expect_identical(collect_and_sort_by_pk(q)$v, v_by_pk)
})

test_that("the pk survives left_join() and inner_join() on the x side", {
  tab <- pk_table()
  other <- data.frame(a = c("x", "y"), label = c("ex", "why"))
  con <- tab$dbconnection$autoconnection
  DBI::dbWriteTable(con, "other", other)
  y <- dplyr::tbl(con, "other")

  q_left <- tab$tbl() |> dplyr::left_join(y, by = "a")
  expect_identical(attr(q_left, "csdb_pk"), c("b", "a"))
  d <- collect_and_sort_by_pk(q_left)
  expect_identical(d$v, v_by_pk)
  expect_identical(d$label, c("ex", "why", NA, "ex", "why"))

  q_inner <- tab$tbl() |> dplyr::inner_join(y, by = "a")
  expect_identical(attr(q_inner, "csdb_pk"), c("b", "a"))
  expect_identical(collect_and_sort_by_pk(q_inner)$v, c(2, 3, 4, 5))
})

test_that("the pk survives distinct()", {
  tab <- pk_table()
  q <- tab$tbl() |> dplyr::distinct(a, b)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  d <- collect_and_sort_by_pk(q)
  expect_identical(d$b, c(1L, 1L, 1L, 2L, 2L))
  expect_identical(d$a, c("x", "y", "z", "x", "y"))
})

test_that("the pk survives head()", {
  tab <- pk_table()
  q <- tab$tbl() |>
    dplyr::arrange(v) |>
    head(3)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  # head() keeps v = 1, 2 and 3. In key order those are 2, 3, 1.
  expect_identical(collect_and_sort_by_pk(q)$v, c(2, 3, 1))
})

test_that("an aggregation that drops a pk column orders by the pk left, then the rest", {
  tab <- pk_table()
  q <- tab$tbl() |>
    dplyr::group_by(a) |>
    dplyr::summarise(s = sum(v, na.rm = TRUE)) |>
    dplyr::select(s, a)
  expect_identical(attr(q, "csdb_pk"), c("b", "a"))
  d <- collect_and_sort_by_pk(q)
  # The order is by a, the pk column that is left. By column order, s first,
  # it would be 1, 6, 8.
  expect_identical(d$a, c("x", "y", "z"))
  expect_identical(d$s, c(6, 8, 1))
})

test_that("collect_and_sort_by_pk stops when x carries no pk", {
  tab <- pk_table()
  con <- tab$dbconnection$autoconnection

  # The query did not start at tbl().
  expect_error(
    collect_and_sort_by_pk(dplyr::tbl(con, "pk_tab")),
    "`x` carries no primary key"
  )

  # The csdb table is on the y side of a join.
  DBI::dbWriteTable(con, "other", data.frame(a = "x"))
  expect_error(
    collect_and_sort_by_pk(
      dplyr::left_join(dplyr::tbl(con, "other"), tab$tbl(), by = "a")
    ),
    "`x` carries no primary key"
  )

  # The table has no keys.
  no_keys <- pk_table(keys = NULL)
  expect_null(attr(no_keys$tbl(), "csdb_pk"))
  expect_error(
    collect_and_sort_by_pk(no_keys$tbl()),
    "`x` carries no primary key"
  )
})
