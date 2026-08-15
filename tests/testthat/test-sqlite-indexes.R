# Index management and table metadata under SQLite.
#
# The blocks are driven through DBTable_v9 wherever DBTable_v9 has a method
# for the thing under test, so what runs is what add_indexes(),
# confirm_indexes() and info() actually call.
#
# `self$indexes` holds LOGICAL names. The database holds PHYSICAL names, one
# per table, from index_physical_name(). No block writes a physical name as a
# string literal, so the layout of that name stays free to change.

# The physical names of every index a table declares, in declaration order.
expected_index_names <- function(tab) {
  vapply(
    names(tab$indexes),
    function(i) {
      index_physical_name(
        table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
        index = i
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

test_that("confirm_indexes executes no DDL when the indexes already match", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"), ind2 = c("c"))
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection
  expected <- expected_index_names(tab)

  # The database must already hold what the code declares, or
  # confirm_indexes() is entitled to emit DDL and the block tests nothing.
  expect_identical(get_indexes(connection = con, table = "tab"), expected)

  # PRAGMA schema_version, not the index names. Comparing names cannot tell a
  # no-op apart from a drop-and-recreate: the names are identical either way.
  # schema_version increments on every schema change, so a drop and a re-add
  # move it by two and a no-op leaves it alone.
  schema_version <- function() {
    DBI::dbGetQuery(con, "PRAGMA schema_version")[[1]]
  }

  before <- schema_version()
  suppressMessages(tab$confirm_indexes())
  after <- schema_version()

  expect_identical(after, before)

  # The counter is live rather than frozen: dropping one index moves it.
  # Without this the assertion above would also pass on a PRAGMA that always
  # returned the same number.
  drop_index(connection = con, table = "tab", index = expected[2])
  expect_true(schema_version() > before)

  tab$disconnect()
})

test_that("get_indexes excludes the primary key autoindex", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"))
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection

  # The autoindex is really there. A PRIMARY KEY creates
  # sqlite_autoindex_<table>_1 on its own, and it is a row in sqlite_master
  # exactly like a user index is, so this is what get_indexes() must filter.
  all_indexes <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'tab' ORDER BY rowid"
  )$name
  expect_true("sqlite_autoindex_tab_1" %in% all_indexes)

  expected <- expected_index_names(tab)
  expect_identical(get_indexes(connection = con, table = "tab"), expected)

  # A plain character vector, not a data.frame column and not carrying
  # attributes. A caller comparing the whole vector uses identical(), which is
  # strict about both.
  retval <- get_indexes(connection = con, table = "tab")
  expect_type(retval, "character")
  expect_null(attributes(retval))
  expect_length(retval, length(tab$indexes))

  # A second table's indexes do not leak in: tbl_name restricts the query.
  DBI::dbExecute(con, "CREATE TABLE other (z TEXT)")
  DBI::dbExecute(con, "CREATE INDEX ind_other ON other (z)")
  expect_identical(get_indexes(connection = con, table = "tab"), expected)

  tab$disconnect()
})

test_that("get_table_names_and_info returns five columns on an empty database", {
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)
  con <- suppressMessages(conn$autoconnection)

  expected_names <- c(
    "table_name",
    "nrow",
    "size_total_gb",
    "size_data_gb",
    "size_index_gb"
  )

  # The empty case is the one that breaks if the result is built from a query
  # rather than column by column: a zero-row query result has no columns to
  # name.
  empty <- get_table_names_and_info(con)
  expect_s3_class(empty, "data.table")
  expect_identical(names(empty), expected_names)
  expect_identical(base::nrow(empty), 0L)

  DBI::dbExecute(con, "CREATE TABLE btab (x TEXT)")
  DBI::dbExecute(con, "CREATE TABLE atab (x TEXT)")
  DBI::dbExecute(con, "INSERT INTO atab (x) VALUES ('p'), ('q'), ('r')")

  info <- get_table_names_and_info(con)
  expect_identical(names(info), expected_names)
  expect_identical(base::nrow(info), 2L)

  # Sorted by table_name, matching the PostgreSQL method's shape.
  expect_identical(info$table_name, c("atab", "btab"))

  # Exact, not an estimate. The other two backends report a stored figure
  # that can be stale; SQLite counts.
  expect_identical(info$nrow, c(3, 0))

  # No per-table size exists: dbstat is not compiled into RSQLite's SQLite.
  expect_identical(info$size_total_gb, c(NA_real_, NA_real_))
  expect_identical(info$size_data_gb, c(NA_real_, NA_real_))
  expect_identical(info$size_index_gb, c(NA_real_, NA_real_))

  conn$disconnect()
})

test_that("a user index or table named sqlite... is not hidden by the autoindex filter", {
  # `_` is a single-character wildcard in SQL LIKE. `NOT LIKE 'sqlite_%'`
  # therefore hides every name beginning "sqlite" followed by ANY character,
  # not just the literal underscore of sqlite_autoindex_*. The filter must be
  # `NOT LIKE 'sqlite\_%' ESCAPE '\'`.
  #
  # The names below deliberately have a non-underscore character after
  # "sqlite": sqliteFoo, sqliteBar. Under the unescaped filter both vanish,
  # and the consequences are the two this block exists to prevent. A hidden
  # index named sqliteBar reads as missing to every caller; a hidden table
  # named sqliteFoo returns nothing from DBTable_v9$nrow(use_count = FALSE)
  # and DBTable_v9$info().
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"))
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection

  DBI::dbExecute(con, "CREATE INDEX sqliteBar ON tab (c)")
  DBI::dbExecute(con, "CREATE TABLE sqliteFoo (z TEXT)")
  DBI::dbExecute(con, "INSERT INTO sqliteFoo (z) VALUES ('p'), ('q')")

  # The index is visible, in creation order, and the autoindex is still gone.
  retval <- get_indexes(connection = con, table = "tab")
  expect_identical(retval, c(expected_index_names(tab), "sqliteBar"))
  expect_false("sqlite_autoindex_tab_1" %in% retval)

  # The table is visible, with its exact row count, and SQLite's own objects
  # are still gone.
  info <- get_table_names_and_info(con)
  expect_true("sqliteFoo" %in% info$table_name)
  expect_identical(info$table_name, c("sqliteFoo", "tab"))
  expect_identical(info[table_name == "sqliteFoo"]$nrow, 2)

  # sqlite_autoindex_tab_1 is genuinely present in the catalogue, so the two
  # exclusions above are real exclusions and not vacuous.
  expect_true(
    "sqlite_autoindex_tab_1" %in%
      DBI::dbGetQuery(
        con,
        "SELECT name FROM sqlite_master WHERE type = 'index'"
      )$name
  )

  tab$disconnect()
})

test_that("PostgreSQL and SQL Server still dispatch to their own methods", {
  # A regression assertion that adding a fourth class did not perturb the
  # existing three. It asserts on the method table rather than opening a
  # connection, because neither of those two backends is reachable from a
  # machine with no server.
  for (generic in list(get_indexes, drop_index, add_index)) {
    expect_true(S7::method(generic, db_postgres) |> is.function())
    expect_true(S7::method(generic, db_mssql) |> is.function())
    expect_true(S7::method(generic, db_sqlite) |> is.function())
  }

  # Distinct functions, not one method shadowing the other two.
  expect_false(identical(
    S7::method(add_index, db_postgres),
    S7::method(add_index, db_sqlite)
  ))
  expect_false(identical(
    S7::method(add_index, db_mssql),
    S7::method(add_index, db_sqlite)
  ))
  expect_false(identical(
    S7::method(get_indexes, db_postgres),
    S7::method(get_indexes, db_sqlite)
  ))

  # The S3 side of the same question: the two pre-existing
  # get_table_names_and_info methods are still registered, and the new one
  # sits beside them rather than replacing either.
  expect_true(is.function(utils::getS3method(
    "get_table_names_and_info",
    "PostgreSQL"
  )))
  expect_true(is.function(utils::getS3method(
    "get_table_names_and_info",
    "Microsoft SQL Server"
  )))
  expect_true(is.function(utils::getS3method(
    "get_table_names_and_info",
    "SQLiteConnection"
  )))
})
