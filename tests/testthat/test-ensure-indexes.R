# A table that exists without its declared indexes gets them on the first call
# that reaches it, once per object.
#
# Blocks 1, 2, 4 and 5 run on SQLite through DBTable_v9, so what runs is what
# a caller reaches. Block 3 calls the PostgreSQL and SQL Server ensure_index
# methods directly on DBI::ANSI(), with DBI mocked, and reads the SQL they
# send. Neither backend runs here.

ensure_fields <- c(a = "TEXT", b = "INTEGER", c = "DOUBLE")

# Create `tab` with two rows and no index, then close the connection. This is
# the state of a table that a caller without `indexes` created.
ensure_plain_table <- function(cfg) {
  plain <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = ensure_fields,
    keys = c("a", "b")
  )
  suppressMessages(plain$insert_data(data.table::data.table(
    a = c("x", "y"),
    b = 1:2,
    c = c(1.5, 2.5)
  )))
  plain$disconnect()
}

# A second object on the same table, which declares `indexes`.
ensure_reopen <- function(cfg, indexes) {
  DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = ensure_fields,
    keys = c("a", "b"),
    indexes = indexes
  )
}

# Every index on `tab`, as PRAGMA index_list reports it.
ensure_index_list <- function(tab) {
  DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    "PRAGMA index_list('tab')"
  )$name
}

ensure_physical <- function(i) {
  index_physical_name(table = DBI::Id(table = "tab"), index = i)
}

test_that("an existing table gets both declared indexes on the first call", {
  cfg <- sqlite_dbconfig()
  ensure_plain_table(cfg)
  tab <- ensure_reopen(cfg, list(ind1 = c("a", "c"), ind2 = "c"))
  expected <- c(ensure_physical("ind1"), ensure_physical("ind2"))

  # The table exists and holds neither index before the call.
  expect_false(any(expected %in% ensure_index_list(tab)))

  rows <- suppressMessages(dplyr::collect(tab$tbl()))
  expect_identical(nrow(rows), 2L)

  expect_true(all(expected %in% ensure_index_list(tab)))
  con <- tab$dbconnection$autoconnection
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = expected[1]),
    c("a", "c")
  )
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = expected[2]),
    "c"
  )

  tab$disconnect()
})

test_that("a second call on the same object does no index work", {
  cfg <- sqlite_dbconfig()
  ensure_plain_table(cfg)
  tab <- ensure_reopen(cfg, list(ind1 = c("a", "c"), ind2 = "c"))

  # Every statement csdb sends through DBI is recorded, and then run.
  statements <- character(0)
  real_execute <- DBI::dbExecute
  real_get_query <- DBI::dbGetQuery
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      statements <<- c(statements, as.character(statement))
      real_execute(conn, statement, ...)
    },
    dbGetQuery = function(conn, statement, ...) {
      statements <<- c(statements, as.character(statement))
      real_get_query(conn, statement, ...)
    },
    .package = "DBI"
  )
  index_work <- function() {
    sum(grepl("index", statements, ignore.case = TRUE))
  }

  suppressMessages(dplyr::collect(tab$tbl()))
  # The counter is live: the first call read the catalogue and created both
  # indexes. Without this the zero below could come from a dead counter.
  expect_gt(index_work(), 0L)
  expect_true(any(grepl("CREATE INDEX", statements, fixed = TRUE)))

  statements <- character(0)
  rows <- suppressMessages(dplyr::collect(tab$tbl()))
  expect_identical(nrow(rows), 2L)
  expect_identical(index_work(), 0L)

  # A direct create_table() call skips lazy_creation_of_table(). It still
  # does no index work on this object.
  statements <- character(0)
  suppressMessages(tab$create_table())
  expect_identical(index_work(), 0L)

  tab$disconnect()
})

test_that("the SQL Server and PostgreSQL statements use the physical name", {
  con <- DBI::ANSI()
  keys <- c("a", "c")

  sent <- character(0)
  params <- list()
  rows_found <- 0L
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      sent <<- c(sent, as.character(statement))
      0L
    },
    dbGetQuery = function(conn, statement, ...) {
      sent <<- c(sent, as.character(statement))
      params <<- list(...)$params
      data.frame(indexname = rep("x", rows_found))
    },
    .package = "DBI"
  )

  # SQL Server. The identity is the bare table name, as DBTable_v9 gives it.
  mssql_physical <- index_physical_name(table = "tab", index = "ind1")
  out <- S7::method(ensure_index, db_mssql)(con, "tab", mssql_physical, keys)
  expect_identical(out, NA)
  expect_identical(
    sent,
    paste0(
      "IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = N'",
      mssql_physical,
      "' AND object_id = OBJECT_ID(N'\"tab\"')) CREATE INDEX \"",
      mssql_physical,
      "\" ON \"tab\" (\"a\", \"c\");"
    )
  )

  # PostgreSQL, index absent: one catalogue read by the physical name, then
  # one create under that name.
  id <- DBI::Id(schema = "anon", table = "tab")
  pg_physical <- index_physical_name(table = id, index = "ind1")
  sent <- character(0)
  out <- S7::method(ensure_index, db_postgres)(con, id, pg_physical, keys)
  expect_true(out)
  expect_length(sent, 2L)
  expect_match(sent[1], "from pg_indexes", fixed = TRUE)
  expect_identical(params, list(pg_physical, "tab", "anon"))
  expect_identical(
    sent[2],
    paste0(
      "CREATE INDEX IF NOT EXISTS \"",
      pg_physical,
      "\" ON \"anon\".\"tab\" (\"a\", \"c\")"
    )
  )

  # PostgreSQL, index present: the catalogue read alone, and no DDL.
  sent <- character(0)
  rows_found <- 1L
  out <- S7::method(ensure_index, db_postgres)(con, id, pg_physical, keys)
  expect_false(out)
  expect_length(sent, 1L)
  expect_match(sent, "from pg_indexes", fixed = TRUE)
})

test_that("an index that cannot be created warns and the call returns data", {
  cfg <- sqlite_dbconfig()
  ensure_plain_table(cfg)
  tab <- ensure_reopen(cfg, list(ind1 = "no_such_column", ind2 = "c"))

  # The outcome is captured as a value, so an error, a warning and the
  # returned rows each meet their own assertion. An error therefore fails
  # expect_null() below, and does not end the block before it.
  out <- list(error = NULL, warnings = character(0), value = NULL)
  out$value <- tryCatch(
    withCallingHandlers(
      suppressMessages(dplyr::collect(tab$tbl())),
      warning = function(w) {
        out$warnings <<- c(out$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      out$error <<- conditionMessage(e)
      NULL
    }
  )

  expect_null(out$error)
  expect_length(out$warnings, 1L)
  expect_match(
    out$warnings,
    paste0(
      "Index ind1 was not created on table tab. Its name there is ",
      ensure_physical("ind1"),
      ". no such column"
    ),
    fixed = TRUE,
    all = FALSE
  )
  expect_identical(nrow(out$value), 2L)

  # The failure is that one index. The next declared index is still created.
  after <- ensure_index_list(tab)
  expect_false(ensure_physical("ind1") %in% after)
  expect_true(ensure_physical("ind2") %in% after)

  tab$disconnect()
})

test_that("an index under an old bare name stays beside the new one", {
  cfg <- sqlite_dbconfig()
  ensure_plain_table(cfg)

  # The state a release before 2026.10.4 left: the logical name, verbatim.
  old <- DBI::dbConnect(RSQLite::SQLite(), cfg$db)
  DBI::dbExecute(old, "CREATE INDEX ind1 ON tab (a, c)")
  DBI::dbDisconnect(old)

  tab <- ensure_reopen(cfg, list(ind1 = c("a", "c")))
  suppressMessages(dplyr::collect(tab$tbl()))

  after <- ensure_index_list(tab)
  expect_true("ind1" %in% after)
  expect_true(ensure_physical("ind1") %in% after)
  expect_identical(
    get_index_columns(
      connection = tab$dbconnection$autoconnection,
      table = "tab",
      index = "ind1"
    ),
    c("a", "c")
  )

  tab$disconnect()
})
