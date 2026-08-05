# SQLite is the one backend csdb can test without a server, so these blocks
# are the first that reach connect_once(), the identifier block in
# DBTable_v9$initialize() and a create_table method at all.

test_that("a SQLite connection opens and reports connected", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)

  expect_false(db$is_connected())
  db$connect()
  expect_true(db$is_connected())

  # The generic ODBC else-branch would have produced an odbc connection, or
  # more likely no connection at all, so the class is what proves which branch
  # ran.
  expect_s4_class(db$connection, "SQLiteConnection")
  # The file the helper named is the file that now exists. This also confirms
  # withr did not delete it when the helper returned.
  expect_true(file.exists(cfg$db))

  # extended_types is load-bearing: without it a DATE column reads back as an
  # integer and the csfmt validators reject it.
  expect_true(db$connection@extended_types)

  db$disconnect()
  expect_false(db$is_connected())
})

test_that("the driver string is matched case-insensitively", {
  for (spelling in c("sqlite", "SQLite", "SQLITE")) {
    cfg <- sqlite_dbconfig(driver = spelling)

    db <- sqlite_connection(cfg)
    db$connect()
    expect_true(db$is_connected(), info = spelling)
    expect_true(
      methods::is(db$connection, "SQLiteConnection"),
      info = spelling
    )
    db$disconnect()

    # The identifier block in DBTable_v9$initialize() has to agree with
    # connect_once() about what counts as SQLite, so both are covered here.
    tab <- DBTable_v9$new(
      dbconfig = cfg,
      table_name = "tab",
      field_types = c(a = "TEXT", b = "INTEGER"),
      keys = "a"
    )
    expect_identical(
      tab$table_name_fully_specified_text,
      "tab",
      info = spelling
    )
    suppressMessages(tab$connect())
    expect_true(
      "tab" %in% DBI::dbListTables(tab$dbconnection$connection),
      info = spelling
    )
    tab$disconnect()
  }
})

test_that("create_table inlines the primary key on the key columns in order", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(
      a = "TEXT",
      b = "INTEGER",
      c = "DOUBLE",
      d = "BOOLEAN",
      e = "DATE",
      f = "DATETIME"
    ),
    # Deliberately not the column order, and not alphabetical: the key order
    # is the order declared here, and PRAGMA reports it as 1-based position.
    keys = c("b", "a")
  )
  suppressMessages(tab$connect())

  info <- DBI::dbGetQuery(
    tab$dbconnection$connection,
    "PRAGMA table_info(`tab`)"
  )

  # The columns are exactly names(field_types), in order.
  expect_identical(info$name, c("a", "b", "c", "d", "e", "f"))
  # ... and carry the mapped SQLite types.
  expect_identical(
    info$type,
    c("TEXT", "INTEGER", "REAL", "INTEGER", "DATE", "DATETIME")
  )

  # pk is 0 for a non-key column and the 1-based position within the primary
  # key for a key column.
  keyed <- info[info$pk > 0, ]
  expect_identical(keyed$name[order(keyed$pk)], c("b", "a"))
  expect_identical(
    as.integer(info$pk),
    c(2L, 1L, 0L, 0L, 0L, 0L)
  )

  tab$disconnect()
})

test_that("key columns are NOT NULL", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b")
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$connection

  info <- DBI::dbGetQuery(con, "PRAGMA table_info(`tab`)")
  expect_identical(as.integer(info$notnull), c(1L, 1L, 0L))

  # A non-key column still takes NULL.
  expect_silent(
    DBI::dbExecute(
      con,
      "INSERT INTO `tab` (`a`, `b`, `c`) VALUES ('x', 1, NULL)"
    )
  )
  # A key column does not.
  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO `tab` (`a`, `b`, `c`) VALUES (NULL, 2, 1.5)"
    ),
    "NOT NULL"
  )
  expect_error(
    DBI::dbExecute(
      con,
      "INSERT INTO `tab` (`a`, `b`, `c`) VALUES ('y', NULL, 1.5)"
    ),
    "NOT NULL"
  )

  tab$disconnect()
})

test_that("schema is ignored when it arrives as an empty string", {
  # cs9 builds every dbconfig from Sys.getenv(), so schema is "" and not NULL.
  # Routed through the shared paste(c(schema, table_name), collapse = ".") the
  # name would come out as ".tab", and str_remove_all("\\[]\\.") does not
  # strip a leading dot.
  cfg <- sqlite_dbconfig(schema = "")
  expect_identical(cfg$schema, "")

  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a"
  )

  expect_identical(tab$table_name_fully_specified_text, "tab")
  expect_identical(
    tab$table_name_short_for_mssql_fully_specified_for_postgres_text,
    "tab"
  )
  expect_false(startsWith(tab$table_name_fully_specified_text, "."))
  expect_false(
    startsWith(
      tab$table_name_short_for_mssql_fully_specified_for_postgres_text,
      "."
    )
  )

  expect_s4_class(tab$table_name_fully_specified, "Id")
  expect_identical(tab$table_name_fully_specified@name, c(table = "tab"))
  expect_identical(
    tab$table_name_short_for_mssql_fully_specified_for_postgres@name,
    c(table = "tab")
  )

  # And the name the database ends up with carries no dot either.
  suppressMessages(tab$connect())
  expect_identical(DBI::dbListTables(tab$dbconnection$connection), "tab")
  tab$disconnect()
})

test_that("an unsupported field type is rejected", {
  cfg <- sqlite_dbconfig()
  # " (100)" with a space is stripped by DBTable_v9$initialize(); "(100)"
  # without one is not, so VARCHAR(100) reaches create_table intact. SQLite
  # would otherwise accept it, give the column BLOB affinity, and say nothing.
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(x = "VARCHAR(100)"),
    keys = "x"
  )

  expect_error(
    suppressMessages(tab$connect()),
    "x \\(VARCHAR\\(100\\)\\)"
  )
  # Nothing was created.
  expect_identical(
    DBI::dbListTables(tab$dbconnection$connection),
    character(0)
  )

  # TEXT(100) without the space is rejected for the same reason.
  cfg2 <- sqlite_dbconfig()
  tab2 <- DBTable_v9$new(
    dbconfig = cfg2,
    table_name = "tab",
    field_types = c(x = "TEXT(100)"),
    keys = "x"
  )
  expect_error(
    suppressMessages(tab2$connect()),
    "x \\(TEXT\\(100\\)\\)"
  )

  # "TEXT (100)" with the space is what DBTable_v9 strips, and is accepted.
  cfg3 <- sqlite_dbconfig()
  tab3 <- DBTable_v9$new(
    dbconfig = cfg3,
    table_name = "tab",
    field_types = c(x = "TEXT (100)"),
    keys = "x"
  )
  suppressMessages(tab3$connect())
  expect_identical(
    DBI::dbGetQuery(
      tab3$dbconnection$connection,
      "PRAGMA table_info(`tab`)"
    )$type,
    "TEXT"
  )
  tab3$disconnect()

  tab$disconnect()
  tab2$disconnect()
})
