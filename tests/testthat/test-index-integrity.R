# A declared index exists on its own table, with the declared columns, or the
# call raises. There is no third outcome.
#
# Every block runs against SQLite in a temporary file, so the file is portable
# and needs no server. Two blocks call an S7 method directly, against a SQLite
# connection. That is deliberate. This release changes the db_default and the
# db_postgres method bodies. A block driven through DBTable_v9 under SQLite
# dispatches to db_sqlite, and would test neither one. SQLite accepts the
# db_postgres SQL, so the real body runs.
#
# No block writes an expected physical index name as a string literal. Every
# one asks index_physical_name() for it. Each then asserts a property of the
# answer: distinct, lowercase, inside the identifier limit, and the same name
# at create and at drop. A block that pinned the layout would make the layout
# impossible to change.

# The physical names of every index a table declares, in declaration order.
#
# The identity is the field the production helper uses,
# table_name_short_for_mssql_fully_specified_for_postgres, and not the _text
# field beside it. A block below asserts that the two agree under SQLite.
expected_physical_names <- function(tab) {
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

test_that("the PostgreSQL add_index method raises instead of returning the error", {
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

  pg_add <- S7::method(add_index, db_postgres)

  # Until 2026.8.16 this returned a `try-error` object holding the same text,
  # and returned it invisibly enough that no caller looked.
  expect_error(
    pg_add(con, "tab", "bad_index", "no_such_column"),
    "no such column"
  )
  expect_false("bad_index" %in% get_indexes(connection = con, table = "tab"))

  # The method still creates an index. Without this the block above would
  # also pass on a method that can never do anything at all.
  pg_add(con, "tab", "good_index", "c")
  expect_true("good_index" %in% get_indexes(connection = con, table = "tab"))

  tab$disconnect()
})

test_that("the default add_index method raises instead of returning the error", {
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

  def_add <- S7::method(add_index, db_default)

  # This method emits MySQL syntax, `ALTER TABLE ... ADD INDEX`, which SQLite
  # rejects. The column below is a real column, so the rejection comes from
  # the statement and not from the argument. That is enough to show the
  # failure leaves the method body, which is the property under test. It says
  # nothing about whether the SQL is right for MySQL.
  expect_error(def_add(con, "tab", "bad_index", "c"), "syntax error")
  expect_false("bad_index" %in% get_indexes(connection = con, table = "tab"))

  # A different method, not one shadowing the other.
  expect_false(identical(
    S7::method(add_index, db_default),
    S7::method(add_index, db_postgres)
  ))

  tab$disconnect()
})

test_that("the temporary index name is built with paste0 and is usable", {
  # `+` does not join strings in R, and csdb defines no method for it. R
  # evaluated the old expression lazily, inside the glue() inside the try(),
  # so the PostgreSQL upsert built every temporary table with no index and
  # reported nothing.
  expect_error(
    "ind" + random_uuid(),
    "non-numeric argument to binary operator"
  )

  nm <- paste0("ind", random_uuid())
  expect_type(nm, "character")
  expect_length(nm, 1)

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

  # Usable, not merely a string: the database accepts it as an index name.
  S7::method(add_index, db_postgres)(con, "tab", nm, "c")
  expect_true(nm %in% get_indexes(connection = con, table = "tab"))

  tab$disconnect()
})

test_that("the PostgreSQL upsert hands add_index the identifier it quotes", {
  # The PostgreSQL add_index method quotes every identifier itself from
  # 2026.8.16, so it needs the DBI::Id and not the pre-quoted text. Until then
  # it pasted the table in raw, and a DBI::Id raised inside glue::glue().
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

  # A DBI::Id is what the method needs now, and it creates a real index.
  S7::method(add_index, db_postgres)(
    con,
    DBI::Id(table = "tab"),
    "ix_from_an_id",
    "c"
  )
  expect_true(
    "ix_from_an_id" %in% get_indexes(connection = con, table = "tab")
  )

  tab$disconnect()

  # The upsert method needs a PostgreSQL server, so this block reads the call
  # it makes rather than running it. `temp_name` is the DBI::Id, and
  # add_index() quotes it.
  find_call <- function(x, name) {
    if (!is.call(x)) {
      return(list())
    }
    if (identical(x[[1]], as.name(name))) {
      return(list(x))
    }
    unlist(
      lapply(as.list(x), find_call, name = name),
      recursive = FALSE
    )
  }

  found <- find_call(
    body(S7::method(upsert_load_data_infile, db_postgres)),
    "add_index"
  )
  expect_length(found, 1)
  expect_identical(found[[1]]$table, as.name("temp_name"))
  expect_identical(found[[1]]$index[[1]], as.name("paste0"))
})

test_that("add_indexes creates each declared index exactly once on a new table", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"), ind2 = "c")
  )

  # No connect() call. The table must not exist, so add_indexes() reaches
  # create_table() through lazy_creation_of_table(), and create_table() ends
  # by calling add_indexes() again. That is the re-entrant path.
  seen <- character(0)
  real_add_index <- add_index
  local_mocked_bindings(
    add_index = function(connection, table, index, keys) {
      seen <<- c(seen, index)
      real_add_index(
        connection = connection,
        table = table,
        index = index,
        keys = keys
      )
    }
  )
  suppressMessages(tab$add_indexes())

  # Each declared index once, in declaration order. Before 2026.8.16 this was
  # four entries, ind1 and ind2 twice each. The names are the physical names,
  # because add_indexes() passes those to add_index().
  expected <- expected_physical_names(tab)
  expect_identical(seen, expected)

  # The indexes are really there, so the count above counts real work.
  expect_identical(
    get_indexes(
      connection = tab$dbconnection$autoconnection,
      table = "tab"
    ),
    expected
  )

  # A later call adds each one once again. The guard resets, so it does not
  # turn every following call into a silent no-op.
  seen <- character(0)
  suppressMessages(tab$add_indexes())
  expect_identical(seen, expected)

  tab$disconnect()
})

test_that("add_indexes announces each index once on a new table", {
  # The same count, taken without mocking anything. add_indexes() emits one
  # "Adding index" message per loop iteration, so the messages count the
  # iterations of the real method.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"), ind2 = "c")
  )

  msgs <- capture_messages(tab$add_indexes())

  expect_identical(sum(grepl("^Adding index", msgs)), 2L)
  expect_identical(sum(grepl("^Creating table", msgs)), 1L)

  tab$disconnect()
})

test_that("a declared index on a missing column raises through DBTable_v9", {
  # The public boundary, not a method in isolation. Under SQLite this
  # dispatches to the db_sqlite method, which never wrapped its call, so the
  # block pins the invariant end to end rather than the try() removal.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "no_such_column")
  )

  expect_error(suppressMessages(tab$add_indexes()), "no such column")

  # Twice. The guard is released on the way out, so the second call reports
  # the same failure instead of returning quietly.
  expect_error(suppressMessages(tab$add_indexes()), "no such column")

  # The table itself was created, so the failure is the index and not the
  # table.
  expect_true(tab$table_exists())
  expect_identical(
    get_indexes(
      connection = tab$dbconnection$autoconnection,
      table = "tab"
    ),
    character(0)
  )

  tab$disconnect()
})

# ---------------------------------------------------------------------------
# A declared index reaches its own table
# ---------------------------------------------------------------------------

test_that("two tables in one database both declaring ind1 both get an index", {
  # The motivating defect. csdb named an index with the caller's logical name,
  # verbatim. A PostgreSQL index name is unique per schema, and a SQLite index
  # name is unique per database. The second table therefore asked for a name
  # the first already held. `CREATE INDEX IF NOT EXISTS` answers a taken name
  # with a notice, not an error, so the second table ended with no index.
  #
  # Measured on the norsyss_data1 database on 2026-08-15: `anon_norsyss_data`
  # had 87 partitions in schema `anon`, all declaring `ind1` and `ind2`. One
  # partition held `ind2`, and none held `ind1`.
  cfg <- sqlite_dbconfig()
  tab1 <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab1",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )
  tab2 <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab2",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )

  suppressMessages(tab1$add_indexes())

  # The second call raised under the old naming. add_indexes() reads the
  # catalogue back, and the index it asked for landed on no table at all.
  expect_no_error(suppressMessages(tab2$add_indexes()))

  con1 <- tab1$dbconnection$autoconnection
  con2 <- tab2$dbconnection$autoconnection

  # Both tables live in one database file, so they share one index namespace.
  # Without this the block would pass on two unrelated databases and prove
  # nothing.
  expect_true(all(c("tab1", "tab2") %in% DBI::dbListTables(con1)))

  all_names <- c(
    get_indexes(connection = con1, table = "tab1"),
    get_indexes(connection = con2, table = "tab2")
  )

  # Two indexes, not one. Under the old naming this vector held one name.
  expect_length(all_names, 2L)

  # Distinct, lowercase, and inside the PostgreSQL identifier limit.
  expect_length(unique(all_names), 2L)
  expect_identical(all_names, tolower(all_names))
  expect_true(all(nchar(all_names, type = "bytes") <= INDEX_NAME_MAX_CHARS))

  # Each index sits on its own table with the declared column.
  expect_identical(
    get_index_columns(connection = con1, table = "tab1", index = all_names[1]),
    "c"
  )
  expect_identical(
    get_index_columns(connection = con2, table = "tab2", index = all_names[2]),
    "c"
  )

  tab1$disconnect()
  tab2$disconnect()
})

test_that("add_indexes, drop_indexes and confirm_indexes agree on one name", {
  # The round trip. A rename that reaches creation but not dropping leaves an
  # index that nobody can remove.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"), ind2 = "c")
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection
  expected <- expected_physical_names(tab)

  expect_identical(get_indexes(connection = con, table = "tab"), expected)

  suppressMessages(tab$drop_indexes())
  expect_identical(
    get_indexes(connection = con, table = "tab"),
    character(0)
  )

  suppressMessages(tab$confirm_indexes())
  expect_identical(get_indexes(connection = con, table = "tab"), expected)

  # The declared columns, in the declared order.
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

test_that("confirm_indexes adds what is missing and leaves the rest alone", {
  # confirm_indexes() dropped every index and re-added it on any mismatch.
  # After the rename that comparison could never match, so it would have
  # dropped and re-added on every call, forever.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"), ind2 = "c")
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection
  expected <- expected_physical_names(tab)

  # An index csdb did not name, which confirm_indexes() must ignore.
  DBI::dbExecute(con, "CREATE INDEX `hand_rolled` ON `tab` (`b`)")
  drop_index(connection = con, table = "tab", index = expected[2])
  expect_identical(
    get_indexes(connection = con, table = "tab"),
    c(expected[1], "hand_rolled")
  )

  seen <- character(0)
  real_add_index <- add_index
  local_mocked_bindings(
    add_index = function(connection, table, index, keys) {
      seen <<- c(seen, index)
      real_add_index(
        connection = connection,
        table = table,
        index = index,
        keys = keys
      )
    }
  )
  suppressMessages(tab$confirm_indexes())

  # One create, and it is the missing index.
  expect_identical(seen, expected[2])

  # The unrelated index survives, and both declared indexes are present.
  after <- get_indexes(connection = con, table = "tab")
  expect_true("hand_rolled" %in% after)
  expect_true(all(expected %in% after))

  tab$disconnect()
})

test_that("confirm_indexes raises when the managed name holds other columns", {
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
  physical <- index_physical_name(table = "tab", index = "ind1")

  # Replace the index with one of the same name over another column. That is
  # the state a changed declaration leaves behind.
  drop_index(connection = con, table = "tab", index = physical)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE INDEX ",
      DBI::dbQuoteIdentifier(con, physical),
      " ON `tab` (`b`)"
    )
  )
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = physical),
    "b"
  )

  expect_error(suppressMessages(tab$confirm_indexes()), "covers b")

  # The index is untouched. confirm_indexes() raises, it does not drop.
  expect_identical(get_indexes(connection = con, table = "tab"), physical)
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = physical),
    "b"
  )

  tab$disconnect()
})

test_that("add_indexes raises when the index does not reach the table", {
  # The catalogue check, isolated. A create that reports success and leaves
  # nothing behind is the failure mode this release removes. The check that
  # catches it needs its own block.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("a", "c"))
  )
  suppressMessages(tab$connect())

  # add_index() does nothing at all, exactly as `CREATE INDEX IF NOT EXISTS`
  # does when another table already holds the name.
  local_mocked_bindings(
    add_index = function(connection, table, index, keys) invisible(NULL)
  )
  drop_index(
    connection = tab$dbconnection$autoconnection,
    table = "tab",
    index = index_physical_name(table = "tab", index = "ind1")
  )

  expect_error(suppressMessages(tab$add_indexes()), "is not on table tab")

  tab$disconnect()
})

# ---------------------------------------------------------------------------
# The physical name itself
# ---------------------------------------------------------------------------

test_that("two identities that the old constraint rule collapses stay apart", {
  # `PK_{table}` at add_constraint() deletes `.`, `[` and `]`, so schema `a`
  # with table `bc` and schema `ab` with table `c` both give `PK_abc`. That
  # rule is the pattern to learn from, not the one to copy.
  collapse_old <- function(x) stringr::str_remove_all(x, "\\.")
  expect_identical(collapse_old("a.bc"), collapse_old("ab.c"))

  expect_false(identical(
    index_physical_name(table = "a.bc", index = "ind1"),
    index_physical_name(table = "ab.c", index = "ind1")
  ))

  # The logical name is part of the identity too, not only the table.
  expect_false(identical(
    index_physical_name(table = "tab", index = "ind1"),
    index_physical_name(table = "tab", index = "ind2")
  ))
})

test_that("two DBI::Id values that join to one string stay apart", {
  # index_table_identity() returns the ordered components and never a joined
  # string. A join loses the boundary between the components, and that
  # boundary is what tells these two tables apart.
  a <- DBI::Id(schema = "a", table = "b.c")
  b <- DBI::Id(schema = "a.b", table = "c")

  # The two really do join to one string. Without this the block would pass
  # on a pair that was never a collision.
  expect_identical(
    paste(as.character(a@name), collapse = "."),
    paste(as.character(b@name), collapse = ".")
  )

  expect_identical(index_table_identity(a), c("a", "b.c"))
  expect_identical(index_table_identity(b), c("a.b", "c"))

  expect_false(identical(
    index_physical_name(table = a, index = "ind1"),
    index_physical_name(table = b, index = "ind1")
  ))

  # A trailing empty component survives. R's strsplit() drops one, so "a.b."
  # would otherwise read as "a.b" and the two would share a name.
  expect_identical(index_table_identity("a.b."), c("a", "b", ""))
  expect_false(identical(
    index_physical_name(table = "a.b.", index = "ind1"),
    index_physical_name(table = "a.b", index = "ind1")
  ))
})

test_that("a DBI::Id and its text form name one index", {
  # This is what makes the four call sites agree. Every site passes
  # table_name_short_for_mssql_fully_specified_for_postgres, and a caller that
  # holds only the text form still lands on the same name.
  expect_identical(
    index_physical_name(
      table = DBI::Id(schema = "anon", table = "tab"),
      index = "ind1"
    ),
    index_physical_name(table = "anon.tab", index = "ind1")
  )
  expect_identical(
    index_physical_name(table = DBI::Id(table = "tab"), index = "ind1"),
    index_physical_name(table = "tab", index = "ind1")
  )
  expect_false(identical(
    index_physical_name(
      table = DBI::Id(schema = "a", table = "bc"),
      index = "ind1"
    ),
    index_physical_name(
      table = DBI::Id(schema = "ab", table = "c"),
      index = "ind1"
    )
  ))

  # Under SQLite the two DBTable_v9 fields agree, so every block in this file
  # that passes the plain table name reaches the production name.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )
  expect_identical(
    index_physical_name(
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
      index = "ind1"
    ),
    index_physical_name(
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres_text,
      index = "ind1"
    )
  )
  tab$disconnect()
})

test_that("a physical index name is lowercase and fits the identifier limit", {
  # PostgreSQL folds an unquoted identifier to lowercase and SQLite does not.
  # A lowercase name reads the same in the source and in both catalogues.
  # Measured on norsyss_data1 on 2026-08-15: 92 lowercase `pk_` constraint
  # names and 0 uppercase, while the source writes `PK_`.
  mixed <- index_physical_name(table = "ANON.MixedCase", index = "IND1")
  expect_identical(mixed, tolower(mixed))
  expect_true(grepl("^[a-z][a-z0-9_]*$", mixed))
  expect_true(nchar(mixed, type = "bytes") <= INDEX_NAME_MAX_CHARS)

  # Case is part of the identity, so folding does not merge two tables.
  expect_false(identical(
    index_physical_name(table = "anon.mixedcase", index = "ind1"),
    mixed
  ))

  # The same inputs always give the same name. Otherwise a drop could never
  # find what a create made.
  expect_identical(
    index_physical_name(table = "anon.tab", index = "ind1"),
    index_physical_name(table = "anon.tab", index = "ind1")
  )
})

test_that("a logical name past the identifier limit is shortened and stays unique", {
  # The two logical names differ in their FIRST character and share the last
  # 71, so the readable part of both physical names is identical. Only the
  # digest tells them apart, which is what the digest is for.
  long_a <- paste0("a", strrep("z", 70), "1")
  long_b <- paste0("b", strrep("z", 70), "1")
  expect_true(nchar(long_a) > INDEX_NAME_MAX_CHARS)

  declared <- list("c", "c")
  names(declared) <- c(long_a, long_b)

  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = declared
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection
  expected <- expected_physical_names(tab)

  # Shortened: neither physical name carries the whole logical name.
  expect_false(grepl(long_a, expected[1], fixed = TRUE))
  expect_false(grepl(long_b, expected[2], fixed = TRUE))

  # Inside the limit, lowercase, and still distinct from the sibling.
  expect_true(all(nchar(expected, type = "bytes") <= INDEX_NAME_MAX_CHARS))
  expect_identical(expected, tolower(expected))
  expect_length(unique(expected), 2L)

  # Both reached the database, and drop_indexes() finds both again.
  expect_identical(get_indexes(connection = con, table = "tab"), expected)
  suppressMessages(tab$drop_indexes())
  expect_identical(
    get_indexes(connection = con, table = "tab"),
    character(0)
  )

  tab$disconnect()
})

test_that("every site that names an index reaches one helper", {
  # A rename that reaches creation but not dropping leaves an index nobody can
  # remove. The default upsert method builds its temporary table LIKE the
  # source table, so that temporary table carries the source table's PHYSICAL
  # index names. It needs the same helper. That method needs a MySQL server,
  # so this block reads the call it makes rather than running it.
  upsert_src <- paste0(
    deparse(S7::method(upsert_load_data_infile, db_default)),
    collapse = "\n"
  )
  expect_true(grepl("index_physical_name", upsert_src, fixed = TRUE))

  helper_src <- paste0(
    deparse(DBTable_v9$private_methods$physical_index_name),
    collapse = "\n"
  )
  expect_true(grepl("index_physical_name", helper_src, fixed = TRUE))

  methods <- c(DBTable_v9$public_methods, DBTable_v9$private_methods)
  for (m in c("drop_indexes", "add_declared_index", "confirm_declared_index")) {
    src <- paste0(deparse(methods[[m]]), collapse = "\n")
    expect_true(grepl("physical_index_name", src, fixed = TRUE))
  }
})

test_that("get_index_columns answers unknown, absent and present apart", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = c("c", "a"))
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection
  physical <- index_physical_name(table = "tab", index = "ind1")

  # Present: the declared columns, in the declared order and not sorted.
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = physical),
    c("c", "a")
  )

  # Absent.
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = "no_such_index"),
    character(0)
  )

  # Present in the database, but on another table. A SQLite index name is
  # unique per database, so the pragma alone would answer for it.
  DBI::dbExecute(con, "CREATE TABLE other (z TEXT)")
  DBI::dbExecute(con, "CREATE INDEX `ind_other` ON `other` (`z`)")
  expect_identical(
    get_index_columns(connection = con, table = "other", index = "ind_other"),
    "z"
  )
  expect_identical(
    get_index_columns(connection = con, table = "tab", index = "ind_other"),
    character(0)
  )

  # Unknown: the default backend cannot read an index definition, so it says
  # so rather than report an absence it did not measure.
  expect_null(S7::method(get_index_columns, db_default)(con, "tab", physical))

  tab$disconnect()
})

test_that("a backend with no catalogue reader creates without verifying", {
  # Column verification is defined for SQLite and for PostgreSQL, and for no
  # other backend. NULL from get_index_columns() says nothing was measured.
  # add_indexes() then creates the index and does NOT check it. It must not
  # raise on an absence it never measured, because that would break every
  # create on SQL Server and on MySQL.
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
  physical <- index_physical_name(table = "tab", index = "ind1")

  drop_index(connection = con, table = "tab", index = physical)
  expect_identical(get_indexes(connection = con, table = "tab"), character(0))

  # A backend that creates nothing and reads nothing back.
  local_mocked_bindings(
    add_index = function(connection, table, index, keys) invisible(NULL),
    get_index_columns = function(connection, table, index) NULL
  )

  expect_no_error(suppressMessages(tab$add_indexes()))

  # Nothing reached the database, and nothing raised. That is the documented
  # limit of this release, not an accident.
  expect_identical(get_indexes(connection = con, table = "tab"), character(0))

  tab$disconnect()
})

test_that("the PostgreSQL drop_index names the schema", {
  # PostgreSQL resolves an unqualified index name through search_path, and
  # csdb creates every index on a fully specified table. An unqualified drop
  # finds the index only when its schema is on the path, and the try() inside
  # the method hides the miss.
  #
  # SQLite reproduces that. ATTACH gives a second schema, one index name can
  # exist in both, and an unqualified DROP INDEX takes the one in main. The
  # db_postgres method runs against this connection, as the two add_index
  # blocks at the top of this file already do.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )
  suppressMessages(tab$connect())
  con <- tab$dbconnection$autoconnection

  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS anon")
  DBI::dbExecute(con, "CREATE TABLE anon.tab (a TEXT, b INTEGER, c REAL)")
  DBI::dbExecute(con, "CREATE INDEX main.dupe ON tab (c)")
  DBI::dbExecute(con, "CREATE INDEX anon.dupe ON tab (c)")

  in_schema <- function(schema) {
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT name FROM ",
        schema,
        ".sqlite_master WHERE type = 'index' AND name = 'dupe'"
      )
    )$name
  }

  # One name, two schemas. Without this the block proves nothing.
  expect_length(in_schema("main"), 1L)
  expect_length(in_schema("anon"), 1L)

  S7::method(drop_index, db_postgres)(con, "anon.tab", "dupe")

  # The index in anon is gone, and the one in main survives untouched.
  expect_length(in_schema("anon"), 0L)
  expect_length(in_schema("main"), 1L)

  # With no schema in the table identity the statement stays unqualified, so
  # a table outside any schema still drops.
  S7::method(drop_index, db_postgres)(con, "tab", "dupe")
  expect_length(in_schema("main"), 0L)

  tab$disconnect()
})

# ---------------------------------------------------------------------------
# One identity, one name, at every call site
# ---------------------------------------------------------------------------

test_that("a table name that holds a dot is created, verified and dropped", {
  # The whole invariant, end to end, on the one backend this suite can run.
  #
  # `add_indexes()` builds the name from the DBI::Id, and the catalogue check
  # after it read the table out of the TEXT field until 2026.8.16. Text splits
  # on every dot, so `an.on.tab` read as the three components `an`, `on` and
  # `tab`. The check looked for the index on a table called `tab` and found
  # nothing. It reported that the index it had just created was on no table.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "an.on.tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )

  expect_no_error(suppressMessages(tab$add_indexes()))
  con <- tab$dbconnection$autoconnection

  physical <- index_physical_name(
    table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
    index = "ind1"
  )
  expect_identical(
    get_indexes(connection = con, table = "an.on.tab"),
    physical
  )
  expect_identical(
    get_index_columns(
      connection = con,
      table = tab$table_name_short_for_mssql_fully_specified_for_postgres,
      index = physical
    ),
    "c"
  )

  # confirm_indexes() finds what add_indexes() made, so it emits no DDL.
  schema_version <- function() {
    DBI::dbGetQuery(con, "PRAGMA schema_version")[[1]]
  }
  before <- schema_version()
  expect_no_error(suppressMessages(tab$confirm_indexes()))
  expect_identical(schema_version(), before)

  # And drop_indexes() removes it.
  suppressMessages(tab$drop_indexes())
  expect_identical(
    get_indexes(connection = con, table = "an.on.tab"),
    character(0)
  )

  tab$disconnect()
})

test_that("every call site computes one name for a table name with a dot", {
  # The property, asserted rather than argued. Every site that names the index
  # names one thing, and every site that reads the table as an IDENTITY
  # recovers the table's own components.
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "an.on.tab",
    field_types = c(a = "TEXT", b = "INTEGER", c = "DOUBLE"),
    keys = c("a", "b"),
    indexes = list(ind1 = "c")
  )

  seen_index <- character(0)
  seen_identity <- list()
  real_add_index <- add_index
  real_drop_index <- drop_index
  real_get_index_columns <- get_index_columns
  local_mocked_bindings(
    # add_index() receives the table as SQL and not as an identity, so only
    # the index name is recorded here.
    add_index = function(connection, table, index, keys) {
      seen_index <<- c(seen_index, index)
      real_add_index(
        connection = connection,
        table = table,
        index = index,
        keys = keys
      )
    },
    drop_index = function(connection, table, index) {
      seen_index <<- c(seen_index, index)
      seen_identity[[length(seen_identity) + 1L]] <<- index_table_identity(
        table
      )
      real_drop_index(connection = connection, table = table, index = index)
    },
    get_index_columns = function(connection, table, index) {
      seen_index <<- c(seen_index, index)
      seen_identity[[length(seen_identity) + 1L]] <<- index_table_identity(
        table
      )
      real_get_index_columns(
        connection = connection,
        table = table,
        index = index
      )
    }
  )

  expect_no_error(suppressMessages(tab$add_indexes()))
  con <- tab$dbconnection$autoconnection
  in_database <- get_indexes(connection = con, table = "an.on.tab")
  expect_no_error(suppressMessages(tab$confirm_indexes()))
  expect_no_error(suppressMessages(tab$drop_indexes()))

  # One name, at every site and in the catalogue.
  expect_length(in_database, 1L)
  expect_gt(length(seen_index), 2L)
  expect_length(unique(c(seen_index, in_database)), 1L)

  # Every identity read gives the table's own components. The text form gives
  # c("an", "on", "tab") instead, which names a different table.
  expect_gt(length(seen_identity), 1L)
  for (k in seq_along(seen_identity)) {
    expect_identical(seen_identity[[k]], "an.on.tab")
  }

  tab$disconnect()
})

test_that("no DBTable_v9 site reads an identity out of the text field", {
  # The regression guard for the defect above. read_index_columns() and
  # drop_indexes() passed
  # table_name_short_for_mssql_fully_specified_for_postgres_text, and the
  # methods behind them read that argument as an identity.
  #
  # add_index() and get_indexes() still take the text field, and must. They
  # use it as SQL and as a bound value, never as an identity.
  methods <- c(DBTable_v9$public_methods, DBTable_v9$private_methods)
  for (m in c("physical_index_name", "read_index_columns", "drop_indexes")) {
    src <- paste0(deparse(methods[[m]]), collapse = "\n")
    expect_false(grepl(
      "table_name_short_for_mssql_fully_specified_for_postgres_text",
      src,
      fixed = TRUE
    ))
    expect_true(grepl(
      "table_name_short_for_mssql_fully_specified_for_postgres",
      src,
      fixed = TRUE
    ))
  }
})

test_that("the text form names the identity of its dot-separated pieces", {
  # Text with a dot cannot say where a component ends. `"anon.tab"` is equally
  # the text form of `Id(schema = "anon", table = "tab")` and of
  # `Id(table = "anon.tab")`. csdb resolves text by splitting on every dot,
  # and the block above pins that `"anon.tab"` means the first of those two.
  # No rule inside the mapper can refuse the ambiguous case, because every
  # dotted text is ambiguous, including the one that must keep working.
  #
  # The cost is that text is not a stand-in for a DBI::Id whose component
  # holds a dot. csdb never uses text as an identity, so no call site reaches
  # this. A direct caller that holds a DBI::Id MUST pass the DBI::Id.
  expect_identical(index_table_identity("an.on.tab"), c("an", "on", "tab"))
  expect_identical(
    index_table_identity(DBI::Id(schema = "an.on", table = "tab")),
    c("an.on", "tab")
  )
  expect_identical(
    index_table_identity(DBI::Id(table = "an.on.tab")),
    "an.on.tab"
  )

  # Three identities, three names.
  expect_length(
    unique(c(
      index_physical_name(DBI::Id(schema = "an.on", table = "tab"), "ind1"),
      index_physical_name(DBI::Id(table = "an.on.tab"), "ind1"),
      index_physical_name("an.on.tab", "ind1")
    )),
    3L
  )
})

test_that("the PostgreSQL drop_index reads the schema from the identity", {
  # The schema is the second-to-last component of the table identity, and only
  # a DBI::Id carries the boundary that identifies it. Given the text
  # `"an.on.tab"` the method reads the schema as `on`, which is a different
  # schema, and the try() around the call hides the miss.
  #
  # The method needs a PostgreSQL server, so this block captures the statement
  # rather than run it. It still needs a live connection, because the method
  # quotes the schema and the index through DBI::dbQuoteIdentifier(). The
  # expected text is built with that same function, so the block asserts the
  # structure and not one backend's quote character.
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)
  con <- suppressMessages(conn$autoconnection)

  quoted <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

  statements <- character(0)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      statements <<- c(statements, as.character(statement))
      0L
    },
    .package = "DBI"
  )

  S7::method(drop_index, db_postgres)(
    con,
    DBI::Id(schema = "an.on", table = "tab"),
    "ix_x"
  )
  S7::method(drop_index, db_postgres)(con, "an.on.tab", "ix_x")

  expect_length(statements, 2L)

  # From the DBI::Id the schema is `an.on`, quoted whole, so the dot inside it
  # cannot read as a separator.
  expect_identical(
    statements[1],
    paste0("DROP INDEX IF EXISTS ", quoted("an.on"), ".", quoted("ix_x"))
  )
  # From the text the schema is `on`, which is a different schema.
  expect_identical(
    statements[2],
    paste0("DROP INDEX IF EXISTS ", quoted("on"), ".", quoted("ix_x"))
  )

  conn$disconnect()
})

test_that("the default drop_index accepts a DBI::Id", {
  # DBTable_v9 hands drop_index() the DBI::Id now. glue::glue() coerces with
  # as.character(), which raises on a DBI::Id, so the two methods that paste
  # the table into SQL convert it first.
  expect_error(
    glue::glue(
      "ALTER TABLE {table}",
      table = DBI::Id(schema = "a", table = "b")
    ),
    "coercing this S4 class"
  )
  expect_identical(
    index_table_text(DBI::Id(schema = "anon", table = "tab")),
    "anon.tab"
  )
  expect_identical(index_table_text("anon.tab"), "anon.tab")
  expect_identical(index_table_text(DBI::Id(table = "an.on.tab")), "an.on.tab")

  statements <- character(0)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      statements <<- c(statements, as.character(statement))
      0L
    },
    .package = "DBI"
  )
  S7::method(drop_index, db_default)(
    NULL,
    DBI::Id(schema = "anon", table = "tab"),
    "ix_x"
  )
  S7::method(drop_index, db_default)(NULL, "anon.tab", "ix_x")

  # One statement from both shapes, so a caller that already passed text sees
  # no change.
  expect_length(statements, 2L)
  expect_identical(statements[1], statements[2])
})

test_that("the PostgreSQL add_index quotes the table, index and columns", {
  # csdb pasted all three into `CREATE INDEX` in raw until 2026.8.16.
  # Measured against PostgreSQL 16.14 with the quoting removed. The table
  # `zz_quote_probe.dot` gave `cross-database references are not implemented`.
  # The table `zz_quote_probeUPPER` gave
  # `relation "anon.zz_quote_probeupper" does not exist`. The table
  # `zz_quote_probe space` gave `syntax error at or near "space"`.
  #
  # The method needs a PostgreSQL server, so this block captures the statement
  # rather than run it. The expected text is built with the same
  # DBI::dbQuoteIdentifier() the method calls, so the block asserts the
  # structure and not one backend's quote character.
  cfg <- sqlite_dbconfig()
  conn <- sqlite_connection(cfg)
  con <- suppressMessages(conn$autoconnection)

  quoted <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))

  statements <- character(0)
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      statements <<- c(statements, as.character(statement))
      0L
    },
    .package = "DBI"
  )

  S7::method(add_index, db_postgres)(
    con,
    DBI::Id(schema = "an.on", table = "My Tab"),
    "ix_x",
    c("Col One", "b.c")
  )

  expect_length(statements, 1L)
  expect_identical(
    statements[1],
    paste0(
      "CREATE INDEX IF NOT EXISTS ",
      quoted("ix_x"),
      " ON ",
      quoted("an.on"),
      ".",
      quoted("My Tab"),
      " (",
      quoted("Col One"),
      ", ",
      quoted("b.c"),
      ")"
    )
  )

  conn$disconnect()
})
