# PostgreSQL integration tests. They run when CSDB_TEST_PG_SERVER names a
# server, and skip otherwise. CI has no PostgreSQL server, so CI skips them.
#
# Set CSDB_TEST_PG_SERVER, CSDB_TEST_PG_DB, CSDB_TEST_PG_USER and
# CSDB_TEST_PG_PASSWORD. CSDB_TEST_PG_PORT (5432), CSDB_TEST_PG_SCHEMA
# ("public") and CSDB_TEST_PG_DRIVER ("PostgreSQL Unicode") are optional. The
# user MUST be able to create tables in the schema. Each test makes tables
# with new names, and drops them when it ends. The psql test also needs psql
# on the PATH.

pg_it_config <- function(pg_widen_doubles = NULL) {
  server <- Sys.getenv("CSDB_TEST_PG_SERVER")
  skip_if(
    server == "",
    paste(
      "no PostgreSQL test database. Set CSDB_TEST_PG_SERVER, CSDB_TEST_PG_DB,",
      "CSDB_TEST_PG_USER and CSDB_TEST_PG_PASSWORD to run these tests."
    )
  )
  return(list(
    driver = Sys.getenv("CSDB_TEST_PG_DRIVER", "PostgreSQL Unicode"),
    server = server,
    port = as.integer(Sys.getenv("CSDB_TEST_PG_PORT", "5432")),
    db = Sys.getenv("CSDB_TEST_PG_DB"),
    schema = Sys.getenv("CSDB_TEST_PG_SCHEMA", "public"),
    user = Sys.getenv("CSDB_TEST_PG_USER"),
    password = Sys.getenv("CSDB_TEST_PG_PASSWORD"),
    pg_load = "psql",
    pg_widen_doubles = pg_widen_doubles
  ))
}

# A DBTable_v9 on a table with a new name. The table is dropped when the
# calling test ends.
pg_it_table <- function(
  field_types,
  pg_widen_doubles = NULL,
  envir = parent.frame()
) {
  name <- paste0("csdb_it_", gsub("-", "", uuid::UUIDgenerate()))
  tab <- DBTable_v9$new(
    dbconfig = pg_it_config(pg_widen_doubles),
    table_name = name,
    field_types = field_types,
    keys = names(field_types)[[1]],
    validator_field_types = validator_field_types_blank,
    validator_field_contents = validator_field_contents_blank
  )
  withr::defer(
    {
      try(
        DBI::dbExecute(
          tab$dbconnection$autoconnection,
          paste0(
            "DROP TABLE IF EXISTS ",
            pg_it_quoted(tab)
          )
        ),
        silent = TRUE
      )
      tab$disconnect()
    },
    envir = envir
  )
  return(tab)
}

pg_it_quoted <- function(tab) {
  con <- tab$dbconnection$autoconnection
  return(paste0(
    DBI::dbQuoteIdentifier(con, tab$dbconfig$schema),
    ".",
    DBI::dbQuoteIdentifier(con, tab$table_name)
  ))
}

pg_it_query <- function(tab, sql) {
  return(DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    gsub("{table}", pg_it_quoted(tab), sql, fixed = TRUE)
  ))
}

pg_it_exec <- function(tab, sql) {
  return(DBI::dbExecute(
    tab$dbconnection$autoconnection,
    gsub("{table}", pg_it_quoted(tab), sql, fixed = TRUE)
  ))
}

# The type of each column, and the oid of the table, which a DROP TABLE and
# a CREATE TABLE change.
pg_it_types <- function(tab) {
  return(pg_it_query(
    tab,
    paste0(
      "SELECT a.attname AS name, format_type(a.atttypid, a.atttypmod) AS type ",
      "FROM pg_attribute a WHERE a.attrelid = '{table}'::regclass ",
      "AND a.attnum > 0 AND NOT a.attisdropped ORDER BY a.attnum"
    )
  ))
}

pg_it_oid <- function(tab) {
  return(pg_it_query(tab, "SELECT '{table}'::regclass::oid::text AS oid")$oid)
}

pg_it_text <- c(
  "",
  NA,
  "\"",
  "a\"b",
  "\"\"",
  "\\",
  "a\\b",
  "\t",
  "a\tb",
  "\n",
  "a\nb",
  "\r",
  "a\r\nb",
  "\\.",
  "a\n\\.\nb",
  "\\N",
  "NA",
  "NULL",
  "'",
  " lead",
  "trail ",
  "\u00e6\u00f8\u00e5",
  "\u65e5\u672c\u8a9e"
)

test_that("PostgreSQL psql load: every edge text value and every double round-trips", {
  skip_if(Sys.which("psql") == "", "psql is not on the PATH")
  tab <- pg_it_table(c(id = "INTEGER", txt = "TEXT", x = "DOUBLE"))
  x <- c(0.1 + 0.2, 1 / 3, pi, 1e-300, 5e-324, .Machine$double.xmax, -2.5e-8)
  x <- rep_len(c(x, NA), length(pg_it_text))
  dt <- data.table::data.table(
    id = seq_along(pg_it_text),
    txt = pg_it_text,
    x = x
  )
  suppressMessages(tab$insert_data(data.table::copy(dt), verbose = FALSE))
  # x::text is the shortest text that PostgreSQL reads back as the same
  # double, so the ODBC driver adds no rounding.
  got <- pg_it_query(
    tab,
    "SELECT id, txt, x::text AS x FROM {table} ORDER BY id"
  )
  expect_identical(got$id, dt$id)
  expect_identical(got$txt, pg_it_text)
  expect_identical(as.numeric(got$x), x)
})

test_that("PostgreSQL: a new table gets double precision for DOUBLE", {
  tab <- pg_it_table(c(id = "INTEGER", x = "DOUBLE"))
  suppressMessages(tab$connect())
  expect_identical(pg_it_types(tab)$type, c("integer", "double precision"))
})

# A table with real columns x and y and 1000 rows, made outside csdb, as
# csdb before 2026.10.16 made it.
pg_it_real_table <- function(tab) {
  tab$dbconnection$connect()
  pg_it_exec(
    tab,
    "CREATE TABLE {table} (id INTEGER NOT NULL PRIMARY KEY, x REAL, y REAL)"
  )
  pg_it_exec(
    tab,
    paste0(
      "INSERT INTO {table} SELECT i, (i / 7.0)::real, ",
      "CASE WHEN i % 10 = 0 THEN NULL ELSE (1e30 / i)::real END ",
      "FROM generate_series(1, 1000) AS i"
    )
  )
  return(invisible(tab))
}

pg_it_read <- paste0(
  "SELECT id, x::float8::text AS x, y::float8::text AS y ",
  "FROM {table} WHERE id <= 1000 ORDER BY id"
)

test_that("PostgreSQL: by default a real column declared DOUBLE stays real, with every row kept, and the table takes rows", {
  tab <- pg_it_table(c(id = "INTEGER", x = "DOUBLE", y = "DOUBLE"))
  pg_it_real_table(tab)
  before <- pg_it_query(tab, pg_it_read)
  oid <- pg_it_oid(tab)
  msgs <- testthat::capture_messages(tab$connect())
  expect_identical(pg_it_types(tab)$type, c("integer", "real", "real"))
  expect_identical(pg_it_oid(tab), oid)
  expect_identical(pg_it_query(tab, pg_it_read), before)
  expect_identical(sum(grepl("are declared DOUBLE but are real", msgs)), 1L)
  suppressMessages(tab$insert_data(
    data.table::data.table(id = 1001L, x = 0.5, y = 2),
    verbose = FALSE
  ))
  expect_identical(
    pg_it_query(tab, "SELECT count(*)::int AS n FROM {table}")$n,
    1001L
  )
})

test_that("PostgreSQL: with pg_widen_doubles = TRUE a real column declared DOUBLE becomes double precision, with every row kept", {
  tab <- pg_it_table(
    c(id = "INTEGER", x = "DOUBLE", y = "DOUBLE"),
    pg_widen_doubles = TRUE
  )
  pg_it_real_table(tab)
  before <- pg_it_query(tab, pg_it_read)
  oid <- pg_it_oid(tab)
  suppressMessages(tab$connect())
  expect_identical(
    pg_it_types(tab)$type,
    c("integer", "double precision", "double precision")
  )
  expect_identical(pg_it_oid(tab), oid)
  after <- pg_it_query(tab, pg_it_read)
  expect_identical(nrow(after), 1000L)
  expect_identical(after, before)
})

test_that("PostgreSQL: a table whose names differ is still dropped and created", {
  tab <- pg_it_table(c(id = "INTEGER", x = "DOUBLE", z = "TEXT"))
  tab$dbconnection$connect()
  pg_it_exec(
    tab,
    "CREATE TABLE {table} (id INTEGER NOT NULL PRIMARY KEY, x REAL)"
  )
  pg_it_exec(tab, "INSERT INTO {table} VALUES (1, 0.5)")
  oid <- pg_it_oid(tab)
  msgs <- testthat::capture_messages(tab$connect())
  expect_true(any(grepl("because fields dont match", msgs, fixed = TRUE)))
  expect_false(identical(pg_it_oid(tab), oid))
  expect_identical(pg_it_types(tab)$name, c("id", "x", "z"))
  expect_identical(
    pg_it_query(tab, "SELECT count(*)::int AS n FROM {table}")$n,
    0L
  )
})
