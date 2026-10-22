# PostgreSQL integration tests. They run when CSDB_TEST_PG_SERVER names a
# server, and skip otherwise. CI has no PostgreSQL server, so CI skips them.
#
# Set CSDB_TEST_PG_SERVER, CSDB_TEST_PG_DB, CSDB_TEST_PG_USER and
# CSDB_TEST_PG_PASSWORD. CSDB_TEST_PG_PORT (5432), CSDB_TEST_PG_SCHEMA
# ("public") and CSDB_TEST_PG_DRIVER ("PostgreSQL Unicode") are optional. The
# user MUST be able to create tables in the schema. Each test makes tables
# with new names, and drops them when it ends.

pg_it_config <- function() {
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
    password = Sys.getenv("CSDB_TEST_PG_PASSWORD")
  ))
}

# A DBTable_v9 on a table with a new name. The table is dropped when the
# calling test ends.
pg_it_table <- function(field_types, envir = parent.frame()) {
  name <- paste0("csdb_it_", gsub("-", "", uuid::UUIDgenerate()))
  tab <- DBTable_v9$new(
    dbconfig = pg_it_config(),
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

pg_it_x <- function() {
  x <- c(0.1 + 0.2, 1 / 3, pi, 1e-300, 5e-324, .Machine$double.xmax, -2.5e-8)
  return(rep_len(c(x, NA), length(pg_it_text)))
}

# x::text is the shortest text that PostgreSQL reads back as the same double,
# so the ODBC driver adds no rounding.
pg_it_rows <- function(tab) {
  return(pg_it_query(
    tab,
    "SELECT id, txt, x::text AS x FROM {table} ORDER BY id"
  ))
}

test_that("PostgreSQL insert: every edge text value and every double round-trips, and no external process starts", {
  tab <- pg_it_table(c(id = "INTEGER", txt = "TEXT", x = "DOUBLE"))
  suppressMessages(tab$connect())
  proc <- local_no_external_process()
  dt <- data.table::data.table(
    id = seq_along(pg_it_text),
    txt = pg_it_text,
    x = pg_it_x()
  )
  suppressMessages(tab$insert_data(data.table::copy(dt), verbose = FALSE))
  got <- pg_it_rows(tab)
  expect_identical(got$id, dt$id)
  expect_identical(got$txt, pg_it_text)
  expect_identical(as.numeric(got$x), pg_it_x())
  expect_identical(proc$n, 0L)
})

test_that("PostgreSQL upsert and insert_data_many(): every value round-trips, and no external process starts", {
  tab <- pg_it_table(c(id = "INTEGER", txt = "TEXT", x = "DOUBLE"))
  tab2 <- pg_it_table(c(id = "INTEGER", txt = "TEXT", x = "DOUBLE"))
  suppressMessages(tab$connect())
  suppressMessages(tab2$connect())
  proc <- local_no_external_process()
  dt <- data.table::data.table(
    id = seq_along(pg_it_text),
    txt = pg_it_text,
    x = pg_it_x()
  )
  suppressMessages(tab$insert_data(dt[1:3], verbose = FALSE))
  suppressMessages(tab$upsert_data(data.table::copy(dt), verbose = FALSE))
  suppressMessages(insert_data_many(
    list(tab2),
    list(data.table::copy(dt)),
    verbose = FALSE
  ))
  for (t in list(tab, tab2)) {
    got <- pg_it_rows(t)
    expect_identical(got$id, dt$id)
    expect_identical(got$txt, pg_it_text)
    expect_identical(as.numeric(got$x), pg_it_x())
  }
  expect_identical(proc$n, 0L)
})

test_that("PostgreSQL upsert: the rows go through a TEMPORARY table, and the upsert updates and inserts", {
  tab <- pg_it_table(c(id = "INTEGER", txt = "TEXT"))
  suppressMessages(tab$connect())
  suppressMessages(tab$insert_data(
    data.table::data.table(id = 1:2, txt = c("a", "b")),
    verbose = FALSE
  ))
  # The persistence of the table that the upsert loads: "t" is temporary,
  # "p" is a plain table. The probe also keeps the connection and the name.
  seen <- character()
  staging <- NULL
  real_load <- load_data_infile
  local_mocked_bindings(load_data_infile = function(connection, table, ...) {
    staging <<- list(
      connection = connection,
      text = DBI::dbQuoteString(
        connection,
        as.character(DBI::dbQuoteIdentifier(connection, table))
      )
    )
    seen <<- c(
      seen,
      DBI::dbGetQuery(
        connection,
        paste0(
          "SELECT relpersistence FROM pg_class WHERE oid = CAST(",
          staging$text,
          " AS regclass)"
        )
      )$relpersistence
    )
    return(real_load(connection = connection, table = table, ...))
  })
  suppressMessages(tab$upsert_data(
    data.table::data.table(id = 2:3, txt = c("B", "c")),
    verbose = FALSE
  ))
  expect_identical(seen, "t")
  got <- pg_it_query(tab, "SELECT id, txt FROM {table} ORDER BY id")
  expect_identical(got$id, 1:3)
  expect_identical(got$txt, c("a", "B", "c"))
  # The upsert dropped its staging table.
  gone <- DBI::dbGetQuery(
    staging$connection,
    paste0(
      "SELECT CASE WHEN to_regclass(",
      staging$text,
      ") IS NULL THEN 1 ELSE 0 END AS gone"
    )
  )$gone
  expect_identical(as.integer(gone), 1L)
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

test_that("PostgreSQL: a real column declared DOUBLE becomes double precision, with every row kept, and the table takes rows", {
  tab <- pg_it_table(c(id = "INTEGER", x = "DOUBLE", y = "DOUBLE"))
  pg_it_real_table(tab)
  before <- pg_it_query(tab, pg_it_read)
  oid <- pg_it_oid(tab)
  msgs <- testthat::capture_messages(tab$connect())
  expect_identical(
    pg_it_types(tab)$type,
    c("integer", "double precision", "double precision")
  )
  expect_identical(pg_it_oid(tab), oid)
  after <- pg_it_query(tab, pg_it_read)
  expect_identical(nrow(after), 1000L)
  expect_identical(after, before)
  expect_false(any(grepl("Dropping table", msgs, fixed = TRUE)))
  suppressMessages(tab$insert_data(
    data.table::data.table(id = 1001L, x = 0.1 + 0.2, y = 2),
    verbose = FALSE
  ))
  expect_identical(
    pg_it_query(tab, "SELECT x::text AS x FROM {table} WHERE id = 1001")$x,
    "0.30000000000000004"
  )
  expect_identical(
    pg_it_query(tab, "SELECT count(*)::int AS n FROM {table}")$n,
    1001L
  )
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

test_that("PostgreSQL insert: an identity column and a serial column that the data lacks get their values", {
  tab <- pg_it_table(c(
    a = "INTEGER",
    id = "BIGINT GENERATED BY DEFAULT AS IDENTITY",
    s = "SERIAL"
  ))
  suppressMessages(tab$connect())
  dt <- data.table::data.table(a = 11:15)
  suppressMessages(tab$insert_data(dt, verbose = FALSE))
  got <- pg_it_query(
    tab,
    "SELECT a, id::int AS id, s::int AS s FROM {table} ORDER BY a"
  )
  expect_identical(got$a, 11:15)
  expect_identical(got$id, 1:5)
  expect_identical(got$s, 1:5)
})

test_that("PostgreSQL insert: a column that the data lacks or names in another case, with DEFAULT 'x', stops the load and stores no row", {
  tab <- pg_it_table(c(a = "INTEGER", b = "TEXT DEFAULT 'x'"))
  suppressMessages(tab$connect())
  for (dt in list(
    data.table::data.table(a = 1:2, B = c("u", "v")),
    data.table::data.table(a = 1:2)
  )) {
    expect_error(
      suppressMessages(tab$insert_data(dt, verbose = FALSE)),
      "lacks these columns of the table: b.",
      fixed = TRUE,
      label = paste(names(dt), collapse = ", ")
    )
  }
  expect_identical(
    pg_it_query(tab, "SELECT count(*)::int AS n FROM {table}")$n,
    0L
  )
})

# Runs fn(args) in a new Rscript process. Returns the exit status of the
# process, and the value of fn when the status is 0. A segfault in the child
# gives a status other than 0. Base R starts the child, not callr. With callr,
# the whole suite ended with "Error while shutting down parallel: unable to
# terminate some child processes", measured on 2026-10-22.
pg_it_in_child <- function(fn, args) {
  script <- withr::local_tempfile(fileext = ".R")
  result <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      paste0(".libPaths(", paste(deparse(.libPaths()), collapse = ""), ")"),
      paste0("fn <- ", paste(deparse(fn), collapse = "\n")),
      paste0("args <- ", paste(deparse(args), collapse = "\n")),
      paste0("dput(do.call(fn, args), file = ", deparse(result), ")")
    ),
    script
  )
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    shQuote(script),
    stdout = FALSE,
    stderr = FALSE
  )
  value <- if (identical(status, 0L)) dget(result) else NULL
  return(list(status = status, value = value))
}

# The child process of the next test. A connection that lost its server
# crashes R at its second statement. A regression therefore ends the child
# with a segfault, and the test fails. The child loads the csdb under test: the
# source tree after load_all(), or else the installed package.
pg_it_dead_child <- function(path, cfg) {
  if (nzchar(path)) {
    pkgload::load_all(path, quiet = TRUE)
  } else {
    loadNamespace("csdb")
  }
  ns <- asNamespace("csdb")
  open <- function() {
    return(ns$DBConnection_v9$new(
      driver = cfg$driver,
      server = cfg$server,
      port = cfg$port,
      db = cfg$db,
      user = cfg$user,
      password = cfg$password
    ))
  }
  admin_conn <- open()
  admin <- admin_conn$autoconnection
  # The server ends the session, as a failover ends every session. The
  # timeout makes pg_terminate_backend() wait until the session is gone.
  kill <- function(con) {
    pid <- DBI::dbGetQuery(con, "SELECT pg_backend_pid() AS pid")$pid
    DBI::dbGetQuery(
      admin,
      paste0("SELECT pg_terminate_backend(", pid, ", 10000) AS killed")
    )
  }
  outcome <- function(expr) {
    return(tryCatch(
      {
        force(expr)
        "returned"
      },
      error = function(e) "R error"
    ))
  }
  out <- list()
  # The state of json_load_attempt() when a statement of the load fails.
  rollback_conn <- open()
  con <- rollback_conn$autoconnection
  DBI::dbBegin(con)
  kill(con)
  out$statement <- outcome(DBI::dbExecute(con, "SELECT 1"))
  out$rolled_back <- ns$json_load_rollback(con)
  # The upsert creates its table first, which fails, and its on.exit drops
  # the table.
  upsert_conn <- open()
  con <- upsert_conn$autoconnection
  kill(con)
  out$upsert <- outcome(ns$upsert_load_data_infile(
    connection = con,
    dbconfig = cfg,
    table = DBI::Id(schema = cfg$schema, table = "csdb_it_dead"),
    dt = data.table::data.table(k = 1L, v = 2),
    fields = c("k", "v"),
    keys = "k"
  ))
  # The server ends the session during the load, after the upsert created its
  # staging table. A temporary table goes with the session. A plain table
  # would stay in the schema.
  DBI::dbExecute(
    admin,
    paste0(
      "CREATE TABLE IF NOT EXISTS ",
      cfg$schema,
      ".csdb_it_dead (k integer PRIMARY KEY, v double precision)"
    )
  )
  count_left <- function() {
    return(
      DBI::dbGetQuery(
        admin,
        paste0(
          "SELECT count(*)::int AS n FROM pg_class c JOIN pg_namespace n ",
          "ON n.oid = c.relnamespace WHERE n.nspname = ",
          DBI::dbQuoteString(admin, cfg$schema),
          " AND c.relname LIKE 'tmp%'"
        )
      )$n
    )
  }
  left_before <- count_left()
  late_conn <- open()
  late <- late_conn$autoconnection
  real_load <- ns$load_data_infile
  utils::assignInNamespace(
    "load_data_infile",
    function(...) {
      kill(late)
      return(real_load(...))
    },
    "csdb"
  )
  out$late_upsert <- outcome(ns$upsert_load_data_infile(
    connection = late,
    dbconfig = cfg,
    table = DBI::Id(schema = cfg$schema, table = "csdb_it_dead"),
    dt = data.table::data.table(k = 1L, v = 2),
    fields = c("k", "v"),
    keys = "k"
  ))
  out$left_in_schema <- count_left() - left_before
  DBI::dbExecute(admin, paste0("DROP TABLE ", cfg$schema, ".csdb_it_dead"))
  return(out)
}

test_that("PostgreSQL: a ROLLBACK and an upsert on a connection that the server ended give an R error, not a segfault", {
  cfg <- pg_it_config()
  dev <- exists(".__DEVTOOLS__", envir = asNamespace("csdb"), inherits = FALSE)
  if (dev) {
    skip_if_not_installed("pkgload")
  }
  path <- if (dev) getNamespaceInfo("csdb", "path") else ""
  child <- pg_it_in_child(pg_it_dead_child, list(path = path, cfg = cfg))
  expect_identical(child$status, 0L)
  out <- child$value
  expect_identical(out$statement, "R error")
  expect_false(out$rolled_back)
  expect_identical(out$upsert, "R error")
  expect_identical(out$late_upsert, "R error")
  expect_identical(out$left_in_schema, 0L)
})
