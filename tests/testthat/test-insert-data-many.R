# insert_data_many() loads several tables. On PostgreSQL one transaction on
# one connection loads all of them, so a failure stores no row in any table.
# Before 2026.10.13 a caller called $insert_data() once for each table, and
# each call started its own psql process and its own login.
#
# SQLite tables stand in for the PostgreSQL tables, with the mocked server of
# local_json_server() in helper-json-server.R. loads_through_json() is mocked
# to TRUE, so the tables load as PostgreSQL tables do.

local_session_waits <- function(.local_envir = parent.frame()) {
  rec <- new.env()
  rec$waits <- numeric()
  local_mocked_bindings(
    retry_wait = function(seconds) {
      rec$waits <- c(rec$waits, seconds)
      return(invisible(NULL))
    },
    .env = .local_envir
  )
  return(rec)
}

capture_conditions <- function(expr) {
  messages <- character()
  error <- withCallingHandlers(
    tryCatch(
      {
        force(expr)
        NULL
      },
      error = function(e) e
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  return(list(
    error = error,
    message = if (is.null(error)) NA_character_ else conditionMessage(error),
    messages = messages
  ))
}

# A subclass whose $insert_data() and $upsert_data() take update_last_updated,
# as the cs9 tables do. Each call records the value it received. A call with
# update_last_updated = TRUE writes one last-updated record: here a count in
# `records`, in cs9 a row of config_tables_last_updated. The methods call
# their parent with the arguments that the parent takes.
Recording <- R6::R6Class(
  "Recording",
  inherit = DBTable_v9,
  public = list(
    seen_insert = character(),
    seen_upsert = character(),
    records = 0L,
    insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE,
      load_timeout = 3600,
      update_last_updated = TRUE
    ) {
      self$seen_insert <- c(self$seen_insert, as.character(update_last_updated))
      super$insert_data(
        newdata,
        confirm_insert_via_nrow = confirm_insert_via_nrow,
        verbose = verbose,
        load_timeout = load_timeout
      )
      if (update_last_updated) {
        self$records <- self$records + 1L
      }
      return(invisible(NULL))
    },
    upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE,
      load_timeout = 3600,
      update_last_updated = TRUE
    ) {
      self$seen_upsert <- c(self$seen_upsert, as.character(update_last_updated))
      super$upsert_data(
        newdata,
        drop_indexes = drop_indexes,
        verbose = verbose,
        load_timeout = load_timeout
      )
      if (update_last_updated) {
        self$records <- self$records + 1L
      }
      return(invisible(NULL))
    }
  )
)

# The number of last-updated records that each table wrote.
records_of <- function(tables) {
  return(vapply(tables, function(t) t$records, integer(1)))
}

# Three SQLite tables t1, t2 and t3 on one shared connection. Each table
# exists before the test starts.
three_tables <- function(class = DBTable_v9, .local_envir = parent.frame()) {
  cfg <- sqlite_dbconfig(.local_envir = .local_envir)
  conn <- sqlite_connection(cfg)
  withr::defer(conn$disconnect(), envir = .local_envir)
  tables <- lapply(c("t1", "t2", "t3"), function(name) {
    tab <- class$new(
      dbconfig = cfg,
      table_name = name,
      field_types = c(id = "INTEGER", x = "TEXT"),
      keys = "id",
      dbconnection = conn
    )
    suppressMessages(tab$connect())
    return(tab)
  })
  return(tables)
}

three_data <- function() {
  return(list(
    data.table::data.table(id = 1:2, x = c("a1", "a2")),
    data.table::data.table(id = 1:2, x = c("b1", "b2")),
    data.table::data.table(id = 1:2, x = c("c1", "c2"))
  ))
}

rows_of <- function(tab) {
  d <- DBI::dbGetQuery(
    tab$dbconnection$autoconnection,
    paste0("SELECT id, x FROM ", tab$table_name, " ORDER BY id")
  )
  return(paste(d$id, d$x))
}

# Run insert_data_many() as if the tables were on PostgreSQL. A
# load_data_infile() call for one table, which the per-table path makes,
# also goes to the PostgreSQL method and so to the mocked server.
insert_as_postgres <- function(tables, data, ...) {
  pg <- S7::method(load_data_infile, db_postgres)
  return(with_mocked_bindings(
    insert_data_many(tables, data, ...),
    loads_through_json = function(connection) TRUE,
    load_data_infile = function(...) pg(...)
  ))
}

# The texts that the server sends, as odbc 1.7.0 and psqlODBC 13.02 gave them
# against PostgreSQL 18.6 on 2026-10-15. test-load-json.R holds the full set.
many_text_duplicate <- "ERROR: duplicate key value violates unique constraint \"t2_pkey\""
many_text_lock <- "ERROR: canceling statement due to lock timeout;"
many_text_lost <- paste(
  "server closed the connection unexpectedly",
  "The connection has been lost",
  sep = "\n"
)

# The first load of table t2 fails as the PostgreSQL load fails on a
# duplicate key. Every other load is the real SQLite load. Returns the
# environment whose `tables` lists every table that a load reached.
local_t2_duplicate_once <- function(.local_envir = parent.frame()) {
  real <- load_data_infile
  per_table <- new.env()
  per_table$tables <- character()
  local_mocked_bindings(
    load_data_infile = function(connection, dbconfig = NULL, table, ...) {
      name <- if (methods::is(table, "Id")) table@name[["table"]] else table
      per_table$tables <- c(per_table$tables, name)
      if (identical(name, "t2") && sum(per_table$tables == "t2") == 1L) {
        stop(errorCondition(
          "Loading data into table t2 failed (mocked).",
          class = c("csdb_load_duplicate_key", "csdb_load_error"),
          table = "t2",
          tool = "json_populate_recordset",
          attempts = 1L,
          failure_class = "duplicate key"
        ))
      }
      return(real(
        connection = connection,
        dbconfig = dbconfig,
        table = table,
        ...
      ))
    },
    .env = .local_envir
  )
  return(per_table)
}

test_that("insert_data_many() loads 3 tables in one transaction, one statement each, in order", {
  rec <- local_json_server()
  proc <- local_no_external_process()
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  inserts <- grep("^INSERT INTO ", rec$sql, value = TRUE)
  expect_identical(
    sub("^INSERT INTO `(t[123])` .*$", "\\1", inserts),
    c("t1", "t2", "t3")
  )
  expect_identical(rec$sql[[1]], "SET LOCAL lock_timeout = '60s'")
  expect_identical(rec$client, 0L)
  expect_identical(proc$n, 0L)
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
  expect_length(waits$waits, 0)
})

test_that("a short row count of table 2 stops the load inside the transaction", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") 1 else NULL
  })
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_other")
  expect_match(
    got$message,
    "table `t2` stored 1 rows of one statement, and csdb sent 2 rows.",
    fixed = TRUE
  )
  for (tab in tables) {
    expect_identical(rows_of(tab), character())
  }
  expect_length(waits$waits, 0)
})

test_that("insert_data_many() runs the whole load again after a lock timeout", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2" && n == 1L) many_text_lock else NULL
  })
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_null(got$error)
  expect_identical(waits$waits, 2)
  expect_identical(sum(rec$sql == "SET LOCAL lock_timeout = '60s'"), 2L)
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_match(
    got$messages,
    "failed on attempt 1 of at most 3 (transient). Retrying in 2 s.",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data_many() skips a table whose data has no rows", {
  rec <- local_json_server()
  tables <- three_tables()
  data <- three_data()
  data[[2]] <- data[[2]][0]
  got <- capture_conditions(insert_as_postgres(tables, data))
  expect_null(got$error)
  inserts <- grep("^INSERT INTO ", rec$sql, value = TRUE)
  expect_identical(
    sub("^INSERT INTO `(t[123])` .*$", "\\1", inserts),
    c("t1", "t3")
  )
})

test_that("insert_data_many() stops on a duplicate key without confirm_insert_via_nrow", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") many_text_duplicate else NULL
  })
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_duplicate_key")
  expect_identical(rows_of(tables[[1]]), character())
  expect_length(waits$waits, 0)
})

test_that("insert_data_many(confirm_insert_via_nrow = TRUE) inserts table by table after a duplicate key", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2" && n == 1L) many_text_duplicate else NULL
  })
  tables <- three_tables()
  suppressMessages(
    tables[[2]]$insert_data(data.table::data.table(id = 1L, x = "old1"))
  )
  data <- three_data()
  data[[2]] <- data.table::data.table(id = c(1L, 3L), x = c("new1", "new3"))

  # The load of all three tables goes to the mocked server. The per-table
  # loads after it are the real SQLite loads, except the first load of t2.
  per_table <- local_t2_duplicate_once()
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, data, confirm_insert_via_nrow = TRUE),
    loads_through_json = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_identical(rec$counts$t2, 1L)
  # t1, then t2, then the staging table of the upsert of t2, then t3.
  expect_length(per_table$tables, 4)
  expect_identical(per_table$tables[c(1, 2, 4)], c("t1", "t2", "t3"))
  expect_match(per_table$tables[3], "^tmp")
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 new1", "3 new3"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
  expect_match(
    got$messages,
    "json_populate_recordset failed on attempt 1 (duplicate key), so the server stored no row in any of the 3 tables",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(
    got$messages,
    "on table t2: json_populate_recordset failed on attempt 1 (duplicate key)",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("insert_data_many(confirm_insert_via_nrow = TRUE) upserts each table that holds fewer rows after the load", {
  # This server reports every row as stored and stores none, so each table
  # holds 0 rows after a load that succeeded. Each upsert is a real SQLite
  # upsert.
  local_mocked_bindings(
    json_load_execute = function(connection, sql, json = NULL) {
      if (is.null(json)) {
        return(0)
      }
      return(nrow(yyjsonr::read_json_str(json)))
    }
  )
  tables <- three_tables()
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, three_data(), confirm_insert_via_nrow = TRUE),
    loads_through_json = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
  expect_identical(
    sum(grepl(
      "After insert have 0 rows. Tried to insert 2. Now trying upsert.",
      got$messages,
      fixed = TRUE
    )),
    3L
  )
})

test_that("csdb writes no last-updated record after a load that commits", {
  rec <- local_json_server()
  tables <- three_tables(Recording)
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(tables, three_data()),
    loads_through_json = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(grep("^INSERT INTO ", rec$sql), 3)
  expect_identical(records_of(tables), c(0L, 0L, 0L))
  for (tab in tables) {
    expect_identical(tab$seen_insert, character(0))
    expect_identical(tab$seen_upsert, character(0))
  }
})

test_that("a load that loses its connection is not retried", {
  rec <- local_json_server(fail = function(table, n) {
    if (table == "t2") many_text_lost else NULL
  })
  waits <- local_session_waits()
  tables <- three_tables()
  got <- capture_conditions(insert_as_postgres(tables, three_data()))
  expect_s3_class(got$error, "csdb_load_ambiguous")
  expect_length(waits$waits, 0)
  expect_match(
    got$message,
    "The failure class is ambiguous, after 1 attempt.",
    fixed = TRUE
  )
  expect_match(
    got$message,
    "server closed the connection unexpectedly",
    fixed = TRUE
  )
})

# The phrases that the installed help and NEWS.md MUST hold. The help is read
# from man/insert_data_many.Rd in the source tree, or from the installed
# package when the tests run from a built package.
rendered_help <- function() {
  src <- test_path("..", "..", "man", "insert_data_many.Rd")
  rd <- if (file.exists(src)) {
    tools::parse_Rd(src)
  } else {
    tools::Rd_db("csdb")[["insert_data_many.Rd"]]
  }
  withr::local_options(useFancyQuotes = FALSE)
  out <- utils::capture.output(tools::Rd2txt(
    rd,
    options = list(underline_titles = FALSE)
  ))
  return(gsub("\\s+", " ", paste(out, collapse = " ")))
}

news_text <- function() {
  src <- test_path("..", "..", "NEWS.md")
  f <- if (file.exists(src)) src else system.file("NEWS.md", package = "csdb")
  return(gsub("\\s+", " ", paste(readLines(f), collapse = " ")))
}

test_that("the help of insert_data_many() states the rules of the load", {
  help <- rendered_help()
  phrases <- c(
    lock_timeout = "The transaction waits at most 60 s for one lock.",
    empty_skip = "A table whose data.table is 'NULL' or has no rows gets no statement.",
    fallback = "With another driver, or with tables that have different connection settings, csdb calls '$insert_data()' on each table in turn.",
    row_count = "A difference raises an error before the commit, so the server stores no row in any table.",
    exception = "The one exception is a duplicate key with 'confirm_insert_via_nrow = TRUE': csdb then inserts table by table."
  )
  for (p in names(phrases)) {
    expect_true(
      grepl(phrases[[p]], help, fixed = TRUE),
      label = paste0("the help holds the ", p, " phrase \"", phrases[[p]], "\"")
    )
  }
  expect_false(grepl("psql", help, fixed = TRUE))
})

test_that("NEWS.md states the rules of the session", {
  news <- news_text()
  phrases <- c(
    lock_timeout = "It waits at most 60 s for one lock",
    empty_skip = "The session skips a table with no rows",
    fallback = "With another driver, or with tables that have different connection settings, it calls `$insert_data()` on each table in turn.",
    row_count = "checks the row count of each table before the commit",
    exception = "The one exception is a duplicate key with `confirm_insert_via_nrow = TRUE`: csdb then inserts table by table."
  )
  for (p in names(phrases)) {
    expect_true(
      grepl(phrases[[p]], news, fixed = TRUE),
      label = paste0("NEWS.md holds the ", p, " phrase \"", phrases[[p]], "\"")
    )
  }
})

test_that("insert_data_many() inserts table by table on a driver without the JSON load", {
  rec <- local_json_server()
  tables <- three_tables()
  got <- capture_conditions(insert_data_many(tables, three_data()))
  expect_null(got$error)
  expect_length(rec$sql, 0)
  expect_identical(rows_of(tables[[1]]), c("1 a1", "2 a2"))
  expect_identical(rows_of(tables[[2]]), c("1 b1", "2 b2"))
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
})

test_that("insert_data_many() inserts table by table when the tables differ in configuration", {
  rec <- local_json_server()
  tables <- three_tables()
  other <- DBTable_v9$new(
    dbconfig = sqlite_dbconfig(),
    table_name = "t4",
    field_types = c(id = "INTEGER", x = "TEXT"),
    keys = "id"
  )
  withr::defer(other$disconnect())
  data <- c(three_data(), list(data.table::data.table(id = 1L, x = "d1")))
  got <- capture_conditions(with_mocked_bindings(
    insert_data_many(c(tables, list(other)), data),
    loads_through_json = function(connection) TRUE
  ))
  expect_null(got$error)
  expect_length(rec$sql, 0)
  expect_identical(rows_of(other), "1 d1")
  expect_identical(rows_of(tables[[3]]), c("1 c1", "2 c2"))
})

test_that("insert_data_many() rejects tables and newdata that do not pair up", {
  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "t1",
    field_types = c(id = "INTEGER", x = "TEXT"),
    keys = "id"
  )
  withr::defer(tab$disconnect())
  d <- three_data()[[1]]
  expect_error(insert_data_many(tab, list(d)), "tables MUST be a list")
  expect_error(insert_data_many(list(tab), d), "newdata MUST be a list")
  expect_error(
    insert_data_many(list(tab), list(d, d)),
    "one data.table for each table"
  )
  expect_null(insert_data_many(list(), list()))
})

test_that("csdb exports insert_data_many()", {
  ns <- readLines(system.file("NAMESPACE", package = "csdb"))
  expect_true("export(insert_data_many)" %in% ns)
})

test_that("loads_through_json() is TRUE for the PostgreSQL class and FALSE for SQLite", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  expect_false(loads_through_json(con))
  pg <- if (methods::is(db_postgres, "classRepresentation")) {
    methods::new(db_postgres@className)
  } else {
    structure(list(), class = "PostgreSQL")
  }
  expect_true(loads_through_json(pg))
})
