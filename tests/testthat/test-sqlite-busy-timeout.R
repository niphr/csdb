# SQLite allows one writer at a time. A csdb connection waits for the write
# lock that another connection holds, and does not fail at once with
# "database is locked". cs9 runs parallel plans in forked processes, and two
# of them can write to the same SQLite file at the same moment.

test_that("a fresh SQLite connection has a busy timeout of 60000 ms", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()

  timeout <- DBI::dbGetQuery(db$connection, "PRAGMA busy_timeout")[[1]]
  expect_identical(as.integer(timeout), 60000L)

  db$disconnect()
})

test_that("a forked writer waits for a lock that another process holds", {
  # Windows has no fork.
  skip_on_os("windows")

  cfg <- sqlite_dbconfig()
  tab <- DBTable_v9$new(
    dbconfig = cfg,
    table_name = "tab",
    field_types = c(a = "TEXT", b = "INTEGER"),
    keys = "a"
  )
  suppressMessages(tab$connect())
  tab$disconnect()

  # Process A writes this file when it holds the write lock.
  ready <- withr::local_tempfile(fileext = ".ready")

  # Process A opens its own csdb connection, takes the write lock with BEGIN
  # IMMEDIATE, inserts one row, and holds the lock for 2 s before COMMIT.
  job_a <- parallel::mcparallel({
    db <- sqlite_connection(cfg)
    db$connect()
    con <- db$connection
    DBI::dbExecute(con, "BEGIN IMMEDIATE")
    DBI::dbExecute(con, "INSERT INTO tab (a, b) VALUES ('from_a', 1)")
    writeLines("locked", ready)
    Sys.sleep(2)
    DBI::dbExecute(con, "COMMIT")
    db$disconnect()
    "a committed"
  })

  waited <- 0
  while (!file.exists(ready) && waited < 60) {
    Sys.sleep(0.05)
    waited <- waited + 0.05
  }
  expect_true(file.exists(ready))

  # Process B inserts through the normal insert path while A holds the lock.
  job_b <- parallel::mcparallel({
    start <- Sys.time()
    result <- try(
      suppressMessages(tab$insert_data(data.table::data.table(
        a = "from_b",
        b = 2L
      ))),
      silent = TRUE
    )
    list(
      error = if (inherits(result, "try-error")) {
        as.character(result)
      } else {
        NULL
      },
      seconds = as.numeric(difftime(Sys.time(), start, units = "secs"))
    )
  })

  # wait = FALSE with a timeout, so a child that hangs fails this block and
  # does not hang the suite.
  collect <- function(job) {
    out <- parallel::mccollect(job, wait = FALSE, timeout = 120)
    if (is.null(out)) {
      tools::pskill(job$pid)
      parallel::mccollect(job, wait = FALSE, timeout = 5)
      return(NULL)
    }
    out[[1]]
  }
  a <- collect(job_a)
  b <- collect(job_b)

  expect_identical(a, "a committed")
  # B stored its row without an error.
  expect_null(b$error, info = paste(b$error, collapse = " "))
  # B started while A held the lock, so B waited for the lock.
  expect_gt(b$seconds, 0.5)

  # Both rows are in the file.
  con <- DBI::dbConnect(RSQLite::SQLite(), cfg$db)
  stored <- DBI::dbGetQuery(con, "SELECT a, b FROM tab ORDER BY b")
  DBI::dbDisconnect(con)
  expect_identical(stored$a, c("from_a", "from_b"))
  expect_identical(as.integer(stored$b), c(1L, 2L))
})
