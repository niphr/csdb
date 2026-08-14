# A DBConnection_v9 never hands out a connection that another process opened,
# and never closes one either.
#
# Layer 1 changes the recorded process ID through
# `x$.__enclos_env__$private`. Reaching into private is deliberate here, and it
# stays inside this file. A fork is the only other way to reach that state, and
# a fork does not exist on every platform this package is checked on. Layer 1
# therefore carries the contract everywhere. Layer 2 proves the same thing
# through a real fork, on the platforms that have one.
#
# Every block uses SQLite, so no block needs a database server.

other_pid <- function() {
  # A value that is not this process. The guard compares with identical(), so
  # an integer one higher is a mismatch and nothing else.
  Sys.getpid() + 1L
}

recorded_pid <- function(db) {
  db$.__enclos_env__$private$pconnection_pid
}

`recorded_pid<-` <- function(db, value) {
  db$.__enclos_env__$private$pconnection_pid <- value
  db
}

inherited_handles <- function(db) {
  db$.__enclos_env__$private$pconnections_inherited
}

# Layer 1: the process-ID logic ----

test_that("connect() records the process that opened the connection", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  expect_null(recorded_pid(db))

  db$connect()
  expect_identical(recorded_pid(db), Sys.getpid())

  db$disconnect()
})

test_that("is_connected() reports FALSE once another process owns the connection", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  expect_true(db$is_connected())

  recorded_pid(db) <- other_pid()
  expect_false(db$is_connected())

  # The guard cleared both fields, so the object holds nothing that belongs to
  # the other process.
  expect_null(db$.__enclos_env__$private$pconnection)
  expect_null(recorded_pid(db))

  DBI::dbDisconnect(inherited_handles(db)[[1]])
})

test_that("the connection binding returns NULL once another process owns it", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection
  expect_false(is.null(original))
  expect_s4_class(original, "SQLiteConnection")

  recorded_pid(db) <- other_pid()
  expect_null(db$connection)

  # The binding dropped the handle. It did not close it.
  expect_true(DBI::dbIsValid(original))

  DBI::dbDisconnect(original)
})

test_that("autoconnection opens a new handle rather than the other process's", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection
  DBI::dbExecute(original, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(original, "INSERT INTO t VALUES (7)")

  recorded_pid(db) <- other_pid()
  fresh <- db$autoconnection

  expect_false(is.null(fresh))
  # identical() and address() are both here on purpose. format() prints the
  # same text for two distinct connections, so it proves nothing.
  expect_false(identical(fresh, original))
  expect_false(
    identical(data.table::address(fresh), data.table::address(original))
  )
  # The new handle belongs to this process, and it reads the same database.
  expect_identical(recorded_pid(db), Sys.getpid())
  expect_identical(DBI::dbGetQuery(fresh, "SELECT x FROM t")$x, 7L)
  # The other process's handle is still open.
  expect_true(DBI::dbIsValid(original))

  db$disconnect()
  DBI::dbDisconnect(original)
})

test_that("connect() opens a new handle rather than the other process's", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection

  recorded_pid(db) <- other_pid()
  db$connect()

  expect_false(identical(db$connection, original))
  expect_identical(recorded_pid(db), Sys.getpid())
  expect_true(DBI::dbIsValid(original))

  db$disconnect()
  DBI::dbDisconnect(original)
})

test_that("disconnect() leaves the other process's connection open", {
  # This block is the "never close the other process's socket" guarantee.
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection
  expect_true(DBI::dbIsValid(original))
  DBI::dbExecute(original, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(original, "INSERT INTO t VALUES (7)")

  recorded_pid(db) <- other_pid()
  db$disconnect()

  # Open, and still usable. dbIsValid() alone would not show the second part.
  expect_true(DBI::dbIsValid(original))
  expect_identical(DBI::dbGetQuery(original, "SELECT x FROM t")$x, 7L)

  DBI::dbDisconnect(original)
})

test_that("the other process's connection stays reachable from the object", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection

  recorded_pid(db) <- other_pid()
  db$is_connected()

  kept <- inherited_handles(db)
  expect_length(kept, 1L)
  expect_identical(kept[[1]], original)

  # Drop the test's own references and collect. The object is then the only
  # thing holding the handle, which is what stops odbc's finalizer closing the
  # other process's socket.
  rm(original, kept)
  gc()
  kept <- inherited_handles(db)
  expect_length(kept, 1L)
  expect_true(DBI::dbIsValid(kept[[1]]))

  DBI::dbDisconnect(kept[[1]])
})

test_that("a same-process connect, use and disconnect cycle is unchanged", {
  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)

  expect_false(db$is_connected())
  db$connect()
  expect_true(db$is_connected())

  con <- db$connection
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (3)")
  expect_identical(DBI::dbGetQuery(con, "SELECT x FROM t")$x, 3L)

  db$disconnect()
  expect_false(DBI::dbIsValid(con))
  expect_false(db$is_connected())
  # disconnect() has never set pconnection back to NULL, and it still does not.
  # The binding therefore hands back the closed handle, as it did before.
  expect_false(DBI::dbIsValid(db$connection))
  # Nothing was inherited, so nothing was kept.
  expect_length(inherited_handles(db), 0L)

  # A second disconnect() is still a no-op.
  expect_silent(db$disconnect())
})

# Layer 2: a real fork ----

test_that("a forked child gets its own connection", {
  # Windows has no fork. Layer 1 above carries the contract there.
  skip_on_os("windows")

  cfg <- sqlite_dbconfig()
  db <- sqlite_connection(cfg)
  db$connect()
  original <- db$connection
  DBI::dbExecute(original, "CREATE TABLE t (x INTEGER)")
  DBI::dbExecute(original, "INSERT INTO t VALUES (7)")
  expect_identical(recorded_pid(db), Sys.getpid())

  # mcparallel, not pbmclapply: pbmclapply blocks forever on a corrupted
  # socket, and this block must fail rather than hang.
  job <- parallel::mcparallel({
    fresh <- db$autoconnection
    list(
      pid = Sys.getpid(),
      recorded = recorded_pid(db),
      same_handle = identical(fresh, original),
      kept_n = length(inherited_handles(db)),
      value = DBI::dbGetQuery(fresh, "SELECT x FROM t")$x
    )
  })
  collected <- parallel::mccollect(job, wait = FALSE, timeout = 120)
  if (is.null(collected)) {
    tools::pskill(job$pid)
    parallel::mccollect(job, wait = FALSE, timeout = 5)
    fail("the forked child returned nothing within 120 seconds")
  }
  child <- collected[[1]]
  expect_false(
    inherits(child, "try-error"),
    info = paste(as.character(child), collapse = " ")
  )

  # The child ran in another process, and recorded that process.
  expect_false(identical(child$pid, Sys.getpid()))
  expect_identical(child$recorded, child$pid)
  # It opened its own handle, and it kept the parent's.
  expect_false(child$same_handle)
  expect_identical(child$kept_n, 1L)
  # It read the right answer through its own handle.
  expect_identical(child$value, 7L)

  # The parent's connection survived the child.
  expect_true(DBI::dbIsValid(original))
  expect_identical(DBI::dbGetQuery(original, "SELECT x FROM t")$x, 7L)
  expect_true(db$is_connected())

  db$disconnect()
})
