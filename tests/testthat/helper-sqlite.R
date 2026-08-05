# A dbconfig for a SQLite database held in a temporary file.
#
# `.local_envir = parent.frame()` is mandatory, not decoration. withr ties the
# deferred unlink() to that environment, and the default is this helper's own
# frame: the file would then be deleted the moment the helper returned, rather
# than when the calling test finishes.
#
# There is deliberately no skip_if_not_installed("RSQLite") anywhere in this
# file. RSQLite is in csdb's Imports, so its absence is a hard failure and must
# read as one; a skip would turn it into a silently green run.
sqlite_dbconfig <- function(
  driver = "SQLite",
  schema = "",
  .local_envir = parent.frame()
) {
  list(
    driver = driver,
    server = NULL,
    port = NULL,
    db = withr::local_tempfile(
      fileext = ".sqlite",
      .local_envir = .local_envir
    ),
    schema = schema,
    user = NULL,
    password = NULL,
    trusted_connection = NULL,
    sslmode = NULL,
    role_create_table = NULL
  )
}

# Build a DBConnection_v9 from one of those lists.
sqlite_connection <- function(dbconfig) {
  DBConnection_v9$new(
    driver = dbconfig$driver,
    server = dbconfig$server,
    port = dbconfig$port,
    db = dbconfig$db,
    schema = dbconfig$schema,
    user = dbconfig$user,
    password = dbconfig$password,
    trusted_connection = dbconfig$trusted_connection,
    sslmode = dbconfig$sslmode,
    role_create_table = dbconfig$role_create_table
  )
}
