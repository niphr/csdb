#' Does a database connection still reach its server?
#'
#' @description
#' Returns FALSE when a catalog request on \code{connection} shows that the
#' connection lost its server. Use it before cleanup SQL on a connection that
#' MAY have lost its server.
#'
#' @details
#' \code{DBI::dbIsValid()} cannot answer this question. It reports TRUE on an
#' odbc connection whose server ended the session. On such a connection, the
#' first statement gives an R error. The second statement crashes R with a
#' segfault in odbc 1.7.0 with psqlODBC 13.02, and \code{tryCatch()} cannot
#' catch a segfault.
#'
#' The function asks the server whether the table \code{csdb_connection_probe}
#' exists. odbc sends that request as an ODBC catalog call, not as a
#' statement. On a lost connection it gives an R error, also after a failed
#' statement. This was measured on 2026-10-22 against PostgreSQL 16.15.
#'
#' Only a condition whose message shows a lost connection gives FALSE. Any
#' other error or warning of the request gives TRUE, so a live connection
#' still gets its cleanup SQL. See \code{csdb_connection_lost_pattern} in the
#' source for the messages.
#'
#' PostgreSQL releases the advisory locks, the role and the temporary tables
#' of a session when the session ends. Cleanup SQL for them then has nothing
#' to do.
#'
#' @param connection A DBI connection, or NULL.
#' @return TRUE or FALSE. FALSE for NULL, for a closed connection, and when the
#'   catalog request gives a condition that shows a lost connection.
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' csdb_connection_is_alive(con)
#' DBI::dbDisconnect(con)
#' csdb_connection_is_alive(con)
#' @export
csdb_connection_is_alive <- function(connection) {
  if (is.null(connection)) {
    return(FALSE)
  }
  valid <- tryCatch(DBI::dbIsValid(connection), error = function(e) FALSE)
  if (!isTRUE(valid)) {
    return(FALSE)
  }
  alive_after <- function(condition) {
    return(!csdb_connection_lost(condition))
  }
  return(tryCatch(
    {
      DBI::dbExistsTable(connection, "csdb_connection_probe")
      TRUE
    },
    error = alive_after,
    warning = alive_after
  ))
}

# The messages that show a lost connection, case-insensitive.
#
# Measured on 2026-10-22 with odbc 1.7.0 and psqlODBC 13.02 against
# PostgreSQL 16.15. The catalog request gave these after pg_terminate_backend()
# and after a restart of the server:
#   "The connection has been lost"                  first request after the loss
#   "SQLTables unable due to the connection lost"   any later request
#   "The connection is down"                        request soon after the kill
# The nanodbc SQLSTATE field was 00000 in each case, so it carries no
# information here.
#
# The other entries are the texts that libpq and psqlODBC give when a server
# ends or drops a session. The last entry matches the SQLSTATE classes 08
# (connection exception) and 57P01 to 57P03 (operator intervention). None
# of them was measured here.
csdb_connection_lost_pattern <- paste(
  c(
    "connection has been lost",
    "connection lost",
    "connection is down",
    "server closed the connection",
    "terminating connection",
    "could not receive data",
    "could not send data",
    "no connection to the server",
    "nanodbc\\.cpp:[0-9]+: (08[0-9A-Z]{3}|57P0[1-3])"
  ),
  collapse = "|"
)

# Does `condition` show that the connection lost its server?
csdb_connection_lost <- function(condition) {
  return(grepl(
    csdb_connection_lost_pattern,
    conditionMessage(condition),
    ignore.case = TRUE
  ))
}
