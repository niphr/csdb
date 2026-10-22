#' Does a database connection still reach its server?
#'
#' @description
#' Returns TRUE when the server answers a catalog request on
#' \code{connection}. Use it before cleanup SQL on a connection that MAY have
#' lost its server.
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
#' PostgreSQL releases the advisory locks, the role and the temporary tables
#' of a session when the session ends. Cleanup SQL for them then has nothing
#' to do. A plain table that the session created stays.
#'
#' @param connection A DBI connection, or NULL.
#' @return TRUE or FALSE. FALSE for NULL, for a closed connection, and when the
#'   catalog request gives an error or a warning.
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
  return(tryCatch(
    {
      DBI::dbExistsTable(connection, "csdb_connection_probe")
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  ))
}
