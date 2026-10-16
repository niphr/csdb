# Caller context ----

#' Set the caller context
#'
#' @description
#' Sets a short string that names the caller, for example
#' \code{"<task>|<plan>"}. PostgreSQL shows it as the \code{application_name}
#' of each csdb connection in \code{pg_stat_activity}.
#'
#' @details
#' With a non-empty context, csdb runs \code{SET application_name} with the
#' context right after each PostgreSQL connection that
#' \code{DBConnection_v9} opens. After a failed SET, csdb runs
#' \code{SELECT 1}. A connection that answers stays open and reports nothing.
#' A connection that does not answer counts as a failed connect attempt. A
#' PostgreSQL load uses that connection, so it shows the same name.
#'
#' The name keeps only the letters A-Z and a-z, the digits 0-9 and the
#' characters \code{_ | . : -}. csdb drops every other character, and then
#' keeps the first 63 characters, the limit of PostgreSQL. A context that
#' keeps no character sends no SET.
#'
#' The context is an option, so a forked worker inherits it. A new R process,
#' for example one that \code{callr} starts, does not.
#'
#' Do not put a secret in the context. \code{pg_stat_activity} shows it.
#'
#' @param x One string. \code{""} clears the context.
#' @return Invisibly returns the previous context.
#' @export
#' @family caller context functions
#' @examples
#' previous <- csdb_set_context("my_task|plan7")
#' csdb_get_context()
#' csdb_set_context(previous)
csdb_set_context <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("x must be one string", call. = FALSE)
  }
  old <- csdb_get_context()
  options(csdb.context = x)
  return(invisible(old))
}

#' Get the caller context
#'
#' @description
#' Returns the string that \code{\link{csdb_set_context}()} set.
#'
#' @return One string, \code{""} when no context is set.
#' @export
#' @family caller context functions
#' @examples
#' csdb_get_context()
csdb_get_context <- function() {
  x <- getOption("csdb.context")
  if (is.null(x)) {
    return("")
  }
  return(x)
}

# The application_name that csdb sends to PostgreSQL: the characters of the
# context in [A-Za-z0-9_|.:-], cut to the first 63. Each kept character is
# one byte, and PostgreSQL keeps NAMEDATALEN - 1 = 63 bytes.
#
# returns  One string, "" when no character is kept.
pg_application_name <- function(context = csdb_get_context()) {
  x <- gsub("[^A-Za-z0-9_|.:-]", "", context)
  return(substr(x, 1L, 63L))
}

# The SQL statement that sets application_name, or NULL when the name is
# empty. pg_application_name() keeps no quote and no backslash, so a plain
# single-quoted literal holds the name under every setting of
# standard_conforming_strings.
pg_application_name_sql <- function(context = csdb_get_context()) {
  name <- pg_application_name(context)
  if (!nzchar(name)) {
    return(NULL)
  }
  return(paste0("SET application_name = '", name, "'"))
}

# The SET statement for a new connection of `driver`, or NULL. Only a
# PostgreSQL connection sets application_name.
connection_application_name_sql <- function(driver) {
  if (!isTRUE(driver %in% c("PostgreSQL Unicode"))) {
    return(NULL)
  }
  return(pg_application_name_sql())
}

# Check that a connection still answers, after its SET application_name
# failed. SELECT 1 that fails stops with an error, so connect() counts the
# attempt as failed and retries it.
probe_connection <- function(connection) {
  tryCatch(
    DBI::dbGetQuery(connection, "SELECT 1"),
    error = function(e) {
      stop(
        "SET application_name failed, and the connection did not answer ",
        "SELECT 1: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  return(invisible(NULL))
}
