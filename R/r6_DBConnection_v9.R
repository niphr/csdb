# Auth hook ----

#' Set authentication hook for database connections
#'
#' @description
#' Registers a function that csdb calls when a database connection fails. Use
#' it to refresh a Kerberos ticket, or other authentication credentials,
#' before the next connection attempt.
#'
#' @param hook A function with no arguments that performs authentication,
#'   or NULL to clear the hook.
#' @return Invisibly returns the previous hook (if any).
#' @export
#' @family auth hook functions
#' @seealso \code{\link{DBConnection_v9}}, whose \code{connect()} method calls
#'   the registered hook once, after its first failed attempt.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not mention this function.
#' @examples
#' # The hook is held in the csdb.auth_hook option. Setting one returns
#' # the previous hook, so it can be put back afterwards.
#' previous <- csdb_set_auth_hook(function() invisible(NULL))
#' is.function(csdb_get_auth_hook())
#' csdb_set_auth_hook(previous)
#' csdb_get_auth_hook()
#'
#' \donttest{
#' # A real hook refreshes a credential, for example a Kerberos ticket.
#' # Registering the hook does not call it, so this block runs on a
#' # machine that has no such script.
#' previous <- csdb_set_auth_hook(function() {
#'   system2("/bin/authenticate.sh", stdout = NULL)
#' })
#' is.function(csdb_get_auth_hook())
#'
#' # Put back whatever was registered before.
#' csdb_set_auth_hook(previous)
#' }
csdb_set_auth_hook <- function(hook) {
  if (!is.null(hook) && !is.function(hook)) {
    stop("hook must be a function or NULL", call. = FALSE)
  }
  old_hook <- getOption("csdb.auth_hook")
  options(csdb.auth_hook = hook)
  return(invisible(old_hook))
}

#' Get the current authentication hook
#'
#' @description
#' Returns the currently registered authentication hook function.
#'
#' @return The current authentication hook function, or NULL when no hook is
#'   set.
#' @export
#' @family auth hook functions
#' @seealso \code{\link{DBConnection_v9}}, whose \code{connect()} method calls
#'   this function to look up the hook.
#'   The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, does not mention this function.
#' @examples
#' # Returns NULL when no hook has been set
#' csdb_get_auth_hook()
#'
#' \donttest{
#' # Register a hook and then read it back. Registering does not call it.
#' previous <- csdb_set_auth_hook(function() {
#'   system2("/bin/kinit", stdout = NULL)
#' })
#' hook <- csdb_get_auth_hook()
#' is.function(hook)
#'
#' csdb_set_auth_hook(previous)
#' }
csdb_get_auth_hook <- function() {
  return(getOption("csdb.auth_hook"))
}

# Password hook ----

#' Set the password hook for PostgreSQL connections
#'
#' @description
#' Registers a function that returns the password for each new PostgreSQL
#' connection and each \code{psql} load. Use it for a password that expires,
#' such as an Entra access token.
#'
#' @details
#' csdb calls the hook with no arguments, and uses its current return value as
#' the password:
#' \itemize{
#'   \item once for each attempt of \code{DBConnection_v9$connect()} with the
#'     driver \code{"PostgreSQL Unicode"}.
#'   \item once for each PostgreSQL load or upsert that runs \code{psql}.
#' }
#'
#' The hook MUST return a single non-empty string. Any other value stops the
#' connection or the load with an error that names the hook. The error does not
#' show the value.
#'
#' The hook applies to PostgreSQL only. A SQL Server, SQLite or other
#' connection, and a \code{bcp} load, use the \code{password} in their settings.
#' csdb therefore never sends the hook value to another kind of server.
#'
#' The message of a failed connection shows \code{***} in place of the hook
#' value.
#'
#' @param hook A function with no arguments that returns the password, or NULL
#'   to clear the hook.
#' @return Invisibly returns the previous hook, or NULL when no hook was set.
#' @export
#' @family password hook functions
#' @seealso \code{\link{csdb_set_auth_hook}} registers a function that
#'   \code{DBConnection_v9$connect()} calls after its first failed attempt.
#' @examples
#' # The hook is held in the csdb.password_hook option. Registering the hook
#' # does not call it.
#' previous <- csdb_set_password_hook(function() "example-token")
#' is.function(csdb_get_password_hook())
#'
#' # Put back whatever was registered before.
#' csdb_set_password_hook(previous)
#' csdb_get_password_hook()
csdb_set_password_hook <- function(hook) {
  if (!is.null(hook) && !is.function(hook)) {
    stop("hook must be a function or NULL", call. = FALSE)
  }
  old_hook <- getOption("csdb.password_hook")
  options(csdb.password_hook = hook)
  return(invisible(old_hook))
}

#' Get the current password hook
#'
#' @description
#' Returns the function that \code{\link{csdb_set_password_hook}} registered.
#'
#' @return The current password hook function, or NULL when no hook is set.
#' @export
#' @family password hook functions
#' @examples
#' # Returns NULL when no hook is set.
#' csdb_get_password_hook()
csdb_get_password_hook <- function() {
  return(getOption("csdb.password_hook"))
}

# The current value of the password hook.
#
# The error messages name the hook and never show what it returned, because
# that value is a credential.
#
# returns  NULL when no hook is set, else the single non-empty string that the
#          hook returned.
password_hook_value <- function() {
  hook <- csdb_get_password_hook()
  if (is.null(hook)) {
    return(NULL)
  }
  value <- tryCatch(
    hook(),
    error = function(e) {
      stop(
        "The csdb password hook (csdb_set_password_hook()) failed: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      "The csdb password hook (csdb_set_password_hook()) must return a ",
      "single non-empty string. It returned an object of class ",
      paste(class(value), collapse = "/"),
      " and length ",
      length(value),
      ".",
      call. = FALSE
    )
  }
  return(value)
}

# Replace every copy of a secret in a text with "***".
#
# The ODBC driver receives the secret in braces, with every "}" doubled, so
# that form is replaced as well.
#
# x        A character vector.
# secret   NULL, or one string.
# returns  x, with every copy of secret replaced.
redact_secret <- function(x, secret) {
  if (is.null(secret)) {
    return(x)
  }
  quoted <- unclass(odbc_quote_value(secret))
  x <- gsub(quoted, "***", x, fixed = TRUE)
  x <- gsub(secret, "***", x, fixed = TRUE)
  return(x)
}

# ODBC connection arguments ----

# Quote one value for an ODBC connection string.
#
# odbc pastes every argument into one string of `key=value` pairs, separated
# by `;`. A raw `;` in a password therefore ends the password, and the rest
# reads as more pairs. The ODBC rule puts the value in braces and doubles
# every `}` inside it. odbc::quote_value() cannot do this: it stops on a value
# that holds both quote characters.
#
# I() tells odbc that the value is already quoted. Without it odbc prints a
# message about the special characters on every connection.
#
# Measured on 2026-10-06 against psqlODBC 13.02 in rbase:4.6.1. The driver
# reads a password in braces with this rule. It reads a user name in braces
# as the literal name, braces included. So this function quotes the password
# only.
#
# x        One character string, or NULL.
# returns  The quoted value, of class AsIs, or NULL when x is NULL.
odbc_quote_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  return(I(paste0("{", gsub("}", "}}", x, fixed = TRUE), "}")))
}

# The arguments that DBConnection_v9 gives DBI::dbConnect() for an ODBC
# driver, after odbc::odbc().
#
# config   The config list of a DBConnection_v9 object.
# returns  A named list.
odbc_connect_args <- function(config) {
  if (
    config$trusted_connection == "yes" &&
      config$driver %in% c("ODBC Driver 17 for SQL Server")
  ) {
    return(list(
      driver = config$driver,
      server = config$server,
      port = config$port,
      trusted_connection = "yes"
    ))
  } else if (config$driver %in% c("ODBC Driver 17 for SQL Server")) {
    return(list(
      driver = config$driver,
      server = config$server,
      port = config$port,
      uid = config$user,
      pwd = odbc_quote_value(config$password),
      encoding = "utf8"
    ))
  } else if (
    config$sslmode == "require" &&
      config$driver %in% c("PostgreSQL Unicode")
  ) {
    return(list(
      driver = config$driver,
      server = config$server,
      port = config$port,
      uid = config$user,
      password = odbc_quote_value(config$password),
      database = config$db,
      sslmode = "require"
    ))
  } else if (config$driver %in% c("PostgreSQL Unicode")) {
    return(list(
      driver = config$driver,
      server = config$server,
      port = config$port,
      uid = config$user,
      password = odbc_quote_value(config$password),
      database = config$db
    ))
  } else {
    return(list(
      driver = config$driver,
      server = config$server,
      port = config$port,
      user = config$user,
      password = odbc_quote_value(config$password),
      encoding = "utf8"
    ))
  }
}

# DBConnection_v9 ----
#' R6 Class representing a database connection
#'
#' @description
#' A database connection manager that handles connections to various database
#' systems including Microsoft SQL Server and PostgreSQL. This class provides
#' connection management, authentication, and automatic reconnection.
#'
#' @details
#' The DBConnection_v9 class holds the database connection logic and provides a
#' consistent interface to different database systems. It supports both trusted
#' connections and user/password authentication. It handles connection
#' failures, and it reconnects automatically.
#'
#' Key features:
#' \itemize{
#'   \item Support for multiple database systems (SQL Server, PostgreSQL).
#'   \item Automatic connection management with retry logic.
#'   \item Secure credential handling.
#'   \item Connection status monitoring.
#'   \item Graceful error handling and recovery.
#'   \item A connection is never shared with another process.
#' }
#'
#' @section Fork safety:
#' A connection belongs to the process that opened it. After a fork, the child
#' holds a copy of this object and a copy of the parent's connection. Both
#' processes then use one socket. PostgreSQL can return wrong results and
#' report no error. Measured against the NorSySS server on 2026-08-14. A child
#' asked for \code{select 4} and read 3. The parent asked for
#' \code{select 999} and read 2. \code{DBI::dbIsValid()} reports TRUE on such a
#' handle, so nothing else detects it.
#'
#' This class records the process that opens each connection. It drops any
#' connection whose recorded process is not the current one.
#' \code{is_connected()} then returns FALSE, \code{connection} returns NULL,
#' and \code{autoconnection} opens a new connection for the current process.
#' \code{disconnect()} closes nothing, because the handle belongs to the other
#' process.
#'
#' The object never closes an inherited handle, and it keeps a reference to it.
#' Both parts are needed. A close, by \code{DBI::dbDisconnect()} or by the
#' garbage collector, would close the other process's socket.
#'
#' @import data.table
#' @import R6
#' @export DBConnection_v9
#' @family database classes
#' @seealso The introduction vignette,
#'   \code{vignette("csdb", package = "csdb")}, which creates one of these,
#'   connects, and disconnects again.
#'   \code{\link{csdb_set_auth_hook}} registers the function that
#'   \code{connect()} calls after its first failed attempt.
#' @examples
#' # Creating the object stores the settings. It opens no connection,
#' # so this runs without a database server.
#' db <- DBConnection_v9$new(
#'   driver = "PostgreSQL Unicode",
#'   server = "localhost",
#'   port = 5432,
#'   db = "mydb",
#'   user = "myuser",
#'   password = "mypass"
#' )
#' db$is_connected()
#' db
#'
#' \donttest{
#' # The full cycle, on SQLite. SQLite needs no server, so this block runs
#' # anywhere. Only the driver and the db argument change for a server.
#' # vignette("backends", package = "csdb") puts the two configurations
#' # side by side.
#' sqlite_db <- DBConnection_v9$new(
#'   driver = "SQLite",
#'   db = tempfile(fileext = ".sqlite")
#' )
#'
#' sqlite_db$connect()
#' sqlite_db$is_connected()
#' DBI::dbListTables(sqlite_db$connection)
#'
#' sqlite_db$disconnect()
#' sqlite_db$is_connected()
#'
#' # $autoconnection opens the file again, so a read after a disconnect
#' # still works.
#' DBI::dbListTables(sqlite_db$autoconnection)
#' sqlite_db$disconnect()
#' }
DBConnection_v9 <- R6::R6Class(
  "DBConnection_v9",

  # public ----
  public = list(
    #' @field config Configuration details of the database.
    config = NULL,

    #' @description
    #' Create a new DBConnection_v9 object.
    #'
    #' @param driver Driver.
    #' @param server Server.
    #' @param port Port.
    #' @param db DB.
    #' @param schema Schema (e.g. "dbo").
    #' @param user User.
    #' @param password Password.
    #' @param trusted_connection NULL or "yes".
    #' @param sslmode NULL or "require".
    #' @param role_create_table NULL or the role to take when creating tables.
    #' @return A new `DBConnection_v9` object.
    initialize = function(
      driver = NULL,
      server = NULL,
      port = NULL,
      db = NULL,
      schema = NULL,
      user = NULL,
      password = NULL,
      trusted_connection = NULL,
      sslmode = NULL,
      role_create_table = NULL
    ) {
      force(driver)
      force(server)
      force(port)
      force(db)
      force(schema)
      force(user)
      force(password)
      force(trusted_connection)
      force(sslmode)
      force(role_create_table)

      if (is.null(trusted_connection)) {
        trusted_connection <- "x"
      }
      if (is.null(sslmode)) {
        sslmode <- "x"
      }
      if (is.null(role_create_table)) {
        role_create_table <- "x"
      }

      return(
        self$config <- list(
          driver = driver,
          server = server,
          port = port,
          db = db,
          schema = schema,
          user = user,
          password = password,
          trusted_connection = trusted_connection,
          sslmode = sslmode,
          role_create_table = role_create_table
        )
      )
    },

    #' @description
    #' Is the DB schema connected?
    #'
    #' A connection that another process opened does not count. The method
    #' drops that connection first, and then reports FALSE.
    #' @return TRUE/FALSE.
    is_connected = function() {
      private$discard_inherited_connection()
      retval <- FALSE
      if (is.null(private$pconnection)) {
        retval <- FALSE
      } else if (DBI::dbIsValid(private$pconnection)) {
        tryCatch(
          {
            z <- private$pconnection |>
              DBI::dbListTables()
            retval <- TRUE
          },
          error = function(e) {
            return(retval <<- FALSE)
          },
          warning = function(e) {
            return(retval <<- FALSE)
          }
        )
      }
      return(retval)
    },

    #' @description
    #' Class-specific print function.
    #' @param ... Not used.
    print = function(...) {
      if (!self$is_connected()) {
        if (requireNamespace("crayon", quietly = TRUE)) {
          cat(crayon::bgRed(crayon::white("(disconnected)\n\n")))
        } else {
          cat("(disconnected)\n\n")
        }
      } else {
        if (requireNamespace("crayon", quietly = TRUE)) {
          cat(crayon::bgCyan(crayon::white("(connected)\n\n")))
        } else {
          cat("(connected)\n\n")
        }
      }
      if (identical(toupper(self$config$driver), "SQLITE")) {
        # SQLite reads none of the server settings, so printing them would
        # only invite someone to set them.
        cat("Driver:             ", self$config$driver, "\n")
        cat("File:               ", self$config$db, "\n")
      } else {
        cat("Driver:             ", self$config$driver, "\n")
        cat("Server:             ", self$config$server, "\n")
        cat("Port:               ", self$config$port, "\n")
        cat("DB:                 ", self$config$db, "\n")
        cat("User:               ", self$config$user, "\n")
        cat(
          "Password:           ",
          paste0(rep("*", nchar(self$config$password)), collapse = ""),
          "\n"
        )
        if (self$config$driver %in% c("PostgreSQL Unicode")) {
          cat("SSL mode:           ", self$config$sslmode, "\n")
        } else {
          cat("Trusted connection: ", self$config$trusted_connection, "\n")
        }
      }
      cat("\n")

      return(invisible(self))
    },

    #' @description
    #' Connect to the database.
    #'
    #' The method drops a connection that another process opened, and then
    #' opens a connection for the current process.
    #' @param attempts Number of attempts to connect.
    connect = function(attempts = 2) {
      private$discard_inherited_connection()
      success <- FALSE
      auth_hook_called <- FALSE

      for (i in 1:attempts) {
        tryCatch(
          {
            private$connect_once()
            success <- TRUE
          },
          error = function(e) {
            return(message("Attempt ", i, ": ", e))
          }
        )
        if (success) {
          break()
        }

        # If first attempt failed and we have an auth hook, call it
        if (i == 1 && !auth_hook_called) {
          auth_hook <- csdb_get_auth_hook()
          if (!is.null(auth_hook)) {
            message("Calling authentication hook...")
            tryCatch(
              {
                auth_hook()
                auth_hook_called <- TRUE
              },
              error = function(e) {
                return(message("Auth hook failed: ", conditionMessage(e)))
              }
            )
          }
        }

        # sleep to give the db time to recover
        # don't need to sleep on the last failed run
        if (i != attempts) Sys.sleep(i)
      }
      if (!success) {
        stop(
          "Failed to connect to database after ",
          attempts,
          " attempts",
          call. = FALSE
        )
      }
    },

    #' @description
    #' Disconnect from the database.
    #'
    #' The method closes only a connection that this process opened. A
    #' connection that another process opened stays open.
    disconnect = function() {
      private$discard_inherited_connection()
      if (self$is_connected()) {
        return(suppressWarnings(DBI::dbDisconnect(private$pconnection)))
      }
    }
  ),

  # active ----
  active = list(
    #' @field connection Database connection. NULL when another process opened
    #'   it.
    connection = function() {
      private$discard_inherited_connection()
      return(private$pconnection)
    },
    #' @field autoconnection Database connection that automatically connects if
    #'   possible. After a fork it opens a connection for the current process.
    autoconnection = function() {
      private$discard_inherited_connection()
      self$connect()
      return(private$pconnection)
    }
  ),

  # private ----
  private = list(
    pconnection = NULL,
    # The process that opened `pconnection`, as Sys.getpid() reported it.
    # `connect_once()` sets it. It is NULL whenever `pconnection` is NULL.
    pconnection_pid = NULL,
    # Handles that another process opened. `discard_inherited_connection()`
    # moves a handle here rather than closing it.
    pconnections_inherited = list(),
    # Drop a connection that another process opened.
    #
    # A fork copies this object, so the child holds the parent's handle and
    # both processes use one socket. PostgreSQL can then return wrong results
    # and report no error. DBI::dbIsValid() reports TRUE on such a handle, and
    # the dbListTables() probe in is_connected() also succeeds, so nothing else
    # detects it.
    #
    # Two details are load-bearing. This method MUST NOT call
    # DBI::dbDisconnect(): that closes the other process's socket, which is the
    # corruption the method prevents. It MUST also keep the handle reachable,
    # or the garbage collector runs odbc's finalizer and closes that socket
    # anyway. Both details come from measurements against the norsyss-postgres
    # server on 2026-08-14.
    discard_inherited_connection = function() {
      if (is.null(private$pconnection)) {
        return(invisible(NULL))
      }
      if (identical(private$pconnection_pid, Sys.getpid())) {
        return(invisible(NULL))
      }
      private$pconnections_inherited[[
        length(private$pconnections_inherited) + 1L
      ]] <- private$pconnection
      private$pconnection <- NULL
      private$pconnection_pid <- NULL
      return(invisible(NULL))
    },
    connect_once = function() {
      if (self$is_connected()) {
        return()
      }

      # The value of the password hook, when this attempt sends one. The error
      # handler below redacts it.
      hook_password <- NULL

      # create connection
      tryCatch(
        {
          if (identical(toupper(self$config$driver), "SQLITE")) {
            # SQLite is a file, not a server. `server`, `port`, `user`,
            # `password`, `trusted_connection`, `sslmode` and
            # `role_create_table` are all ignored, and `db` is the file path.
            # This arm has to sit above the unguarded `else` below, which
            # issues a generic odbc::odbc() call and would otherwise swallow
            # every driver string that is not one of the two ODBC ones.
            db_directory <- dirname(self$config$db)
            if (!dir.exists(db_directory)) {
              dir.create(db_directory, recursive = TRUE, showWarnings = FALSE)
            }
            # extended_types = TRUE is load-bearing, not a nicety: without it
            # a DATE column reads back as the integer 18262 rather than a
            # Date, and validator_field_contents_csfmt_rts_data_v1() rejects
            # it.
            private$pconnection <- DBI::dbConnect(
              RSQLite::SQLite(),
              dbname = self$config$db,
              extended_types = TRUE
            )
          } else {
            # odbc_connect_args() holds the five ODBC branches, and it quotes
            # the password.
            args <- odbc_connect_args(self$config)
            # The password hook applies to PostgreSQL only, so its value never
            # reaches another kind of server. Both PostgreSQL branches of
            # odbc_connect_args() send the password as `password`.
            if (self$config$driver %in% c("PostgreSQL Unicode")) {
              hook_password <- password_hook_value()
            }
            if (!is.null(hook_password)) {
              args[["password"]] <- odbc_quote_value(hook_password)
            }
            private$pconnection <- do.call(
              DBI::dbConnect,
              c(list(odbc::odbc()), args)
            )
          }
          # Record the owning process here, and not inside each branch. Both
          # DBI::dbConnect() calls above run in this process, so one record
          # covers both, and it also covers a branch added later.
          # A failed connection never reaches this line, because the error
          # handler below calls stop().
          private$pconnection_pid <- Sys.getpid()
        },
        error = function(cond) {
          stop(
            "Could not connect to database server '",
            self$config$server,
            "'\n",
            "Original error: ",
            redact_secret(conditionMessage(cond), hook_password),
            call. = FALSE
          )
        }
      )

      # use db if available
      # SQLite is excluded because the file is already the database: it has no
      # USE statement, and issuing one is a syntax error.
      if (
        !is.null(self$config$db) &&
          !isTRUE(self$config$driver == "PostgreSQL Unicode") &&
          !identical(toupper(self$config$driver), "SQLITE")
      ) {
        return(tryCatch(
          {
            a <- DBI::dbExecute(
              private$pconnection,
              glue::glue({
                "USE {self$config$db};"
              })
            )
          },
          error = function(e) {
            stop(
              "Database '",
              self$config$db,
              "' does not exist",
              call. = FALSE
            )
          }
        ))
      }
    },
    finalize = function() {
      # message("Closing connection automatically")
      return(self$disconnect())
    }
  )
)
