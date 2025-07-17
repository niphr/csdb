# DBConnection_v9 ----
#' R6 Class representing a database connection
#'
#' @description
#' A robust database connection manager that handles connections to various database systems
#' including Microsoft SQL Server and PostgreSQL. This class provides connection management,
#' authentication, and automatic reconnection capabilities.
#'
#' @details
#' The DBConnection_v9 class encapsulates database connection logic and provides a consistent
#' interface for connecting to different database systems. It supports both trusted connections
#' and user/password authentication, handles connection failures gracefully, and provides
#' automatic reconnection functionality.
#'
#' Key features:
#' \itemize{
#'   \item Support for multiple database systems (SQL Server, PostgreSQL)
#'   \item Automatic connection management with retry logic
#'   \item Secure credential handling
#'   \item Connection status monitoring
#'   \item Graceful error handling and recovery
#' }
#'
#' @import data.table
#' @import R6
#' @export DBConnection_v9
#' @examples
#' \dontrun{
#' # Create a SQL Server connection
#' db_config <- DBConnection_v9$new(
#'   driver = "ODBC Driver 17 for SQL Server",
#'   server = "localhost",
#'   port = 1433,
#'   db = "mydb",
#'   user = "myuser",
#'   password = "mypass"
#' )
#' 
#' # Connect to the database
#' db_config$connect()
#' 
#' # Check connection status
#' db_config$is_connected()
#' 
#' # Use the connection
#' tables <- DBI::dbListTables(db_config$connection)
#' 
#' # Disconnect when done
#' db_config$disconnect()
#' 
#' # PostgreSQL example
#' pg_config <- DBConnection_v9$new(
#'   driver = "PostgreSQL",
#'   server = "localhost",
#'   port = 5432,
#'   db = "mydb",
#'   user = "myuser",
#'   password = "mypass"
#' )
#' 
#' pg_config$connect()
#' # ... use connection ...
#' pg_config$disconnect()
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
    #' @param driver Driver
    #' @param server Server
    #' @param port Port
    #' @param db DB
    #' @param schema Schema (e.g. "dbo")
    #' @param user User
    #' @param password Password
    #' @param trusted_connection NULL or "yes"
    #' @param sslmode NULL or "require"
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

      if(is.null(trusted_connection)) trusted_connection <- "x"
      if(is.null(sslmode)) sslmode <- "x"
      if(is.null(role_create_table)) role_create_table <- "x"

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
    },

    #' @description
    #' Is the DB schema connected?
    #' @return TRUE/FALSE
    is_connected = function() {
      retval <- FALSE
      if (is.null(private$pconnection)) {
        retval <- FALSE
      } else if (DBI::dbIsValid(private$pconnection)) {
        tryCatch({
          z <- private$pconnection %>%
            DBI::dbListTables()
          retval <- TRUE
        }, error = function(e){
          retval <<- FALSE
        }, warning = function(e){
          retval <<- FALSE
        })
      }
      return(retval)
    },

    #' @description
    #' Class-specific print function.
    #' @param ... Not used.
    print = function(...) {
      if (!self$is_connected()) {
        if(requireNamespace("crayon", quietly = TRUE)) {
          cat(crayon::bgRed(crayon::white("(disconnected)\n\n")))
        } else {
          cat("(disconnected)\n\n")
        }
      } else {
        if(requireNamespace("crayon", quietly = TRUE)) {
          cat(crayon::bgCyan(crayon::white("(connected)\n\n")))
        } else {
          cat("(connected)\n\n")
        }
      }
      cat("Driver:             ", self$config$driver, "\n")
      cat("Server:             ", self$config$server, "\n")
      cat("Port:               ", self$config$port, "\n")
      cat("DB:                 ", self$config$db, "\n")
      cat("User:               ", self$config$user, "\n")
      cat("Password:           ", paste0(rep("*", nchar(self$config$password)), collapse=""), "\n")
      cat("Trusted connection: ", self$config$trusted_connection, "\n")
      cat("\n")

      invisible(self)
    },

    #' @description
    #' Connect to the database
    #' @param attempts Number of attempts to be made to try to connect
    connect = function(attempts = 2) {
      success <- FALSE
      for(i in 1:attempts){
        tryCatch({
          private$connect_once()
          success <- TRUE
        },
        error = function(e){
          message("Attempt ", i,": ", e)
        })
        if(success) break()
        # sleep to give the db time to recover
        # don't need to sleep on the last failed run
        if(i!=attempts) Sys.sleep(i)
      }
      if(!success) stop("Failed to connect to database after ", attempts, " attempts")
    },

    #' @description
    #' Disconnect from the database
    disconnect = function() {
      if(self$is_connected()) suppressWarnings(DBI::dbDisconnect(private$pconnection))
    }
  ),

  # active ----
  active = list(
    #' @field connection Database connection.
    connection = function(){
      private$pconnection
    },
    #' @field autoconnection Database connection that automatically connects if possible.
    autoconnection = function(){
      self$connect()
      return(private$pconnection)
    }
  ),

  # private ----
  private = list(
    pconnection = NULL,
    connect_once = function() {
      if(self$is_connected()){
        return()
      }

      # create connection
      tryCatch(
        {
          if (self$config$trusted_connection == "yes" & self$config$driver %in% c("ODBC Driver 17 for SQL Server")) {
            private$pconnection <- DBI::dbConnect(
              odbc::odbc(),
              driver = self$config$driver,
              server = self$config$server,
              port = self$config$port,
              trusted_connection = "yes"
            )
          } else if (self$config$driver %in% c("ODBC Driver 17 for SQL Server")) {
            private$pconnection <- DBI::dbConnect(
              odbc::odbc(),
              driver = self$config$driver,
              server = self$config$server,
              port = self$config$port,
              uid = self$config$user,
              pwd = self$config$password,
              encoding = "utf8"
            )
          } else if (self$config$sslmode == "require" & self$config$driver %in% c("PostgreSQL Unicode")) {
            private$pconnection <- DBI::dbConnect(
              odbc::odbc(),
              driver = self$config$driver,
              server = self$config$server,
              port = self$config$port,
              uid = self$config$user,
              password = self$config$password,
              database = self$config$db,
              sslmode = "require"
            )
          } else if (self$config$driver %in% c("PostgreSQL Unicode")) {
            private$pconnection <- DBI::dbConnect(
              odbc::odbc(),
              driver = self$config$driver,
              server = self$config$server,
              port = self$config$port,
              uid = self$config$user,
              password = self$config$password,
              database = self$config$db
            )
          } else {
            private$pconnection <- DBI::dbConnect(
              odbc::odbc(),
              driver = self$config$driver,
              server = self$config$server,
              port = self$config$port,
              user = self$config$user,
              password = self$config$password,
              encoding = "utf8"
            )
          }
        },
        error=function(cond){
          stop("Could not connect to database server '", self$config$server,"'")
        }
      )

      # use db if available
      if(!is.null(self$config$db) & !self$config$driver %in% c("PostgreSQL Unicode")){
        tryCatch(
          {
            a <- DBI::dbExecute(private$pconnection, glue::glue({
              "USE {self$config$db};"
            }))
          },
          error = function(e) {
            stop("Database '", self$config$db,"' does not exist")
          }
        )
      }
    },
    finalize = function() {
      # message("Closing connection automatically")
      self$disconnect()
    }
  )
)

