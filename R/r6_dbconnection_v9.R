# DBConnection_v9 ----
#' R6 Class representing a DB schema/table
#'
#' @description
#' The fundamental way to communicate with database tables.
#'
#' @details
#' This class is a representation of a database table. It is the way that you can
#' access data (e.g. `tbl()`), manipulate data (e.g. `insert_data`, `upsert_data`),
#' and manipulate structural aspects of the database table (e.g. `add_indexes`, `drop_indexes`).
#'
#' @import data.table
#' @import R6
#' @export DBConnection_v9
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
        cat(crayon::bgRed(crayon::white("(disconnected)\n\n")))
      } else {
        cat(crayon::bgCyan(crayon::white("(connected)\n\n")))
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

