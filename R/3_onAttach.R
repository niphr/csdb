#' @import data.table ggplot2
#' @importFrom stats runif
#' @importFrom methods isClass getClass initialize
.onLoad <- function(libname, pkgname) {
  # Ensure methods package is available for S7 operations
  if (!requireNamespace("methods", quietly = TRUE)) {
    return(NULL)
  }
  
  # Register S7 methods for database utilities
  tryCatch({
    S7::methods_register()
  }, error = function(e) {
    # Silently continue if S7 registration fails
    NULL
  })
  
  # Try to register S4 classes and update method dispatch when package loads
  # This will re-run the registration in case odbc package is now available
  tryCatch({
    register_s4_classes()
    
    # Re-register methods with updated class information
    S7::methods_register()
  }, error = function(e) {
    # Silently continue if registration fails
    NULL
  })
}

.onAttach <- function(libname, pkgname) {
    version <- tryCatch(
      utils::packageDescription("csdb", fields = "Version"),
      warning = function(w){
        1
      }
    )

  packageStartupMessage(paste0(
    "csdb ",
    version,
    "\n",
    "https://www.csids.no/csdb/"
  ))
}
