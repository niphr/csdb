#' @import data.table ggplot2
#' @importFrom stats runif
.onLoad <- function(libname, pkgname) {
  # Register S7 methods for database utilities
  S7::methods_register()
  
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
