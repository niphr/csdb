# General utility functions for csdb package

#' Generate a random UUID string
#'
#' Creates a random UUID string suitable for use as temporary identifiers.
#' The UUID is modified to ensure it starts with a letter and includes
#' additional randomness for seed compatibility.
#'
#' @return A character string containing a random UUID
#' @keywords internal
#' @noRd
random_uuid <- function() {
  x <- uuid::UUIDgenerate(F)
  x <- gsub("-", "", x)
  # the second part here allows for the usage of set.seed()
  x <- paste0("a", x, round(runif(1)*10000000))
  x
}

#' Generate a random file path
#'
#' Creates a random file path in the specified folder with the given extension.
#' The folder is created if it doesn't exist.
#'
#' @param folder Directory path where the file should be created
#' @param extension File extension to use (default: ".csv")
#' @param extra_insert Additional string to insert in filename (optional)
#' @return A character string containing the full file path
#' @keywords internal
#' @noRd
random_file <- function(folder, extension = ".csv", extra_insert = NULL) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  fs::path(folder, paste0(random_uuid(), extra_insert, extension))
}