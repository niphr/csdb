# odbc isn't getting recognized within the R6 classes
# and the CRAN checks are therefore throwing an error
fix_r6 <- function(){
  odbc::odbc()
}

# dplyr and csutil isn't getting recognized within the S7 methods
# and the CRAN checks are therefore throwing an error
fix_s7 <- function(){
  csutil::easy_split()
  dplyr::filter()
}
