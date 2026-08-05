# odbc isn't getting recognized within the R6 classes
# and the CRAN checks are therefore throwing an error
fix_r6 <- function() {
  odbc::odbc()
}

# dplyr and csutil isn't getting recognized within the S7 methods
# and the CRAN checks are therefore throwing an error
fix_s7 <- function() {
  csutil::easy_split()
  dplyr::filter()
}

# dbplyr is reached only through dplyr's own dispatch: DBTable_v9$tbl() calls
# dplyr::tbl() on a DBI connection, which lands in dplyr:::tbl.DBIConnection()
# and stops in check_dbplyr() when dbplyr is absent. No csdb code names dbplyr,
# so R CMD check reports "All declared Imports should be used" without this.
fix_dbplyr <- function() {
  dbplyr::sql()
}
