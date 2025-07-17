# odbc isn't getting recognized within the R6 classes
# and the CRAN checks are therefore throwing an error
fix <- function(){
  odbc::odbc()
}
