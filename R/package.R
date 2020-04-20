#' @useDynLib dtutils
#' @importFrom Rcpp evalCpp
#'
#' @import data.table
#'
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return
  packageStartupMessage(paste("Package dtutils", utils::packageVersion("dtutils")))
}
