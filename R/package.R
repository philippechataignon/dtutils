#' @useDynLib byfunc
#' @importFrom Rcpp evalCpp
#'
#' @import data.table
#'
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return
  packageStartupMessage(paste("Package byfunc", utils::packageVersion("byfunc")))
}
