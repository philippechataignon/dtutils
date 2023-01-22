#' @title Returns a logical TRUE for first row of each group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @return an vector of integer
#' @examples
#' dt <- data.table(id = rep(1:10, each=5))
#' dt[first_by(dt, "id"), pos := "F"]
#' dt[last_by(dt, "id"), pos := "L"]
#' @export
first_by <- function(dt, by=NULL) {
  if (is.null(by)) {
    grp = numeric(0)
    starts = 1
  } else {
    grp = group(dt, by)
    starts = attr(grp, "starts")
  }
  Cfirst_by(dt[,.N], grp, starts)
}

#' @title Returns a logical TRUE for last row of each group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @return an vector of integer
#' @examples
#' dt <- data.table(id = rep(1:10, each=5))
#' dt[last_by(dt, "id"), pos := "F"]
#' dt[last_by(dt, "id"), pos := "L"]
#' @export
last_by <- function(dt, by=NULL) {
  if (is.null(by)) {
    grp = numeric(0)
    starts = 1
  } else {
    grp = group(dt, by)
    starts = attr(grp, "starts")
  }
  Clast_by(dt[,.N], grp, starts)
}

row_number_by <- function(dt, by=NULL) {
  if (is.null(by)) {
    grp = numeric(0)
    starts = 1
  } else {
    grp = group(dt, by)
    starts = attr(grp, "starts")
  }
  Crow_number_by(dt[,.N], grp, starts)
}