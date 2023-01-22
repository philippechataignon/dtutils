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
    attr(grp, "starts") = 1
    attr(grp, "maxgrpn") = 1
  } else {
    grp = group(dt, by)
  }
  Cfirst_by(dt[,.N], grp, attr(grp, "starts"))
}