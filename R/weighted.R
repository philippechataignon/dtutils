#' @title Compute weighted sum
#' @param dt a data.table
#' @param wt numeric vector of weights
#' @param by name(s) of variable(s) which determines groups
#' @return A dataframe
#' @export
wsum_by <- function(dt, wt, by=NULL, na.rm=F) {
  setDT(dt)
  nm <- colnames(dt)
  var <- setdiff(nm, by)
  if (is.null(by)) {
    grp = numeric(0)
    attr(grp, "starts") = 1
    attr(grp, "maxgrpn") = 1
  } else {
    grp = group(dt, by)
  }
  ret <- Cweightedsum((dt[, var, with=F]), wt, grp, na.rm)
  if (!is.null(by)) {
    if (length(grp) == 0) {
      ret = cbind(dt[attr(grp, "starts"), ..by], ret)
    } else {
      ret = cbind(dt[grp[attr(grp, "starts")], ..by], ret)
    }
  }
  ret
}
