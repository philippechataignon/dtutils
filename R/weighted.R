#' @title Compute weigthed sum
#' @param dt a data.table
#' @param wt numeric vector of weights
#' @param by name(s) of variable(s) which determines groups
#' @return A dataframe
#' @export
weightedsum <- function(dt, wt, by=NULL, na.rm=F) {
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
      set(ret, j=by, value=dt[attr(grp, "starts"), get(by)])
    } else {
      set(ret, j=by, value=dt[grp[attr(grp, "starts")], get(by)])
    }
  }
  setcolorder(ret, by)
  ret
}
