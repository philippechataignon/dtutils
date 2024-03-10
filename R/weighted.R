#' @title Compute weigthed sum
#' @param dt a data.table
#' @param wt numeric vector of weights
#' @param by name(s) of variable(s) which determines groups
#' @return
#' @examples
#' @export
weightedsum <- function(dt, wt, by=NULL, var=NULL) {
  nm <- names(dt)
  if (!is.null(by) && !all(by %in% nm)) {
    stop("When by is not NULL, all names in 'by' must be dt colnames")
  }
  if (is.null(var)) {
    var <- setdiff(nm, by)
    var <- setdiff(var, wt)
  }
  if (!all(var %in% nm)) {
    stop("All names in 'var' must be dt colnames")
  }
  tt1 <- intersect(by, var)
  if (length(tt1) > 0) {
    stop("Some variables are in 'by' and in 'var': ", tt1)
  }
  if (is.null(by)) {
    grp = numeric(0)
    attr(grp, "starts") = 1
    attr(grp, "maxgrpn") = 1
  } else {
    grp = group(dt, by)
  }
  ret <- as.data.table(Cweightedsum(as.list(dt[, ..var]), dt[[wt]], grp, var))
  if (!is.null(by)) {
    if (length(grp) == 0) {
      set(ret, j=by, value=dt[attr(grp, "starts"), ..by])
    } else {
      set(ret, j=by, value=dt[grp[attr(grp, "starts")], ..by])
    }
  }
  setcolorder(ret, c(by, var))
  ret
}
