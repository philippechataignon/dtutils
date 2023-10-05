cumope_by <- function(dt, var = NULL, by = NULL, type) {
  nm <- names(dt)
  if (!is.null(by) && !all(by %in% nm)) {
    stop("When by is not NULL, all names in 'by' must be dt colnames")
  }
  if (is.null(var)) {
    var <- setdiff(nm, by)
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
  ldt <- lapply(var, function(x) dt[[x]])
  names(ldt) <- var
  if (type == 1) {
    ret <- Ccumsum_by(ldt, grp)
  } else if (type == 2) {
    ret <- Ccumprod_by(ldt, grp)
  }
  ret
}

#' @title Cumulative sum by
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is omitted, dt is considered as one group
#' @return a list with item for each var
#' @export
cumsum_by <- function(dt, var = NULL, by = NULL) {
  cumope_by(dt, var, by, 1)
}
#' @title Cumulative prod by
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is omitted, dt is considered as one group
#' @return a list with item for each var
#' @export
cumprod_by <- function(dt, var = NULL, by = NULL) {
  cumope_by(dt, var, by, 2)
}