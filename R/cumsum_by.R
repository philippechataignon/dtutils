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
  } else if (type == 3) {
    ret <- Ccummax_by(ldt, grp)
  } else if (type == 4) {
    ret <- Ccummin_by(ldt, grp)
  } else if (type == 5) {
    ret <- Ccumsurv_by(ldt, grp)
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
#' @title Cumulative max by
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is omitted, dt is considered as one group
#' @return a list with item for each var
#' @export
cummax_by <- function(dt, var = NULL, by = NULL) {
  cumope_by(dt, var, by, 3)
}
#' @title Cumulative min by
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is omitted, dt is considered as one group
#' @return a list with item for each var
#' @export
cummin_by <- function(dt, var = NULL, by = NULL) {
  cumope_by(dt, var, by, 4)
}
#' @title Cumulative surv by
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is omitted, dt is considered as one group
#' @return a list with item for each var
#' @export
cumsurv_by <- function(dt, var = NULL, by = NULL) {
  cumope_by(dt, var, by, 5)
}