#' @title Replace NA by constant, previous or next value, optionally by group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @param var name(s) of variable(s) with atomic values ; if 'var' is missing, all variables not in 'by' are selected
#' @return a named list with item for each var
#' @examples
#' library(data.table)
#' ngrp <- 18000
#' nbygrp <- 50
#' dt <- data.table(
#'   id=rep(1:ngrp, each=nbygrp) * 2,
#'   a=sample(1:100, nbygrp * ngrp, replace=TRUE),
#'   b=sample(LETTERS, nbygrp * ngrp, replace=TRUE),
#'   c=sample((1:100 + 0.5), nbygrp * ngrp, replace=TRUE),
#'   d=sample(1:100, nbygrp * ngrp, replace=TRUE),
#'   e=sample(1:100, nbygrp * ngrp, replace=TRUE)
#' )
#' dt[(1:.N %% 2 == 1), a:=NA]
#' dt[(1:.N %% 3 < 2), b:=NA]
#' dt[(1:.N %% 4 < 3), c:=NA]
#' dt[(1:.N %% 5 < 4), d:=NA]
#' dt[(1:.N %% 6 < 5), e:=NA]
#'
#' coalesce_by(dt, by="id")
#' vars = c("b", "c", "e")
#' coalesce_by(dt, var=vars, by=c("id", "a"))
#' coalesce_by(dt, var=vars, by=c("a"))
#' @export
coalesce_by <- function(dt, by=NULL, var=NULL) {
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
  ret <- as.data.table(Ccoalesce_by(as.list(dt[, ..var]), grp, var))
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