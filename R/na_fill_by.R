#' @title
#' na_fill_by Replace NA by constant, previous or next value, optionally by group
#' @description
#' List of
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is ommitted, dt is considered as one group
#' @param type specifies type of filling : 0 constant 1 LOCF, 2 NOCB, 3 LOCF then NOCB. See note below.
#' @param fill when type=0, NA is replaced by this 'fill' value. For type = 1, 2 or 3, 'fill' is unused.
#' @param inplace when inplace = TRUE, na_fill is compute inplace
#' @return a list with item for each var
#' @note
#' When \code{type = 0}, all NA values are replaced by the 'fill' value. Ex with 'fill = 0' : \code{c(NA, 1, NA, NA, 2, NA, 3, NA)}
#' gives \code{c(0, 1, 0, 0, 2, 0, 3, 0)}
#'
#' When \code{type = 1}, LOCF = Last Observation Carry Forward. Ex : \code{c(NA, 1, NA, NA, 2, NA, 3, NA)} gives \code{c(NA, 1, 1, 1, 2, 2, 3, 3)}
#'
#' When \code{type = 2}, NOCB = Next Observation Carry Backward. Ex : \code{c(NA, 1, NA, NA, 2, NA, 3, NA)} gives \code{c(1, 1, 2, 2, 2, 3, 3, NA)}
#'
#' When \code{type = 3}, LOCF then NOCB. Ex : \code{c(NA, 1, NA, NA, 2, NA, 3, NA)} gives \code{c(1, 1, 1, 1, 2, 2, 3, 3)}.
#'
#' @examples
#' library(data.table)
#' ngrp <- 18000
#' nbygrp <- 50
#' dt <- data.table(
#'   id=rep(1:ngrp, each=nbygrp),
#'   a=sample(1:100, nbygrp * ngrp, replace=TRUE),
#'   b=sample(LETTERS, nbygrp * ngrp, replace=TRUE),
#'   c=sample((1:100 + 0.5), nbygrp * ngrp, replace=TRUE),
#'   d=sample(1:100, nbygrp * ngrp, replace=TRUE),
#'   e=sample(1:100, nbygrp * ngrp, replace=TRUE)
#' )
#' dt[(1:.N %% 2 == 0), a:=NA]
#' dt[(1:.N %% 3 == 0), b:=NA]
#' dt[(1:.N %% 3 == 0), c:=NA]
#' dt[(1:.N %% 4 == 0), d:=NA]
#' dt[(1:.N %% 5 == 0), e:=NA]
#'
#' vars = c("a", "c", "d", "e")
#'
#' dt1 <- copy(dt)
#' na_fill_by(dt1, var=vars, by="id", inplace=TRUE)
#' dt2 <- copy(dt)
#' dt2[, (vars) := na_fill_by(.SD, vars, "id")]
#' dt3 <- copy(dt)
#' dt3[, na_fill_by(.SD, vars, "id", inplace=TRUE)]
#' @export
na_fill_by <- function(dt, var = NULL, by = NULL, type = 1L, inplace = FALSE, fill = NA) {
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
  if (!length(type) == 1L && type < 0 && type > 3) {
    stop("type must be 0, 1, 2 or 3")
  }
  if (!length(inplace) == 1L) {
    stop("inplace must be TRUE or FALSE")
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
  ret <- Cna_fill_by(ldt, grp, type, inplace, fill)
  if (inplace)
    invisible(ret)
  else
    ret
}
