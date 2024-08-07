#' @title shift/lag by group
#' @param dt a data.table
#' @param var name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
#' @param by name(s) of variable(s) which determines groups (optional) ; if 'by' is ommitted, dt is considered as one group
#' @param n offset by which to lead or lag the input, 1 by default
#' @param type `lag` or `lead`, default to `lag`
#' @param fill value to use for padding before/after shift positions
#' @param inplace when inplace = TRUE, na_fill is compute inplace
#' @return a list with item for each var
#' @export
shift_by <- function(dt, var = NULL, by = NULL, n = 1,
                     type = c("lag", "lead"), fill=NA, inplace = FALSE)
{
  type = match.arg(type)
  n <- as.integer(n)
  if (type == "lag") {
    n <- -n
  }
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
  ret <- Cshift_by(ldt, grp, n, fill, inplace)
  if (inplace)
    invisible(ret)
  else
    ret
}
