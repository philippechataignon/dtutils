#' @title Case_when like function with multiple constant values
#' @param ... A sequence of two-sided formulas. The left hand side (LHS)
#'   determines which values match this case. The right hand side (RHS)
#'   provides the replacement value.
#'
#'   The LHS must evaluate to a logical vector. Each logical vector can
#'   either have length 1 or a common length. All RHSs must have the same length,
#'   generally list or vector of constant values
#' @return A vector when only one-length value ; otherwise a data.table with
#'   one column per value
#' @examples
#' library(data.table)
#' dt <- data.table(a = c(1:5, NA, 7:10), b=rnorm(10), c=letters[1:10])
#' dt[, c("w", "x") :=
#'   case_when_mult(
#'     a %% 4 == 0 ~ list("p", 1.5),
#'     a %% 2 == 0 ~ list("m", 4) ,
#'     T ~ list("i", 6)
#'   )
#' ]
#' print(dt)
#' @export
case_when_mult <- function (...)
{
  formulas <- list(...)
  n <- length(formulas)
  if (n == 0) {
    stop("No cases provided")
  }
  query <- vector("list", n)
  value <- vector("list", n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (!inherits(f, "formula") || length(f) != 3) {
      stop("Case ", i, " must be a two-sided formula")
    }
    env <- environment(f)
    query[[i]] <- eval(f[[2]], env)
    if (!is.logical(query[[i]])) {
      stop("LHS of case ", i , " must be a logical, not ", typeof(query[[i]]))
    }
    value[[i]] <- eval(f[[3]], env)
  }
  lhs_len <- vapply(query, length, integer(1))
  lhs_uni <- unique(lhs_len)
  if (length(lhs_uni) == 1) {
    m <- lhs_uni[[1]]
  }
  else {
    non_atomic_len <- lhs_uni[lhs_uni != 1]
    if (length(non_atomic_len) > 1) {
      stop("All lengths of LHS must be the same or 1")
    }
    m <- non_atomic_len[[1]]
  }
  rhs_len <- vapply(value, length, integer(1))
  rhs_uni <- unique(rhs_len)
  if (length(rhs_uni) == 1) {
    p <- rhs_uni[[1]]
  } else {
    stop("All lengths of RHS must be the same")
  }
  if (p >= 100) {
    stop("Too much values in RHS: ", p, ". Did you use a variable in RHS ?")
  }
  out <- rep(NA_integer_, m)
  replaced <- rep(FALSE, m)
  for (i in seq_len(n)) {
    t <- query[[i]] & !replaced
    t[is.na(t)] <- FALSE
    out[t] <- i
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }
  dtval <- data.table()
  for (i in seq_len(p)) {
    set(dtval, j=paste0("V",i), value=unlist(lapply(value, function(x) x[[i]])))
  }
  ret <- dtval[out]
  if (ncol(dtval[out]) == 1L) {
    ret$V1
  } else {
    ret
  }
}
