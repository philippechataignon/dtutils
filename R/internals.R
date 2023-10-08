group <- function(dt, by) {
  data.table:::forderv(dt, by = by, retGrp = TRUE, sort = F)
}

#' @title wrapper of [
#' @examples
#' dt <- data.table(a = c(1:5, na, 7:10), b=rnorm(10), c=letters[1:10])
#'
#' DT(dt, a %% 2 == 0, .(b, d=2*b))
#' @export
DT <- data.table:::"[.data.table"
