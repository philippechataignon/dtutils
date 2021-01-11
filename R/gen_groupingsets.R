#' @title Helper for creating groupingsets
#' @param dim: character vector containing variable's names
#' @param prefix: number of \emph{non-crossing} first variables
#' @examples
#' > gen_groupingsets(c("a", "b", "c", "d", "e"), prefix=3)
#' [[1]]
#' [1] "a" "b" "c"
#'
#' [[2]]
#' [1] "a" "b" "c" "e"
#'
#' [[3]]
#' [1] "a" "b" "c" "d"
#'
#' [[4]]
#' [1] "a" "b" "c" "d" "e"
#' @export
gen_groupingsets <- function(dim, prefix = 0) {
  ret <- list()
  levels = length(dim) - prefix
  vprefix = rep(1, prefix)
  for (o in seq.int(0, 2 ^ levels - 1)) {
    q = o %/% (2^((levels - 1):0)) %% 2
    q = as.logical(c(vprefix, q))
    ret[[o + 1]] <- dim[q]
  }
  ret
}