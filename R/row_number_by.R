#' @title Create a sequence 1...N by group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @return an vector of integer
#' @examples
#' dt1 <- data.table(id = rep(1:10, each=5))
#' dt1[, row_num := row_number_by(dt, "id")]
#' dt2 <- data.table(id1 = rep(1:100, each=50), id2 = rep(1:1000, each=5))
#' dt2[, row_num := row_number_by(dt, c("id1", "id2"))]
#' @export
row_number_by <- function(dt, by) {
  Crow_number_by(dt[,.N], group(dt, by))
}

#' @title Returns a logical TRUE for first row of each group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @return an vector of integer
#' @examples
#' dt <- data.table(id = rep(1:10, each=5))
#' dt[first_by(dt, "id"), pos := "F"]
#' dt[last_by(dt, "id"), pos := "L"]
#' @export
first_by <- function(dt, by) {
  Cfirst_by(dt[,.N], group(dt, by))
}

#' @title Returns a logical TRUE for last row of each group
#' @param dt a data.table
#' @param by name(s) of variable(s) which determines groups
#' @return an vector of integer
#' @examples
#' dt <- data.table(id = rep(1:10, each=5))
#' dt[first_by(dt, "id"), pos := "F"]
#' dt[last_by(dt, "id"), pos := "L"]
#' @export
last_by <- function(dt, by) {
  Clast_by(dt[,.N], group(dt, by))
}