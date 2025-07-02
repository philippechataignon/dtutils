#' @export
rle_cpp <- function(dt)
{
  Crle_cpp(dt)
}

#' @export
nfirst_by <- function(dt)
{
  Cnfirst_by(as.character(dt))
}
