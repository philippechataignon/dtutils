group <- function(dt, by) {
  data.table:::forderv(dt, by = by, retGrp = TRUE, sort = F)
}
