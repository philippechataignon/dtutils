context("cum_ope")
library(data.table)

# pour compatibilité seed
suppressWarnings(RNGversion("3.4"))
ngrp <- 18000
nbygrp <- 50

set.seed(64)
dt <- data.table(
  id=rep(1:ngrp, each=nbygrp),
  c=sample((1:100 * 0.001 + 0.1), nbygrp * ngrp, replace=T),
  d=sample(1:100, nbygrp * ngrp, replace=T),
  e=sample(1:100, nbygrp * ngrp, replace=T)
)
dt[, alea := runif(.N)]
dt[899990, c := NA]
dt = dt[order(alea)][, alea := NULL]

dt[, z := cumsum_by(.SD, "c", "id")]
dt[, w := cumsum(c), by="id"]
test_that("cumsum1", {
  expect_equal(dt$w, dt$z)
  expect_equal(is.na(dt[is.na(c), z]), TRUE)
})

dt[, zz := cumprod_by(.SD, "c", "id")]
dt[, ww := cumprod(c), by="id"]
test_that("cumprod1", {
  expect_equal(dt$ww, dt$zz)
  expect_equal(is.na(dt[is.na(c), zz]), TRUE)
})
