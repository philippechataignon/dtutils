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
  e=sample(1:100, nbygrp * ngrp, replace=T),
  l=sample(c(T, F), nbygrp * ngrp, replace=T)
)
dt[, alea := runif(.N)]
dt[1, c := NA]
dt[899990, c := NA]
dt = dt[order(alea)]
dt[1, c := NA]

dt[, z := cumsum_by(.SD, "c", "id")]
dt[, w := cumsum(c), by="id"]
test_that("cumsum1", {
  expect_equal(dt[!is.na(w), w], dt[!is.na(w), z])
})

dt[, wl := cumsum(l), by="id"]
test_that("cumsum1", {
  expect_equal(sum(dt$wl), 11500431)
})

dt[, zz := cumprod_by(.SD, "c", "id")]
dt[, ww := cumprod(c), by="id"]
test_that("cumprod1", {
  expect_equal(dt[!is.na(ww), ww], dt[!is.na(ww), zz])
})

dt[, zzz := cummin_by(.SD, "c", "id")]
dt[, www := cummin(c), by="id"]
test_that("cummin1", {
  expect_equal(dt[!is.na(www), www], dt[!is.na(www), zzz])
})

dt[, zzzz := cummax_by(.SD, "c", "id")]
dt[, wwww := cummax(c), by="id"]
test_that("cummax1", {
  expect_equal(dt[!is.na(wwww), wwww], dt[!is.na(wwww), zzzz])
})
