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
dt[899990, c := NA]

dt[, z := cumsum_by(.SD, "c", "id")]
dt[, w := cumsum(c), by="id"]
test_that("cumsum1", {
  expect_equal(dt$w, dt$z)
  expect_equal(is.na(dt[899999, z]), TRUE)
})

dt[, zz := cumprod_by(.SD, "c", "id")]
dt[, ww := cumprod(c), by="id"]
test_that("cumprod1", {
  expect_equal(dt$ww, dt$zz)
  expect_equal(is.na(dt[899999, zz]), TRUE)
})

dt[, zzz := cummax_by(.SD, "c", "id")]
dt[, www := cummax(c), by="id"]
test_that("cummax1", {
  expect_equal(dt$www, dt$zzz)
  expect_equal(is.na(dt[899999, zzz]), TRUE)
})

dt[, zzzz := cummin_by(.SD, "c", "id")]
dt[, wwww := cummin(c), by="id"]
test_that("cummax1", {
  expect_equal(dt$wwww, dt$zzzz)
  expect_equal(is.na(dt[899999, zzzz]), TRUE)
})

dt[, s := cumsurv_by(.SD, "c", "id")]
dt[, p := 1 - cumprod(1 - c), by="id"]
test_that("cumsurv", {
  expect_equal(dt$s, dt$p)
  expect_equal(is.na(dt[899999, s]), TRUE)
})

