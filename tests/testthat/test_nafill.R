context("na_fill_by")
library(data.table)

dt <- CJ(id=1:2, val=c(NA, 1, NA, NA, 2, NA, 3, NA), sorted=F)

test_that("na_fill_elem", {
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", by="id", type=1)]$val, c(NA, 1, 1, 1, 2, 2, 3, 3, NA, 1, 1, 1, 2, 2, 3, 3))
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", by="id", type=2)]$val, c(1, 1, 2, 2, 2, 3, 3, NA, 1, 1, 2, 2, 2, 3, 3, NA))
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", by="id", type=3)]$val, c(1, 1, 1, 1, 2, 2, 3, 3, 1, 1, 1, 1, 2, 2, 3, 3))
})

# pour compatibilité seed
suppressWarnings(RNGversion("3.4"))
ngrp <- 18000
nbygrp <- 50

set.seed(64)
dt <- data.table(
  id=rep(1:ngrp, each=nbygrp),
  a=sample(1:100, nbygrp * ngrp, replace=T),
  b=sample(LETTERS, nbygrp * ngrp, replace=T),
  c=sample((1:100 + 0.5), nbygrp * ngrp, replace=T),
  d=sample(1:100, nbygrp * ngrp, replace=T),
  e=sample(1:100, nbygrp * ngrp, replace=T)
)
dt[(1:.N %% 2 == 0), a:=NA]
dt[(1:.N %% 3 == 0), b:=NA]
dt[(1:.N %% 3 == 0), c:=NA]
dt[(1:.N %% 4 == 0), d:=NA]
dt[(1:.N %% 5 == 0), e:=NA]

vars = c("a", "b", "c", "d", "e")

dt1 <- copy(dt)
system.time(na_fill_by(dt1, var=vars, by="id", inplace=T))
test_that("na_fill_by1", {
  expect_equal(dt1[2, a], 5)
})

dt2 <- copy(dt)
system.time(dt2[, (vars) := na_fill_by(.SD, var=vars, by="id")])
test_that("na_fill_by2", {
  expect_equal(dt2[2, a], 5)
  expect_identical(dt1, dt2)
})

dt3 <- copy(dt)
system.time(dt3[, na_fill_by(.SD, vars, "id", inplace=T)])
test_that("na_fill_by3", {
  expect_equal(dt3[2, a], 5)
  expect_identical(dt1, dt3)
})

vars = c("a", "c", "d", "e")

dt4 <- copy(dt)
system.time(na_fill_by(dt4, var=vars, by="id", inplace=T))
test_that("na_fill_by4", {
  expect_equal(dt4[2, a], 5)
})

dt5 <- copy(dt)
system.time(dt5[, (vars) := na_fill_by(.SD, var=vars, by="id")])
test_that("na_fill_by5", {
  expect_equal(dt5[2, a], 5)
  expect_identical(dt4, dt5)
})

dt6 <- copy(dt)
system.time(dt6[, na_fill_by(.SD, vars, "id", inplace=T)])
test_that("na_fill_by6", {
  expect_equal(dt6[2, a], 5)
  expect_identical(dt4, dt6)
})