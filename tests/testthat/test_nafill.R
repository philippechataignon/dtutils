context("na_fill_by")
library(data.table)

dt <- data.table(val=c(NA, 1, NA, NA, 2, NA, 3, NA))
test_that("na_fill_elem", {
  expect_equal(dt[, na_replace(dt=.SD, var="val", fill=9)]$val, c(9, 1, 9, 9, 2, 9, 3, 9))
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", type=1)]$val, c(NA, 1, 1, 1, 2, 2, 3, 3))
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", type=2)]$val, c(1, 1, 2, 2, 2, 3, 3, NA))
  expect_equal(dt[, na_fill_by(dt=.SD, var="val", type=3)]$val, c(1, 1, 1, 1, 2, 2, 3, 3))
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

vars = c("a", "c", "d", "e")

dt0 <- copy(dt)
system.time(na_fill_by(dt0, by="id", inplace=T))
test_that("na_fill_by0", {
  expect_equal(dt0[2, a], 5)
})

dt1 <- copy(dt)
system.time(na_fill_by(dt1, var=vars, by="id", inplace=T))
test_that("na_fill_by1", {
  expect_equal(dt1[2, a], 5)
})

dt2 <- copy(dt)
system.time(dt2b <- na_fill_by(dt2, var=vars, by="id"))
test_that("na_fill_by2", {
  expect_true(is.na(dt2[2, a]))
  expect_equal(dt2b[[1]][2], 5)
})

dt3 <- copy(dt)
system.time(dt3[, (vars) := na_fill_by(.SD, vars, "id")])
test_that("na_fill_by3", {
  expect_equal(dt3[2, a], 5)
  expect_identical(dt1, dt3)
})

dt4 <- copy(dt)
system.time(dt4[, na_fill_by(.SD, vars, "id", inplace=T)])
test_that("na_fill_by4", {
  expect_equal(dt4[2, a], 5)
  expect_identical(dt1, dt4)
})

dt5 <- copy(dt)
dt6 <- copy(dt)
vars2 = c("id", "a", "c", "d", "e")
system.time(dt5[, na_fill_by(.SD, vars2, "b", inplace=T)])
system.time(dt6[, (vars2) := setnafill(.SD, type="locf", cols = vars2), by=b])
test_that("na_fill_by4", {
  expect_identical(dt5$a, dt6$a)
  expect_identical(dt5$c, dt6$c)
  expect_identical(dt5$d, dt6$d)
  expect_identical(dt5$e, dt6$e)
  expect_identical(dt5$id, dt6$id)
})
