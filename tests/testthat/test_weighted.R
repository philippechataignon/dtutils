context("weighted")
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
dt[899990, e := NA]
dt = dt[order(alea)]
dt[1, d := NA]

s1=dt[, .(d=sum(d*c, na.rm=T), e=sum(e*c, na.rm=T)), by=id][order(id)]
s2=weightedsum(dt[, .(id, d, e)], dt$c, "id", na.rm=T)[order(id)]

test_that("weightedsum1", {
  expect_equal(s1$d, s2$d)
  expect_equal(s1$e, s2$e)
})

s1=dt[, .(d=sum(d*c, na.rm=F), e=sum(e*c, na.rm=F)), by=id][order(id)]
s2=weightedsum(dt[, .(id, d, e)], dt$c, "id", na.rm=F)[order(id)]
test_that("weightedsum2", {
  expect_equal(s1$d[2:nrow(s1)], s2$d[2:nrow(s2)])
  expect_equal(s1$e[2:nrow(s1)], s2$e[2:nrow(s2)])
  expect_equal(s2$e[[1]], 419.556)
})

