context("shift")
library(data.table)

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
system.time(shift_by(dt0, by="id", inplace=T))

dt[, c("la", "lb") := shift_by(.SD, by="id", var=c("a", "b"))]
dt[, c("ma", "mb") := shift(.SD), by="id", .SDcols = c("a", "b")]

test_that("na_fill_by0", {
  expect_equal(dt0[id==18000, head(a)], c(NA, 49L, NA, 6L, NA, 84L))
  expect_identical(dt$la, dt$ma)
  expect_identical(dt$lb, dt$mb)
})

