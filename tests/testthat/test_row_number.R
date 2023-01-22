context("row_number")
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

dt[first_by(dt, "id"), pos := "F"]

test_that("first_last", {
  expect_equal(dt[pos=="F", .N], 18000)
})
