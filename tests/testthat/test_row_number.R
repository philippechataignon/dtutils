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

dt[, num := row_number_by(dt, "id")]
dt[, row := seq_len(.N), by=id]

test_that("row_number1",
          expect_equal(dt[num != row, .N], 0)
)

dt[first_by(dt, "id"), pos := "F"]
dt[last_by(dt, "id"), pos := "L"]

test_that("first_last", {
  expect_equal(dt[pos=="F", .N], 18000)
  expect_equal(dt[pos=="L", .N], 18000)
})
