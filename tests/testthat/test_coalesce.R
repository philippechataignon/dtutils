context("coalesce")
library(data.table)

# pour compatibilité seed
suppressWarnings(RNGversion("3.4"))
ngrp <- 18000
nbygrp <- 50

set.seed(64)
dt <- data.table(
  id=rep(1:ngrp, each=nbygrp) * 2,
  a=sample(1:100, nbygrp * ngrp, replace=T),
  b=sample(LETTERS, nbygrp * ngrp, replace=T),
  c=sample((1:100 + 0.5), nbygrp * ngrp, replace=T),
  d=sample(1:100, nbygrp * ngrp, replace=T),
  e=sample(1:100, nbygrp * ngrp, replace=T)
)
dt[(1:.N %% 2 == 1), a:=NA]
dt[(1:.N %% 3 < 2), b:=NA]
dt[(1:.N %% 4 < 3), c:=NA]
dt[(1:.N %% 5 < 4), d:=NA]
dt[(1:.N %% 6 < 5), e:=NA]


ret1 <- coalesce_by(dt, "id")

test_that("coalesce1", {
  expect_error(coalesce_by(dt, "ids"), "All names in 'by' must be dt colnames")
  expect_equal(ret1[1, id], 2L)
  expect_equal(ret1[1, e], 11L)
})

vars = c("b", "c", "e")
ret2 <- coalesce_by(dt, var=vars, by=c("id", "a"))
test_that("coalesce1", {
  expect_error(coalesce_by(dt, var=c("a", "b", "c", "e"), by=c("id", "a")), "Some variables are in 'by' and in 'var': a")
  expect_equal(ret2[1, c], 96.5)
  expect_true(is.na(ret2[.N, e]))
})