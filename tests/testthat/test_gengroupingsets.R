context("gen_groupingsets")
library(data.table)

test_that("gen_groupingsets", {
  expect_equal(
    gen_groupingsets(c("a", "b", "c", "d", "e"), prefix=3),
    list(
      c("a", "b", "c"),
      c("a", "b", "c", "e"),
      c("a", "b", "c", "d"),
      c("a", "b", "c", "d", "e")
    )
  )
  expect_equal(
    gen_groupingsets(c("a", "b", "c", "d", "e"), prefix=2),
    list(
      c("a", "b"),
      c("a", "b", "e"),
      c("a", "b", "d"),
      c("a", "b", "d", "e"),
      c("a", "b", "c"),
      c("a", "b", "c", "e"),
      c("a", "b", "c", "d"),
      c("a", "b", "c", "d", "e")
    )
  )
})
