context("case_when_mult_mult")
library(data.table)

test_that("normal use", {
  DT <- data.table(
    a=1:10,
    b=LETTERS[1:10]
  )
  DT[, c("w", "x") :=
    case_when_mult(
      a %% 4 == 0 ~ list("p", 1.5),
      a %% 2 == 0 ~ list("m", 4) ,
      T ~ list("i", 6)
    )
  ]
  expect_equal(DT[1:4, w], c("i", "m", "i", "p"))
  expect_equal(DT[1:4, x], c(6, 4, 6, 1.5))
})

test_that("NA catched by TRUE", {
  DT <- data.table(
    a=c(1:5, NA, 7:10),
    b=LETTERS[1:10]
  )
  DT[, c("w", "x") :=
    case_when_mult(
      a %% 4 == 0 ~ list("p", 1.5),
      a %% 2 == 0 ~ list("m", 4) ,
      T ~ list("i", 6)
    )
  ]
  expect_equal(DT[6, w], "i")
  expect_equal(DT[6, x], 6)
})

test_that("NA explicit catch and default value", {
  DT <- data.table(
    a=c(1:5, NA, 7:10),
    b=LETTERS[1:10]
  )
  DT[, c("w", "x") :=
    case_when_mult(
      a %% 4 == 0 ~ list("p", 1.5),
      a %% 2 == 0 ~ list("m", 4) ,
      is.na(a) ~ list("z", 9)
    )
  ]
  expect_equal(DT[6, w], "z")
  expect_equal(DT[6, x], 9)
  expect_true(is.na(DT[5, w]))
  expect_true(is.na(DT[5, x]))
})

test_that("DT with filter rows", {
  DT <- data.table(
    a=c(1:5, NA, 7:10),
    b=LETTERS[1:10]
  )
  DT[
    b %in% c("A", "B", "C", "D", "E", "F"),
    c("w", "x") :=
      case_when_mult(
        a %% 4 == 0 ~ list("p", 1.5),
        a %% 2 == 0 ~ list("m", 4) ,
        is.na(a) ~ list("z", 9)
      )
  ]
  expect_equal(DT[4, w], "p")
  expect_equal(DT[4, x], 1.5)
  expect_equal(DT[6, w], "z")
  expect_equal(DT[6, x], 9)
  expect_true(is.na(DT[5, w]))
  expect_true(is.na(DT[5, x]))
  expect_true(is.na(DT[8, w]))
  expect_true(is.na(DT[8, x]))
})

test_that("zero inputs throws an error", {
  expect_error(
    case_when_mult(),
    "No cases provided",
    fixed = TRUE
  )
})

test_that("error messages", {
  expect_error(
    case_when_mult(
      paste(50)
    ),
    "Case 1 must be a two-sided formula",
    fixed = TRUE
  )

  expect_error(
    case_when_mult(
      50 ~ 1:3
    ),
    "LHS of case 1 must be a logical, not double",
    fixed = TRUE
  )
})

test_that("cases must yield compatible lengths", {
  expect_error(
    case_when_mult(
      c(TRUE, FALSE) ~ 1,
      c(FALSE, TRUE, FALSE) ~ 2
    ),
    "All lengths of LHS must be the same or 1",
    fixed = TRUE
  )

  expect_error(
    case_when_mult(
      c(TRUE, FALSE) ~ 1:3,
      c(FALSE, TRUE) ~ 1:2
    ),
    "All lengths of RHS must be the same",
    fixed = TRUE
  )

  DT <- data.table(a = 1:1000, b=rnorm(1000))
  expect_error(
    DT[, case_when_mult(
        a %% 4 == 0 ~ b,
        a %% 2 == 0 ~ a
    )],
    "Too much values in RHS: 1000. Did you use a variable in RHS ?"
  )
})

test_that("matches values in order", {
  x <- 1:3
  expect_equal(
    case_when_mult(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      x <= 3 ~ 3
    ),
    c(1, 2, 3)
  )
})

test_that("unmatched gets missing value", {
  x <- 1:3
  expect_equal(
    case_when_mult(
      x <= 1 ~ 1,
      x <= 2 ~ 2
    ),
    c(1, 2, NA)
  )
})

test_that("missing values can be replaced", {
  x <- c(1:3, NA)
  expect_equal(
    case_when_mult(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      is.na(x) ~ 0
    ),
    c(1, 2, NA, 0)
  )
})
