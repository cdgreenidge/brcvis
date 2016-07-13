context("Test plot.BrcParcellation.R")

# Test .factorToNumeric()

test_that("it should check that the first argument is a factor", {
  expect_error(.factorToNumeric(numeric()), "argument must be a factor")
})

test_that("it should convert the factor to a numeric vector", {
  expect_equal(class(.factorToNumeric(factor(c(1, 2)))), "numeric")
})

test_that("the numbers should map to the factor levels", {
  expect_equal(.factorToNumeric(factor(c(0, 2, 3))), c(0, 2, 3))
})
