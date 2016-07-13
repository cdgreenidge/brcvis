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

# Test .parcellationToArray()

dim3d <- c(2, 2, 2)
partition <- factor(c(0, 0, 1, 1, 2, 2, 3, 3))
parcel <- brcbase::BrcParcellation(dim3d=dim3d, partition=partition)

test_that("it should check that the first argument is a BrcParcellation", {
  expect_error(.parcellationToArray(character()), "argument must be of class")
})

test_that("the dims of the array should be the dims of the parcellation", {
  arr <- .parcellationToArray(parcel)
  expect_equal(dim(arr), c(2, 2, 2))
})

test_that("the values in the array correspond to the partition", {
  arr <- .parcellationToArray(parcel)
  expected <- array(c(0, 0, 1, 1, 2, 2, 3, 3), dim=c(2, 2, 2))
  expect_equal(arr, expected)
})
