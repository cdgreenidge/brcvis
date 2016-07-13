context("Test plot.BrcParcellation.R")

# Test .factorToNumeric()

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

test_that("the dims of the array should be the dims of the parcellation", {
  arr <- .parcellationToArray(parcel)
  expect_equal(dim(arr), c(2, 2, 2))
})

test_that("the values in the array correspond to the partition", {
  arr <- .parcellationToArray(parcel)
  expected <- array(c(0, 0, 1, 1, 2, 2, 3, 3), dim=c(2, 2, 2))
  expect_equal(arr, expected)
})

# test splitAlongDim()

arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim=c(2, 2, 2))

test_that("it splits an array along a dimension", {
  expected <- list(array(c(1, 2, 3, 4), dim=c(2, 2)),
                   array(c(5, 6, 7, 8), dim=c(2, 2)))
  expect_equal(.splitAlongDim(arr, 3), expected)
})

test_that("you can choose the dimension along which to split", {
  expected <- list(array(c(1, 2, 5, 6), dim=c(2, 2)),
                   array(c(3, 4, 7, 8), dim=c(2, 2)))
  expect_equal(.splitAlongDim(arr, 2), expected)
})
