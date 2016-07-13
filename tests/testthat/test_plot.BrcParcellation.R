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

# test .removeZeroSlices()

arr <- array(c(rep.int(0, 16),
               0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 4, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 5, 6, 0, 0, 7, 8, 0, 0, 0, 0, 0,
               rep.int(0, 16)),
               dim=c(4, 4, 4))

test_that("it should remove slices that are all zeroes across a dimension", {
  cropped <- .removeZeroSlices(arr, dim=1)
  expected <- arr[c(2, 3), , ]
  expect_equal(cropped, expected)
})

test_that("we should be able to choose the dimension arbitrarily", {
  cropped <- .removeZeroSlices(arr, dim=2)
  expected <- arr[ , c(2, 3), ]
  expect_equal(cropped, expected)
})

# test .makeIndexSequence()

test_that("it should return a sequence of the user-specified length", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_equal(length(seq), 13)
})

test_that("it should return a vector of integers", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_equal(seq, as.integer(seq))
})

test_that("all of the values should be positive", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_true(all(seq > 0))
})
