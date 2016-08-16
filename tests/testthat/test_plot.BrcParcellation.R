context("Test plot.BrcParcellation.R")

# Test plot.BrcParcellation()
parcel <- brcbase::BrcParcellation(partition=c(1:8), dim3d=c(2, 2, 2))

test_that("it errors if x is an invalid BrcParcellation object", {
  parcel <- brcbase::BrcParcellation(partition=c(1:8), dim3d=c(2, 2, 2))
  parcel$dim3d <- c(2,3,2)
  expect_error(p <- plot(parcel, numSlices=3), "invalid BrcParcellation object")
})

test_that("it errors if numSlices is not a positive integer", {
  expect_error(p <- plot(parcel, numSlices=-3), "numSlices argument must be")
  expect_error(p <- plot(parcel, numSlices=1.2), "numSlices argument must be")
})

test_that("it errors if the view is not sagittal, coronal, or axial", {
  expect_error(p <- plot(parcel, numSlices=4, view="nonexistent"),
               "view argument must be one of")
})

test_that("it errors if the colors argument is the wrong length", {
  colors <- c("#100000", "#220000", "#030000", "#400000", "#050000", "#600000")
  expect_error(p <- plot(parcel, numSlices=4, colors=colors),
               "colors argument must contain")
})

test_that("it errors if the colors argument contains invalid colors", {
  colors <- c("all", "your", "base", "are", "belong", "to", "us", "lol")
  expect_error(p <- plot(parcel, numSlices=4, view="sagittal", colors=colors),
               "color argument contains")
})

# Test .isColor()

test_that("it returns TRUE if the string is a valid color, and FALSE if not", {
  actual <- .isColor(c("asdf", "#000000"))
  expected <- c(FALSE, TRUE)
  expect_equal(actual, expected)
})

# test .defaultColors()

test_that("the first color is black", {
  colors <- .defaultColors(5)
  expect_equal(colors[[1]], "#000000FF")
})

test_that("it gives you one color for each parcel plus black", {
  colors <- .defaultColors(5)
  expect_equal(length(colors), 6)
})

# Test .parcellationToArray()

dim3d <- c(2, 2, 2)
partition <- c(0, 0, 1, 1, 2, 2, 3, 3)
parcel <- brcbase::BrcParcellation(dim3d=dim3d, partition=partition)

test_that("the dims of the array should be the dims of the parcellation", {
  arr <- .parcellationToArray(parcel)
  expect_equal(dim(arr), c(2, 2, 2))
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

# test .extractSlices()
arr <- array(c(rep.int(0, 16),
               0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 4, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 5, 6, 0, 0, 7, 8, 0, 0, 0, 0, 0,
               rep.int(0, 16)),
               dim=c(4, 4, 4))

test_that("it should return a list of the same length as the indices list", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=1)
  expect_equal(length(slices), 2)
})

test_that("it should contain elements whose dimensions are one less than arr", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=1)
  dimensionLengths <- lapply(slices, function(x) length(dim(x)))
  expect_true(all(dimensionLengths == length(dim(arr)) - 1))
})

test_that("it contains the slices corresponding to the indices", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=2)
  expect_equal(slices, .splitAlongDim(arr, 2)[2:3])
})

# test .plotLayout()

test_that("the product of nrow and ncol is larger than numSlices", {
  layout <- .plotLayout(10)
  expect_true(layout$nrow * layout$ncol >= 10)
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

