context("Test plot3d.BrcParcellation.R")

# Test plot3d.BrcParcellation()
parcel <- brcbase::BrcParcellation(partition=factor(1:8), dim3d=c(2, 2, 2))

test_that("it errors if x is an invalid BrcParcellation object", {
  parcel <- brcbase::BrcParcellation(partition=factor(1:8), dim3d=c(2, 3, 2))
  expect_error(p <- plot3d(parcel, numSlices=3), "invalid BrcParcellation object")
})

# Test .arrayToShapes()
test_that("it converts an array to a list of shape values", {
  arr <- array(data=c(0, 1, 1, 0, 1, 0, 0, 1), dim=c(2, 2, 2))
  shape0 <- t(matrix(data=c(1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 2), nrow=3,
                     ncol=4))
  shape1 <- t(matrix(data=c(2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2), nrow=3,
                     ncol=4))
  shapes <- list(shape0, shape1)
  expect_equal(.arrayToShapes(arr), shapes, check.attributes=FALSE)
})

# Test .smoothShapes()
test_that("it returns a list of kernel density estimates", {
  shapes <- .arrayToShapes(.parcellationToArray(parcel))
  kdes <- .smoothShapes(shapes)
  lapply(kdes, function(x) expect_equal(class(x), "kde"))
})
