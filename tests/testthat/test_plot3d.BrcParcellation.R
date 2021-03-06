context("Test plot3d.BrcParcellation.R")

# Test plot3d.BrcParcellation()
parcel <- brcbase::BrcParcellation(partition=factor(c(1, 1, 1, 1, 0, 0, 0, 0)),
                                   dim3d=c(2, 2, 2))

test_that("it errors if x is an invalid BrcParcellation object", {
  parcel <- brcbase::BrcParcellation(partition=factor(1:8), dim3d=c(2, 3, 2))
  expect_error(p <- plot3d(parcel, numSlices=3), "invalid BrcParcellation object")
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

# Test .arrayToShapes()
test_that("it converts an array to a list of shape values", {
  arr <- array(data=c(0, 1, 1, 0, 1, 0, 0, 1), dim=c(2, 2, 2))
  shape1 <- t(matrix(data=c(2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2), nrow=3,
                     ncol=4))
  shapes <- list(shape1)
  expect_equal(.arrayToShapes(arr), shapes, check.attributes=FALSE)
})

# Test .smoothShapes()
test_that("it returns a list of kernel density estimates", {
  shapes <- .arrayToShapes(.parcellationToArray(parcel))
  kdes <- .smoothShapes(shapes)
  lapply(kdes, function(x) expect_equal(class(x), "kde"))
})

# Test .rglView()

test_that("it returns the correct view matrix for the sagittal view", {
  mat <- matrix(c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), nrow=4,
                ncol=4)
  expect_equal(.rglView("sagittal"), mat)
})

test_that("it returns the correct view matrix for the saggital_reversed view", {
  mat <- matrix(c(0, 0, -1, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), nrow=4,
                ncol=4)
  expect_equal(.rglView("sagittal_reversed"), mat)
})

test_that("it returns the correct view matrix for the coronal view", {
  mat <- matrix(c(1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 1), nrow=4,
                ncol=4)
  expect_equal(.rglView("coronal"), mat)
})

test_that("it returns the correct view matrix for the coronal_reversed view", {
  mat <- matrix(c(-1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1), nrow=4,
                ncol=4)
  expect_equal(.rglView("coronal_reversed"), mat)
})

test_that("it returns the correct view matrix for the axial view", {
  expect_equal(.rglView("axial"), diag(4))
})

test_that("it returns the correct view matrix for the axial_reversed view", {
  mat <- matrix(c(-1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), nrow=4,
                ncol=4)
  expect_equal(.rglView("axial_reversed"), mat)
})

test_that("it errors if the view is not found", {
  expect_error(.rglView("mealybug"), "view argument must be")
})
