context("Test plotTimeSeries.BrcFmri.R")

## Test .convertVoxel3Dto2D()

test_that("it returns the first idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, dim3d) == prod(dim3d))
})

######

## Test .convertVoxel3DGridto2D()

test_that("it returns the first idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3DGridto2D(dim3d, c(1,1), c(1,1), c(1,1)) == 1)
})