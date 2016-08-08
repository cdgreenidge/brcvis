context("Test plotTimeSeries.BrcFmri.R")

## Test .convertVoxel3Dto2D()

test_that("it returns the first value correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last value correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, dim3d) == prod(dim3d))
})