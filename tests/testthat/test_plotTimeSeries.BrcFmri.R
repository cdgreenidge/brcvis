context("Test plotTimeSeries.BrcFmri.R")

mat <- matrix(rnorm(8*10), ncol = 8)
dim3d <- c(2, 2, 2)
partition <- factor(1:8)
parcellation <- brcbase:::BrcParcellation(dim3d, partition)
mri <- brcbase:::BrcFmri(data2d=mat, id="01", parcellation=parcellation)
rm(list = c("mat", "dim3d", "partition", "parcellation"))

## Test .convertVoxel3Dto2D()

test_that("it returns the first idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, dim3d) == prod(dim3d))
})

test_that("it returns the middle idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3Dto2D(dim3d, c(3,3,3)) == (1+prod(dim3d))/2)
})

######

## Test .convertVoxel3DGridto2D()

test_that("it returns the first idx correctly",{
  dim3d = c(5,5,5)
  expect_true(.convertVoxel3DGridto2D(dim3d, c(1,1), c(1,1), c(1,1)) == 1)
})

test_that("it returns all the idx correctly",{
  dim3d = c(5,5,5)
  expect_true(all(.convertVoxel3DGridto2D(dim3d, c(1,5), c(1,5), c(1,5))
                  == 1:prod(dim3d)))
})

######

## Test .extractColumnsFromMri

test_that("it returns the first time series", {
  expect_true(all(.extractColumnsFromMri(mri, c(1,1), c(1,1), c(1,1)) ==
                    mri$data2d[,1]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(2,2), c(1,1), c(1,1)) ==
                    mri$data2d[,2]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(2,2), c(2,2), c(2,2)) ==
                    mri$data2d[,8]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(1,1), c(2,2), c(1,1)) ==
                    mri$data2d[,3]))
})