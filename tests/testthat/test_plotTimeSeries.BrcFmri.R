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

test_that("it returns the correct single time series", {
  expect_true(all(.extractColumnsFromMri(mri, c(1,1), c(1,1), c(1,1)) ==
                    mri$data2d[,1]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(2,2), c(1,1), c(1,1)) ==
                    mri$data2d[,2]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(2,2), c(2,2), c(2,2)) ==
                    mri$data2d[,8]))
  
  expect_true(all(.extractColumnsFromMri(mri, c(1,1), c(2,2), c(1,1)) ==
                    mri$data2d[,3]))
})

test_that("it returns all the time series", {
  mat <- .extractColumnsFromMri(mri, c(1,2), c(1,2), c(1,2)) 
  expect_true(ncol(mat) == 8)
  expect_true(nrow(mat) == 10)
})


#######

## .removeZeroTimeSeries()

test_that("it removes zero columns successfully", {
  mat <- cbind(matrix(1, 5, 5), 0)
  expect_true(all(.removeZeroTimeSeries(mat)[,1] == 1))
})

test_that("errors if all 0", {
  expect_error(.removeZeroTimeSeries(matrix(0,5,5)))
})

test_that("does not remove columns that are centered", {
  mat <- matrix(rnorm(20),ncol = 4, nrow = 5)
  mat <- scale(mat)
  mat <- cbind(mat, 0)
  expect_true(ncol(.removeZeroTimeSeries(mat)) == 4)
})

test_that("does not do anything if matrix does not have 0-columns", {
  mat <- matrix(rnorm(20),ncol = 4, nrow = 5)
  expect_true(ncol(.removeZeroTimeSeries(mat)) == 4)
})

test_that("it does not drop columns when there is only one column", {
  mat <- cbind(rep(1,5), rep(0,5))
  mat2 <- .removeZeroTimeSeries(mat)
  expect_true(is.matrix(mat2))
  expect_true(all(mat2 == 1))
})

######

dim3d <- c(3,4,5)
mat <- matrix(rnorm(prod(dim3d)*3), ncol = prod(dim3d))
partition <- factor(1:prod(dim3d))
parcellation <- brcbase:::BrcParcellation(dim3d, partition)
mri5 <- brcbase:::BrcFmri(data2d=mat, id="01", parcellation=parcellation)
rm(list = c("mat", "dim3d", "partition", "parcellation"))

## .checkTimeSeriesLim()

test_that("errors for negative values flagged", {
  expect_error(.checkTimeSeriesLim(mri5, c(0,2), 1))
  expect_error(.checkTimeSeriesLim(mri5, c(-5,2), 1))
  expect_error(.checkTimeSeriesLim(mri5, c(2,-5), 1))
  expect_error(.checkTimeSeriesLim(mri5, c(-5,-5), 1))
})

test_that("errors for positive values exceeding mri flagged", {
  expect_error(.checkTimeSeriesLim(mri5, c(1,10), 2))
  expect_error(.checkTimeSeriesLim(mri5, c(10,3), 2))
  expect_error(.checkTimeSeriesLim(mri5, c(10,15), 2))
})

test_that("errors for decimals/numeric flagged", {
  expect_error(.checkTimeSeriesLim(mri5, c(2.3,4), 2))
  expect_error(.checkTimeSeriesLim(mri5, c("1","3"), 2))
})

test_that("errors for decreasing vector flagged", {
  expect_error(.checkTimeSeriesLim(mri5, c(4,2), 2))
})

test_that("no arguments is properly filled in", {
  expect_true(all(.checkTimeSeriesLim(mri5, 3, 2) == c(3,3)))
  expect_true(all(.checkTimeSeriesLim(mri5, NULL, 1) == c(1,3)))
  expect_true(all(.checkTimeSeriesLim(mri5, NULL, 2) == c(1,4)))
  expect_true(all(.checkTimeSeriesLim(mri5, NULL, 3) == c(1,5)))
})

###################

## plotTimeSeries.BrcFmri()

test_that("error if not BrcFmri class", {
  expect_error(plotTimeSeries.BrcFmri(x$parcellation))
  expect_error(plotTimeSeries.BrcFmri(matrix(1,5,5)))
})