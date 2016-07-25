context("Test controller.BrcFmri.R")

## Test makeKeyboardHandler

test_that("makeKeyboardHandler returns a function", {
    handler <- makeKeyboardHandler(mri)
    expect_true(is.function(handler))
})

## Test defaultView(mri)

mri <- brcbase::buildBrcFmri(data2d=matrix(1:320, nrow=4, ncol=80),
                             dim3d=c(4, 5, 4))

test_that("it sets the center to the center of the MRI", {
    view <- defaultView(mri)
    expect_equal(view$center, c(2, 2, 2, 1))
})

test_that("it sets the max center to the max dimensions of the MRI", {
    view <- defaultView(mri)
    expect_equal(view$maxCenter, c(4, 5, 4, 4))
})

test_that("it sets the scale to 1", {
    view <- defaultView(mri)
    expect_equal(view$scale, 1)
})

test_that("it sets the minimum scale to one voxel at the highest zoom", {
    view <- defaultView(mri)
    expect_equal(view$minScale, 1/4)
})

test_that("the default selected view is coronal", {
    view <- defaultView(mri)
    expect_equal(view$selectedView, "coronal")
})
