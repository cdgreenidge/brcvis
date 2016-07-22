context("Test view.BrcFmri.R")

### Test View()

test_that("it returns an object of class View", {
    expect_equal(class(View(center=c(1, 2, 1), maxCenter=c(10, 10, 10),
                            scale=1, minScale=0.01, selectedView="coronal")), "View")
})

test_that("it has a center field", {
    view <- View(center=c(1, 2, 1), maxCenter=c(10, 10, 10), scale=1,
                 minScale=0.01, selectedView="coronal")
    expect_equal(view$center, c(1, 2, 1))
})

test_that("it has a maxCenter field", {
    view <- View(center=c(1, 2, 1), maxCenter=c(10, 10, 10), scale=1,
                 minScale=0.01, selectedView="coronal")
    expect_equal(view$maxCenter, c(10, 10, 10))
})

test_that("it has a scale field", {
    view <- View(center=c(1, 2, 1), maxCenter=c(10, 10, 10), scale=0.5,
                 minScale=0.01, selectedView="coronal")
    expect_equal(view$scale, 0.5)
})

test_that("it has a minScale field", {
    view <- View(center=c(1, 2, 1), maxCenter=c(10, 10, 10), scale=0.5,
                 minScale=0.1, selectedView="coronal")
    expect_equal(view$minScale, 0.1)
})

test_that("it has a selectedView filed", {
    view <- View(center=c(1, 2, 1), maxCenter=c(10, 10, 10), scale=0.5,
                 minScale=0.1, selectedView="axial")
    expect_equal(view$selectedView, "axial")
})

### Test isValid.View()

test_that("it checks if the center is within its bounds", {
    view <- View(center=c(8, 11, 8), maxCenter=c(10, 10, 10), scale=1,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view))
})

test_that("it checks if the scale is strictly greater than 0", {
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=-1,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view))
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=0,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view))
})

test_that("it checks if the scale is less than or equal to 1", {
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=2,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view))
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=1,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view), NA)
})

test_that("it checks if the scale is greater than minScale", {
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=0.001,
                 minScale=0.01, selectedView="coronal")
    expect_error(isValid(view))
})

test_that("it checks if selectedView is coronal, sagittal, or axial", {
    view <- View(center=c(1, 2, 3), maxCenter=c(10, 10, 10), scale=1,
                 minScale=0.01, selectedView="foobar")
    expect_error(isValid(view))
})

### Test mriRenderer(mri)

mri <- brcbase::buildBrcFmri(data2d=matrix(1:16, nrow=2, ncol=8),
                             dim3d=c(2, 2, 2))

test_that("it returns a function that renders an MRI", {
    render <- renderer(mri)
    expect_true(is.function(render))
})
