context("Test controller.BrcFmri.R")

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

test_that("the default seleced view is coronal", {
    view <- defaultView(mri)
    expect_equal(view$selectedView, "coronal")
})

## Test zoomIn(view)

test_that("it returns a view with a decreased scale", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.3,
                 minScale=0.1, selectedView="coronal")
    zoomed <- zoomIn(view)
    expect_true(zoomed$scale < view$scale)
})

test_that("it does not zoom in beyond the minimum bound", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.10001,
                 minScale=0.1, selectedView="coronal")
    zoomed <- zoomIn(view)
    expect_equal(zoomed$scale, 0.1)
})

## Test zoomOut(view)

test_that("it returns a view with a increased scale", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.3,
                 minScale=0.1, selectedView="coronal")
    zoomed <- zoomOut(view)
    expect_true(zoomed$scale > view$scale)
})

test_that("it does not zoom out beyond the maximum bound", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    zoomed <- zoomOut(view)
    expect_equal(zoomed$scale, 1)
})

## Test stepForward(view)

test_that("it returns a view that goes forward one time step", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    theFuture <- stepForward(view)
    expect_true(theFuture$center[4] > view$center[4])
})

test_that("it does not step beyond the maximum time step", {
    view <- View(c(10, 10, 10, 20), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    theFuture <- stepForward(view)
    expect_true(theFuture$center[4] == 20)
})

## Test stepForward(view)

test_that("it returns a view that goes backward one time step", {
    view <- View(c(10, 10, 10, 15), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    theFuture <- stepBackward(view)
    expect_true(theFuture$center[4] < view$center[4])
})

test_that("it does not step beyond the first time step", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    theFuture <- stepBackward(view)
    expect_true(theFuture$center[4] == 1)
})

## Test selectCoronal(view)

test_that("it returns a view with coronal selected", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="axial")
    view <- selectCoronal(view)
    expect_equal(view$selectedView, "coronal")
})

## Test selectAxial(view)

test_that("it returns a view with axial selected", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    view <- selectAxial(view)
    expect_equal(view$selectedView, "axial")
})

## Test selectSagittal(view)

test_that("it returns a view with sagittal selected", {
    view <- View(c(10, 10, 10, 1), maxCenter=c(20, 20, 20, 20), scale=0.9999,
                 minScale=0.1, selectedView="coronal")
    view <- selectSagittal(view)
    expect_equal(view$selectedView, "sagittal")
})
