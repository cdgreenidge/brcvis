defaultView <- function(mri) {
    defaultCenter <- floor(brcbase::dim4d(mri) / 2)
    defaultCenter[4] <- 1

    View(
        center=defaultCenter,
        maxCenter=brcbase::dim4d(mri),
        scale=1,
        minScale=1 / min(brcbase::dim4d(mri)[-4]),
        selectedView="coronal"
    )
}

selectCoronal <- function(view) {
    view$selectedView <- "coronal"
    view
}

selectAxial <- function(view) {
    view$selectedView <- "axial"
    view
}

selectSagittal <- function(view) {
    view$selectedView <- "sagittal"
    view
}

stepForward <- function(view) {
    view$center[4] <- min(view$center[4] + 1, view$maxCenter[4])
    view
}

stepBackward <- function(view) {
    view$center[4] <- max(view$center[4] - 1, 1)
    view
}

zoomIn <- function(view) {
    numSteps <- 10
    step <- (1 - view$minScale) / numSteps
    view$scale <- max(view$scale - step, view$minScale)
    view
}

zoomOut <- function(view) {
    numSteps <- 10
    step <- (1 - view$minScale) / numSteps
    view$scale <- min(view$scale + step, 1)
    view
}
