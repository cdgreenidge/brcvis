makeKeyboardHandler <- function(mri) {
    ## view <- defaultView(mri)
    function(key) {

    }
}

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
