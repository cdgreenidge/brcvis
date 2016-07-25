view <- function(obj) UseMethod("view")

view.BrcFmri <- function(obj) {
    ## Ugly hack because R doesn't support event handling for cairo
    os <- Sys.info()["sysname"]
    if (os == "Linux" || os == "Darwin") {
        grDevices::X11(type="Xlib")
    }

    view <- defaultView(obj)
    render <- renderer(obj)
    render(view)

    onKeybd <- function(key) {
        ## We can't use a switch statment here because of special characters
        f <- identity

        if (key == "a" || key == "A" ) {
            f <- selectAxial
        } else if (key == "b" || key == "B") {
            f <- stepBackward
        } else if (key == "c" || key == "C") {
            f <- selectCoronal
        } else if (key == "f" || key == "F") {
            f <- stepForward
        } else if (key == "s" || key == "S") {
            f <- selectSagittal
        } else if (key == "=" || key == "+") {
            f <- zoomIn
        } else if (key == "-" || key == "_") {
            f <- zoomOut
        }

        print(key)
        view <<- f(view)
        render(view)
        NULL
    }

    grDevices::setGraphicsEventHandlers(onKeybd=onKeybd)
    grDevices::getGraphicsEvent()
}

View <- function(center, maxCenter, scale, minScale, selectedView) {
    structure(list(center=center, maxCenter=maxCenter,
                   scale=scale, minScale=minScale, selectedView=selectedView),
              class="View")
}

isValid <- function(obj) UseMethod("isValid")

isValid.View <- function(obj) {
    stopifnot(
        all(obj$center <= obj$maxCenter),
        obj$scale > 0,
        obj$scale <= 1,
        obj$scale >= obj$minScale,
        all(obj$selectedView %in% c("coronal", "sagittal", "axial"))
    )
}

renderer <- function(obj) UseMethod("renderer")

renderer.BrcFmri <- function(mri) {
    arr <- brcbase::data2dTo4d(mri$data2d, mri$parcellation)
    zlims <- c(min(arr), max(arr))

    function(view) {
        grDevices::dev.hold()

        slices <- list(
            coronal=arr[ , view$center[2], , view$center[4]],
            sagittal=arr[view$center[1], , , view$center[4]],
            axial=arr[ , , view$center[3], view$center[4]]
        )
        sideLengths <- (view$maxCenter[1:3] - 1) * view$scale
        prismMin <- view$center[1:3] - (sideLengths / 2)
        prismMin[4] <- view$center[4]
        prismMax <- view$center[1:3] + (sideLengths / 2)
        prismMax[4] <- view$center[4]

        ## If prism is outside bounds, move it back
        minOffset <- pmax(1 - prismMin, 0)
        maxOffset <- pmin(view$maxCenter - prismMax, 0)

        prismMin <- prismMin + maxOffset + minOffset
        prismMax <- prismMax + maxOffset + minOffset

        graphics::par(mfrow=c(2, 2),
                      bg="black",
                      col.axis="white",
                      col.lab="white",
                      col.main="white",
                      col.sub="white",
                      fg="white")
        titleColors <- list(coronal="white", sagittal="white", axial="white")
        titleColors[[view$selectedView]] <- "red"

        ## Render coronal slice
        xlims <- c(prismMin[1], prismMax[1])
        ylims <- c(prismMin[3], prismMax[3])
        .drawImage(slices[["coronal"]], xlims, ylims, zlims)
        graphics::title("Coronal", col.main=titleColors$coronal)
        .drawCrossHairs(view$center[1], view$center[3])
        .drawDirectionLabels(right="L", top="S", left="R", bottom="I",
                             xlims=xlims, ylims=ylims)

        ## Render sagittal slice
        xlims <- c(prismMin[2], prismMax[2])
        ylims <- c(prismMin[3], prismMax[3])
        .drawImage(slices[["sagittal"]], xlims, ylims, zlims)
        graphics::title("Sagittal", col.main=titleColors$sagittal)
        .drawCrossHairs(view$center[3], view$center[2])
        .drawDirectionLabels(right="A", top="S", left="P", bottom="I",
                             xlims=xlims, ylims=ylims)

        ## Render axial slice
        xlims <- c(prismMin[1], prismMax[1])
        ylims <- c(prismMin[2], prismMax[2])
        .drawImage(slices[["axial"]], xlims, ylims, zlims)
        graphics::title("Axial", col.main=titleColors$axial)
        .drawCrossHairs(view$center[2], view$center[1])
        .drawDirectionLabels(right="L", top="A", left="R", bottom="P",
                             xlims=xlims, ylims=ylims)

        invisible(grDevices::dev.flush())
    }
}

.drawImage <- function(img, xlims, ylims, zlims) {
    graphics::image(x=1:dim(img)[1],
                    bty="n",
                    col=grDevices::grey(seq(0, 1, length=256)),
                    xaxt="n",
                    xlab="",
                    xlim=xlims,
                    y=1:dim(img)[2],
                    yaxt="n",
                    ylab="",
                    ylim=ylims,
                    z=img,
                    zlim=zlims)
}

.drawCrossHairs <- function(x, y) {
    graphics::abline(h=x, col="red")
    graphics::abline(v=y, col="red")
}

.drawDirectionLabels <- function(right, top, left, bottom, xlims, ylims) {
    xmin <- xlims[1]
    xmax <- xlims[2]
    ymin <- ylims[1]
    ymax <- ylims[2]

    graphics::text(xmax, ymin + (ymax - ymin)/2, right, col="white",
                   pos=4, xpd=NA)
    graphics::text(xmin + (xmax - xmin)/2, ymax, top, col="white",
                   pos=3, xpd=NA)
    graphics::text(xmin, ymin + (ymax - ymin)/2, left, col="white",
                   pos=2, xpd=NA)
    graphics::text(xmin + (xmax - xmin)/2, ymin, bottom, col="white",
                   pos=1, xpd=NA)
}
