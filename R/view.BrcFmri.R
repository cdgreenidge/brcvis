View <- function(center, maxCenter, scale, minScale) {
    structure(list(center=center, maxCenter=maxCenter,
                   scale=scale, minScale=minScale), class="View")
}

isValid <- function(obj) UseMethod("isValid")

isValid.View <- function(obj) {
    stopifnot(
        all(obj$center <= obj$maxCenter),
        obj$scale > 0,
        obj$scale <= 1,
        obj$scale >= obj$minScale)
}

renderer <- function(obj) UseMethod("renderer")

renderer.BrcFmri <- function(mri) {
    arr <- brcbase::data2dTo4d(mri$data2d, mri$parcellation)
    zlims <- c(min(arr), max(arr))

    function(view) {
        slices <- list(
            coronal=arr[ , view$center[2], , view$center[4]],
            sagittal=arr[view$center[1], , , view$center[4]],
            axial=arr[ , , view$center[3], view$center[4]]
        )
        sideLengths <- view$maxCenter * view$scale
        prismMin <- view$center - sideLengths / 2
        prismMax <- view$center + sideLengths / 2

        graphics::par(mfrow=c(2, 2),
                      bg="black",
                      col.axis="white",
                      col.lab="white",
                      col.main="white",
                      col.sub="white",
                      fg="white")

        ## Render coronal slice
        xlims <- c(prismMin[1], prismMax[1])
        ylims <- c(prismMin[3], prismMax[3])
        .drawImage(slices[["coronal"]], xlims, ylims, zlims)
        graphics::title("Coronal")
        .drawCrossHairs(view$center[1], view$center[3])
        .drawDirectionLabels(right="L", top="S", left="R", bottom="I",
                             xlims=xlims, ylims=ylims)

        ## Render sagittal slice
        xlims <- c(prismMin[2], prismMax[2])
        ylims <- c(prismMin[3], prismMax[3])
        .drawImage(slices[["sagittal"]], xlims, ylims, zlims)
        graphics::title("Sagittal")
        .drawCrossHairs(view$center[2], view$center[3])
        .drawDirectionLabels(right="A", top="S", left="P", bottom="I",
                             xlims=xlims, ylims=ylims)

        ## Render axial slice
        xlims <- c(prismMin[1], prismMax[1])
        ylims <- c(prismMin[2], prismMax[2])
        .drawImage(slices[["axial"]], xlims, ylims, zlims)
        graphics::title("Axial")
        .drawCrossHairs(view$center[1], view$center[2])
        .drawDirectionLabels(right="L", top="A", left="R", bottom="P",
                             xlims=xlims, ylims=ylims)
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

    graphics::text(xmax, ymax/2, right, col="white", pos=4, xpd=NA)
    graphics::text(xmax/2, ymax, top, col="white", pos=3, xpd=NA)
    graphics::text(xmin, ymax/2, left, col="white", pos=2, xpd=NA)
    graphics::text(xmax/2, ymin, bottom, col="white", pos=1, xpd=NA)
}
