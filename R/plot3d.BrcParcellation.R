plot3d <- function(x, ...) UseMethod("plot3d")

plot3d.BrcParcellation <- function(x, colors=NULL, ...) {
  numParcels <- .numParcels(x)

  tryCatch({ brcbase::isValid(x) }, error=function(e) {
    stop(paste("Tried to plot invalid BrcParcellation object: ", e))
  })

  if (is.null(colors)) {
    colors <- .defaultColors(numParcels)
  } else if (!all(.isColor(colors))) {
    stop("color argument contains invalid colors")
  } else if (.numParcels(x) != (length(colors) - 1)) {
    stop(paste("colors argument must contain 1 more color than the number ",
               "of parcels in the parcellation"))
  }

  arr <- .parcellationToArray(x)
  shapes <- .arrayToShapes(arr)
  kdes <- .smoothShapes(shapes)
}

.arrayToShapes <- function(arr) {
  parcels <- unique(c(arr))
  shapes <- lapply(parcels, function(x) {
    which(arr == x, arr.ind=T)
  })
}

.smoothShapes <- function(shapes) {
  lapply(shapes, function(x) {
    bandwidth <- matrix(1, ncol=3, nrow=3)
    diag(bandwidth) <- 3
    ks::kde(x, H=bandwidth, compute.cont=TRUE)
  })
}
