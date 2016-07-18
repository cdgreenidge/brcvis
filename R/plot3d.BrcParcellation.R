plot3d <- function(x, ...) UseMethod("plot3d")

plot3d.BrcParcellation <- function(x, ...) {
  numParcels <- .numParcels(x)

  tryCatch({ brcbase::isValid(x) }, error=function(e) {
    stop(paste("Tried to plot invalid BrcParcellation object: ", e))
  })

  arr <- .parcellationToArray(x)
}

.arrayToShapes <- function(arr) {
  parcels <- unique(c(arr))
  shapes <- lapply(parcels, function(x) {
    which(arr == x, arr.ind=T)
  })
  unname(shapes)
}
