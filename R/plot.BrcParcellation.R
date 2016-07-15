plot.BrcParcellation <- function(x, y, view="saggital", numSlices,
                                 colors=NULL, ...) {
  views <- list(saggital=1, coronal=2, axial=3)
  dimension <- views[[view]]

  arr <- .parcellationToArray(x)
  arr <- .removeZeroSlices(arr, dimension)
  indices <- .makeIndexSequence(max=dim(arr)[dimension], length=numSlices)
  slices <- .extractSlices(arr, indices, dimension)
  numParcels <- length(levels(x$partition))
  invisible(.plotSlices(slices, numParcels, colors))
}

.plotSlices <- function(slices, numParcels, colors) {
  if (is.null(colors)) {
    colors <- .defaultColors(numParcels)
  }
  layout <- .plotLayout(numSlices=length(slices))

  maxParcel <- max(unlist(slices))

  par(mfrow=c(layout$nrow, layout$ncol), mar=rep(0.2, 4), bg="black")
  for (i in 1:length(slices)) {
    image(slices[[i]],
          asp=ncol(slices[[i]]) / nrow(slices[[i]]),
          breaks=(0:(maxParcel + 1)) - 0.5,
          bty="n",
          col=colors,
          xaxt="n",
          yaxt="n")
  }
}

.defaultColors <- function(numParcels) {
  c("#000000FF", rainbow(numParcels - 1))
}

.plotLayout <- function(numSlices) {
	nrow = ceiling(sqrt(numSlices / 2))
	ncol = ceiling(numSlices / nrow)
	list(nrow=nrow, ncol=ncol)
}

.extractSlices <- function(arr, indices, dim) {
  .splitAlongDim(arr, dim)[indices]
}

.makeIndexSequence <- function(max, length) {
  round(seq(1, max, length.out=length))
}

.removeZeroSlices <- function(arr, dim) {
  nonzero <- apply(arr, dim, function(xs) any(xs != 0))

  # R is really really bad at handling high-dimensional arrays. In particular,
  # there is no way to subset along a dimension if we only know the dimension
  # at runtime, because the syntax varies by dimension: arr[nonzero, , ] for
  # the first dimension, arr[ , nonzero, ] for the second dimension, etc.
  # So, we have to construct the function call by hand. How lovely!

  args <- rep(list(bquote()), times=length(dim(arr)))
  args[[dim]] <- quote(nonzero)
  call <- as.call(c(as.name("["), quote(arr), args))

  # No, I'm not bitter

  eval(call)
}

# This function borrowed from
# http://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
# Thanks, internet!
.splitAlongDim <- function(arr, dim) {
  setNames(lapply(split(arr, arrayInd(seq_along(arr), dim(arr))[, dim]),
                  array, dim=dim(arr)[-dim], dimnames(arr)[-dim]),
           dimnames(arr)[[dim]])
}

.parcellationToArray <- function(parcellation) {
  data <- .factorToNumeric(parcellation$partition)
  array(data=data, dim=parcellation$dim3d)
}

.factorToNumeric <- function(xs) {
  as.numeric(levels(xs))[xs]
}
