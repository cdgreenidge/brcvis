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

.parcellationToArray <- function(parcellation) {
  data <- .factorToNumeric(parcellation$partition)
  array(data=data, dim=parcellation$dim3d)
}

.factorToNumeric <- function(xs) {
  as.numeric(levels(xs))[xs]
}
