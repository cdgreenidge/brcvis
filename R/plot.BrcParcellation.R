.parcellationToArray <- function(parcellation) {
  if (class(parcellation) != "BrcParcellation") {
    stop("parcellation argument must be of class BrcParcellation")
  }
  data <- .factorToNumeric(parcellation$partition)
  array(data=data, dim=parcellation$dim3d)
}

.factorToNumeric <- function(xs) {
  if (!is.factor(xs)) {
    stop("xs argument must be a factor")
  }
  as.numeric(levels(xs))[xs]
}
