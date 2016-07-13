.parcellationToArray <- function(parcellation) {
  data <- .factorToNumeric(parcellation$partition)
  array(data=data, dim=parcellation$dim3d)
}

.factorToNumeric <- function(xs) {
  as.numeric(levels(xs))[xs]
}
