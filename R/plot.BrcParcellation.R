# This function stolen from
# http://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
.splitAlongDim <- function(arr, d) {
  setNames(lapply(split(arr, arrayInd(seq_along(arr), dim(arr))[, d]),
                  array, dim=dim(arr)[-d], dimnames(arr)[-d]),
           dimnames(arr)[[d]])
}

.parcellationToArray <- function(parcellation) {
  data <- .factorToNumeric(parcellation$partition)
  array(data=data, dim=parcellation$dim3d)
}

.factorToNumeric <- function(xs) {
  as.numeric(levels(xs))[xs]
}
