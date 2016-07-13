.factorToNumeric <- function(xs) {
  if (!is.factor(xs)) {
    stop("xs argument must be a factor")
  }
  as.numeric(levels(xs))[xs]
}
