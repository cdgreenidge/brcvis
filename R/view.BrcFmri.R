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
        obj$scale > obj$minScale)
}
