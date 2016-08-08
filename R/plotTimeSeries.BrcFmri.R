plotTimeSeries <- function(x, ...) UseMethod("plotTimeSeries")

plotTimeSeries.BrcFmri <- function(x, xlim, ylim, zlim){
  
}

.extractBoundingBox <- function(x, xlim, ylim, zlim){
  
}

.removeZeroTimeSeries <- function(x){
  
}

.convertVoxel3DGridto2D <- function(dim3d, xlim = NULL, ylim = NULL, zlim = NULL){
  
}

.convertVoxel3Dto2D <- function(dim3d, vec){
  stopifnot(all(vec > 0))
  stopifnot(all(vec %% 1 == 0))
  stopifnot(all(vec <= dim3d))
  stopifnot(length(vec) == 3, length(dim3d) == 3)
  
  vec[1] + (vec[2]-1)*dim3d[1] + (vec[3]-1)*(dim3d[1]*dim3d[2])
}