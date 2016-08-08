plotTimeSeries <- function(x, ...) UseMethod("plotTimeSeries")

plotTimeSeries.BrcFmri <- function(x, xlim, ylim, zlim, same.screen = TRUE){
  
}

.extractBoundingBox <- function(x, xlim, ylim, zlim){
  
}

.removeZeroTimeSeries <- function(x){
  
}

.convertVoxel3DGridto2D <- function(dim3d, xlim = NULL, ylim = NULL, zlim = NULL){
  stopifnot(length(xlim) == 2, xlim[1] <= xlim[2])
  stopifnot(length(ylim) == 2, ylim[1] <= ylim[2])
  stopifnot(length(zlim) == 2, zlim[1] <= zlim[2])
  
  xseq <- xlim[1]:xlim[2]; yseq <- ylim[1]:ylim[2]; zseq <- zlim[1]:zlim[2]
  voxel.mat <- expand.grid(xseq, yseq, zseq)
  
  sort(apply(voxel.mat, 1, .convertVoxel3Dto2D, dim3d = dim3d))
}

.convertVoxel3Dto2D <- function(dim3d, vec){
  stopifnot(all(vec > 0))
  stopifnot(all(vec %% 1 == 0))
  stopifnot(all(vec <= dim3d))
  stopifnot(length(vec) == 3, length(dim3d) == 3)
  
  vec[1] + (vec[2]-1)*dim3d[1] + (vec[3]-1)*(dim3d[1]*dim3d[2])
}