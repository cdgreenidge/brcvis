plotTimeSeries <- function(x, ...) UseMethod("plotTimeSeries")

plotTimeSeries.BrcFmri <- function(x, xlim = NULL, ylim = NULL, zlim = NULL, 
                                   same.screen = TRUE){
  xlim <- .checkTimeSeriesLim(x, xlim, 1)
  ylim <- .checkTimeSeriesLim(x, ylim, 1)
  zlim <- .checkTimeSeriesLim(x, zlim, 1)
  
  mat <- .extractColumnsFromMri(x, xlim, ylim, zlim)
  mat <- .removeZeroSlices(mat)
  if(same.screen) mat <- scale(mat)
  
  mat
}

.checkTimeSeriesLim <- function(mri, vec, dim.idx){
  dim3d.val <- mri$parcellation$dim3d[dim.idx]
  
  if(length(vec) == 1) vec <- (rep(vec, 2))
  if(is.null(vec)) vec <- c(1,dim3d.val)
  
  if(!all(vec > 0)) stop("xlim, ylim and zlim must be positive")
  if(!all(vec <= dim3d.val)) stop("xlim, ylim and zlim must be less than dimension of x")
  if(!all(vec %% 1 == 0)) stop("xlim, ylim and zlim must be integers")
  if(vec[1] > vec[2]) stop("xlim, ylim and zlim must each be in ascending order")
  
  vec
}

.extractColumnsFromMri <- function(mri, xlim, ylim, zlim){
  stopifnot(class(mri) == "BrcFmri")
  
  idx.3d <- .convertVoxel3DGridto2D(mri$parcellation$dim3d, xlim, ylim, zlim)
  idx.col <- unique(as.numeric(mri$parcellation$partition[idx.3d]))
  mri$data2d[,idx.col]
}

.removeZeroTimeSeries <- function(mat){
  stopifnot(is.matrix(mat))
  
  idx <- which(apply(mat, 2, function(x){abs(sum(x))}) != 0)
  mat[,idx,drop = FALSE]
}

.convertVoxel3DGridto2D <- function(dim3d, xlim = NULL, ylim = NULL, zlim = NULL){
  stopifnot(length(xlim) == 2, xlim[1] <= xlim[2])
  stopifnot(length(ylim) == 2, ylim[1] <= ylim[2])
  stopifnot(length(zlim) == 2, zlim[1] <= zlim[2])
  
  xseq <- xlim[1]:xlim[2]; yseq <- ylim[1]:ylim[2]; zseq <- zlim[1]:zlim[2]
  voxel.mat <- expand.grid(xseq, yseq, zseq)
  
  unique(sort(apply(voxel.mat, 1, .convertVoxel3Dto2D, dim3d = dim3d)))
}

.convertVoxel3Dto2D <- function(dim3d, vec){
  stopifnot(all(vec > 0))
  stopifnot(all(vec %% 1 == 0))
  stopifnot(all(vec <= dim3d))
  stopifnot(length(vec) == 3, length(dim3d) == 3)
  
  vec[1] + (vec[2]-1)*dim3d[1] + (vec[3]-1)*(dim3d[1]*dim3d[2])
}