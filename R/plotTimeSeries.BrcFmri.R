#' Generic Plotting for Time Series
#'
#' @param x the S3 object
#' @param ... Arguments to be passed to methods, such as graphical parameters.
#' @export
plotTimeSeries <- function(x, ...) {UseMethod("plotTimeSeries")}

#' Plotting Time Series for a BrcFmri Object
#' 
#' Creates plots of the time series at various voxels inside the BrcFmri object.
#'
#' @param x the BrcFmri object
#' @param buffer a numeric determine the spacing between two time series in the plot. 
#' Only used if same.screen = TRUE.
#' @param same.screen a boolean to determine if all the time series are put on one plot
#' or seperate plots
#' @param xregion the direction along the first dimension of the BrcFmri data in 3-dimensions
#' to dictate which voxels have their time series shown
#' @param yregion the direction along the second dimension of the BrcFmri data in 3-dimensions
#' to dictate which voxels have their time series shown
#' @param zregion the direction along the third dimension of the BrcFmri data in 3-dimensions
#' to dictate which voxels have their time series shown
#' @param ... Arguments to be passed to plotting, such as graphical parameters.
#'
#' @export
plotTimeSeries.BrcFmri <- function(x, buffer = NULL, same.screen = TRUE, 
                                   xregion = NULL, yregion = NULL, zregion = NULL, ...){
  if(class(x) != "BrcFmri") stop("x must be class BrcFmri")
  
  xregion <- .checkTimeSeriesLim(x, xregion, 1)
  yregion <- .checkTimeSeriesLim(x, yregion, 1)
  zregion <- .checkTimeSeriesLim(x, zregion, 1)
  
  if(is.null(buffer)) buffer <- stats::quantile(abs(x$data2d), 0.25)
  if(!is.numeric(buffer) | length(buffer) != 1) stop("buffer must be a single numeric")
  
  mat <- .extractColumnsFromMri(x, xregion, yregion, zregion)
  mat <- .removeZeroTimeSeries(mat)
  if(same.screen) {
    .plotTimeSeriesAll(mat, buffer, ...)
  } else {
    .plotTimeSeriesIndividual(mat, ...)
  }
  
  invisible()
}

.plotTimeSeriesIndividual <- function(mat, ...){
  layout <- .plotLayout(ncol(mat))
  graphics::par(mfrow=c(layout$nrow, layout$ncol))
  for(i in 1:ncol(mat)) graphics::plot(mat[,i], ...)
  
  invisible()
}

.plotTimeSeriesAll <- function(mat, buffer, ...){
  mat <- scale(mat)
  mat <- .spaceOutColumns(mat)
  mat <- .shrinkColumnsTogether(mat, buffer)
  
  graphics::plot(NA, xlim = c(1,nrow(mat)), ylim = c(min(mat), max(mat)))
  for(i in 1:ncol(mat)) graphics::lines(mat[,i], ...)
  
  invisible()
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

.spaceOutColumns <- function(mat){
  d <- ncol(mat)
  if(d == 1) return(mat)
  
  for(i in 2:d){
    mat[,i] = max(abs(mat[,i-1])) + max(abs(mat[,i] - mat[,i-1])) + mat[,i]
  }
  
  mat
}

.shrinkColumnsTogether <- function(mat, buffer){
  d <- ncol(mat)
  if(d == 1) return(mat)
  
  for(i in 2:d){
    mat[,i] = mat[,i] - min(abs(mat[,i] - mat[,i-1])) + buffer
  }
  
  mat
}

.extractColumnsFromMri <- function(mri, xlim, ylim, zlim){
  stopifnot(class(mri) == "BrcFmri")
  
  idx.3d <- .convertVoxel3DGridto2D(mri$parcellation$dim3d, xlim, ylim, zlim)
  idx.col <- unique(as.numeric(mri$parcellation$partition[idx.3d]))
  mri$data2d[,idx.col]
}

.removeZeroTimeSeries <- function(mat){
  stopifnot(is.matrix(mat))
  
  idx <- which(apply(mat, 2, function(x){sum(abs(x))}) != 0)
  if(length(idx) == 0) stop("mat contains no columns that are not all-0")
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