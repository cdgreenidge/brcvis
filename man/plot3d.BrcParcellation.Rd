% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot3d.BrcParcellation.R
\name{plot3d.BrcParcellation}
\alias{plot3d.BrcParcellation}
\title{3D Plotting for a BrcParcellation Object}
\usage{
\method{plot3d}{BrcParcellation}(x, view = "sagittal", colors = NULL, ...)
}
\arguments{
\item{x}{the BrcParcellation object}

\item{view}{a string specifying the camera view. One of "saggital",
"saggital_reversed", "coronal", "coronal_reversed", "axial", or
"axial_reversed"}

\item{colors}{A list of R colors similar to that generated by
grDevices::rainbow().  Must contain a background color, plus exactly as
many colors as there are unique parcels. If NULL, a default coloring is
used.}

\item{...}{not used}
}
\description{
Creates a 3D plot of a BrcParcellation.
}

