% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lighting.R
\name{compute_lighting}
\alias{compute_lighting}
\title{Apply lighting models to surface normals}
\usage{
compute_lighting(normals, lighting = lighting("lambert"))
}
\arguments{
\item{normals}{Matrix with 3 columns (x, y, z normal components), where each
row represents a face normal vector. Should be unit vectors (normalized).}

\item{lighting}{A lighting specification object created by \code{lighting()}}
}
\value{
Vector of lighting values. For most methods, returns numeric values.
For \code{method = "normal_rgb"}, returns hex color strings with \code{I()}
class for identity scaling.
}
\description{
Computes lighting values from surface normals using various lighting models.
This function is shared between stat_surface and stat_terrain to ensure
consistent lighting behavior.
}
