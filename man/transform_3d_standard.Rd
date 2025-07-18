% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-3d.R
\name{transform_3d_standard}
\alias{transform_3d_standard}
\title{Transform 3D points using rotation and optional perspective with viewpoint distance}
\usage{
transform_3d_standard(
  data,
  proj = list(pitch = 0, roll = 0, yaw = 0, persp = TRUE, dist = 2)
)
}
\arguments{
\item{data}{Data frame with x, y, z columns (in standard \link{-0.5, 0.5} domain)}

\item{proj}{A list of projection parameters}
}
\value{
Data frame with transformed coordinates, depth for sorting, and depth_scale for size scaling
}
\description{
Transform 3D points using rotation and optional perspective with viewpoint distance
}
