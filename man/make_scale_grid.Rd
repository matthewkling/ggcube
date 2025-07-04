% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel-rendering.R
\name{make_scale_grid}
\alias{make_scale_grid}
\title{Generate grid data using actual scale breaks with aspect ratio}
\usage{
make_scale_grid(visible_faces, scale_info, scales = "free", ratio = c(1, 1, 1))
}
\arguments{
\item{visible_faces}{Character vector of visible face names}

\item{scale_info}{List containing limits and breaks for x, y, z}

\item{scales}{Aspect ratio behavior ("free" or "fixed")}

\item{ratio}{Length-3 numeric vector of axis ratios}
}
\value{
Data frame with grid lines in standard domain, including break values
}
\description{
Generate grid data using actual scale breaks with aspect ratio
}
