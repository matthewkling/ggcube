% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-identity-3d.R
\name{stat_identity_3d}
\alias{stat_identity_3d}
\title{3D-aware identity transformation}
\usage{
stat_identity_3d(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}}.}

\item{data}{The data to be displayed in this layer.}

\item{geom}{The geometric object to use display the data.}

\item{position}{Position adjustment, defaults to "identity".}

\item{na.rm}{If \code{FALSE}, missing values are removed with a warning.}

\item{show.legend}{Logical indicating whether this layer should be included in legends.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics.}

\item{...}{Other arguments passed on to \code{\link[=layer]{layer()}}.}
}
\description{
This stat performs identity transformation (passes data through unchanged) while
properly handling discrete scales for 3D coordinate systems. It converts factor
and character variables to numeric positions and preserves original values in
\verb{*_raw} columns for proper scale labeling.
}
\details{
This stat is primarily intended for use with 3D geoms that need discrete scale
support, following the same pattern as other ggcube stats.
}
\section{Computed variables}{

\itemize{
\item \code{x_raw}, \code{y_raw}, \code{z_raw}: Original values before discrete-to-numeric conversion
}
}

\seealso{
\code{\link[=geom_point_3d]{geom_point_3d()}} which uses this stat by default for discrete scale support.
}
