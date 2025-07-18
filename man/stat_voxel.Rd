% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-voxel.R
\name{stat_voxel}
\alias{stat_voxel}
\title{3D voxel visualization from sparse 3D data}
\usage{
stat_voxel(
  mapping = NULL,
  data = NULL,
  geom = GeomPolygon3D,
  position = "identity",
  width = 1,
  faces = "all",
  light = lighting(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}}.}

\item{data}{The data to be displayed in this layer.}

\item{geom}{The geometric object to use display the data. Defaults to
\link{GeomPolygon3D} for proper 3D depth sorting.}

\item{position}{Position adjustment, defaults to "identity".}

\item{width}{Numeric value controlling voxel size as a fraction of grid spacing.
Default is 1.0 (voxels touch each other). Use 0.8 for small gaps, 1.2 for overlap.
Grid spacing is determined automatically using \code{\link[=resolution]{resolution()}} for each dimension.}

\item{faces}{Character vector specifying which faces to render. Options:
\itemize{
\item \code{"all"} (default): Render all 6 faces
\item \code{"none"}: Render no faces
\item Vector of face names: \code{c("zmax", "xmin", "ymax")}, etc.
}
Valid face names: "xmin", "xmax", "ymin", "ymax", "zmin", "zmax".}

\item{light}{A lighting specification object created by \code{lighting()}}

\item{na.rm}{If \code{FALSE}, missing values are removed with a warning.}

\item{show.legend}{Logical indicating whether this layer should be included in legends.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics.}

\item{...}{Other arguments passed on to \code{\link[=layer]{layer()}}.}
}
\description{
Creates 3D voxel visualizations from sparse 3D point data.
Each data point becomes a fixed-size cube centered on its coordinates.
Useful for volumetric data and 3D pixel art.
}
\section{Aesthetics}{

\code{stat_voxel()} requires the following aesthetics:
\itemize{
\item \strong{x}: X coordinate (voxel center position)
\item \strong{y}: Y coordinate (voxel center position)
\item \strong{z}: Z coordinate (voxel center position)
}

And understands these additional aesthetics:
\itemize{
\item \strong{fill}: Voxel fill color
\item \strong{colour}: Voxel border color
\item \strong{alpha}: Voxel transparency
}
}

\section{Computed variables}{

\itemize{
\item \code{light}: Computed lighting value (numeric for most methods, hex color for \code{normal_rgb})
\item \code{normal_x}, \code{normal_y}, \code{normal_z}: Face normal components
\item \code{group}: Hierarchical group identifier with format "voxel_XXXX__face_type" for proper depth sorting
\item \code{voxel_id}: Sequential voxel number
\item \code{face_type}: Face name ("zmax", "xmin", etc.)
}
}

\examples{
# Sparse 3D voxel data
voxel_data <- data.frame(
  x = c(1, 2, 3, 2, 1, 3, 4),
  y = c(1, 1, 2, 3, 2, 1, 2),
  z = c(1, 2, 1, 1, 3, 3, 2)
)

p <- ggplot(voxel_data, aes(x, y, z)) + coord_3d()

# Basic 3D voxel plot
p + stat_voxel(aes(fill = z), color = "black") +
  scale_fill_viridis_c()

# Directional lighting (like sunlight)
p + stat_voxel(aes(fill = after_stat(light)),
             light = lighting(direction = c(1, 0, .5))) +
  scale_fill_gradient(low = "darkgreen", high = "lightgreen")

# Show only visible faces for performance
p + stat_voxel(faces = c("zmax", "ymax", "xmin"), color = "black")

}
\seealso{
\code{\link[=stat_pillar]{stat_pillar()}} for variable-height columns, \code{\link[=stat_surface]{stat_surface()}} for smooth surfaces,
\code{\link[=coord_3d]{coord_3d()}} for 3D coordinate systems, \code{\link[=lighting]{lighting()}} for lighting specifications,
\link{GeomPolygon3D} for the default geometry.
}
