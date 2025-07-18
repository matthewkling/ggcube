#' ggcube: 3D plotting extension for ggplot2
#'
#' ggcube extends ggplot2 into the third dimension, providing 3D coordinate
#' systems, surface plotting, and volumetric visualization capabilities.
#'
#' To use ggcube, load both packages:
#' \code{library(ggplot2)}
#' \code{library(ggcube)}
#'
#' Key functions:
#' \itemize{
#'   \item \code{coord_3d()}: 3D coordinate system with rotation and perspective
#'   \item \code{aes()}: Enhanced aesthetic mapping with positional z support
#'   \item \code{stat_surface()}: Surface plotting from grid data
#'   \item \code{stat_hull()}: 3D convex and alpha hulls
#'   \item \code{stat_voxel()}: 3D voxel visualization
#'   \item \code{stat_pillar()}: 3D pillar/bar charts
#'   \item \code{lighting()}: Lighting specifications for 3D surfaces
#' }
#'
#' @docType package
#' @name ggcube
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @importFrom alphashape3d ashape3d
#' @importFrom geometry convhulln
#' @importFrom grDevices col2rgb
#' @importFrom grDevices hsv
#' @importFrom grDevices rgb
#' @importFrom grDevices rgb2hsv
#' @importFrom grid convertHeight
#' @importFrom grid convertWidth
#' @importFrom grid gpar
#' @importFrom grid grobHeight
#' @importFrom grid grobTree
#' @importFrom grid grobWidth
#' @importFrom grid nullGrob
#' @importFrom grid polygonGrob
#' @importFrom grid rectGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom labeling extended
#' @importFrom lifecycle deprecate_soft0
#' @importFrom lifecycle is_present
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @importFrom rlang %||%
#' @importFrom scales extended_breaks
#' @importFrom scales rescale
#' @importFrom scales resolution
#' @importFrom stats chull
#' @importFrom stringr str_remove
#' @importFrom tidyr expand_grid
#' @importFrom tidyr gather
#' @importFrom tidyr unite
## usethis namespace: end
NULL
