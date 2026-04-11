#' ggcube: Extend ggplot2 into 3D
#'
#' ggcube provides 3D geoms, stats, and coordinate systems for ggplot2.
#' Add [coord_3d()] to any plot with a z aesthetic to render it in 3D
#' with rotation, perspective, lighting, and depth sorting. Layer types
#' include surfaces, hulls, contours, ridgelines, paths, points, segments,
#' text, bars, and columns.
#'
#' @docType package
#' @name ggcube
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @importFrom ggplot2 resolution
#' @importFrom grDevices as.raster
#' @importFrom grDevices chull
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
#' @importFrom isoband isobands
#' @importFrom labeling extended
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle is_present
#' @importFrom magrittr %>%
#' @importFrom polyclip polyclip
#' @importFrom polyclip polysimplify
#' @importFrom purrr map_dfr
#' @importFrom rlang %||%
#' @importFrom scales extended_breaks
#' @importFrom scales rescale
#' @importFrom stats ave density dnorm glm lm loess na.omit predict setNames weighted.mean
#' @importFrom stringr str_remove
#' @importFrom systemfonts glyph_outline
#' @importFrom systemfonts match_fonts
#' @importFrom systemfonts shape_string
#' @importFrom tidyr expand_grid
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom utils modifyList
## usethis namespace: end
utils::globalVariables(c(
      ".", ".area", ".backface", ".prim_depth", ".render_order", ".subgroup",
      ".vertex_idx", ".vertex_order", ".x_round", ".y_round",
      "Sxx", "Sxy", "Sxz", "Syy", "Syz",
      "apply_quantization", "axis", "bin_area", "column", "cont_bin",
      "contour", "d_col", "d_row", "denom", "depth", "depth1", "depth_3d",
      "dzdx", "dzdy", "e",
      "e1x", "e1y", "e1z", "e2x", "e2y", "e2z",
      "element_type", "face", "facet", "far_corner", "foreground",
      "free_axis", "fx", "fz", "group", "id", "keep",
      "level1", "level2",
      "n_pts", "near_corner", "normal_x", "normal_y", "normal_z",
      "on_hull", "ref_circle_radius", "ref_circle_vertices",
      "simple", "sum_x", "sum_xx", "sum_xy", "sum_xz",
      "sum_y", "sum_yy", "sum_yz", "sum_z",
      "temp", "tile_col", "tile_id", "tile_row",
      "value", "vert", "vertex_order", "weight",
      "x", "x1", "x2", "x3", "x_bin",
      "y", "y1", "y2", "y3", "y_bin",
      "z", "z1", "z2", "z3", "zmean"
))
NULL
