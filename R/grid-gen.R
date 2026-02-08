
# Tile grids --------------------------------------

#' Generate rectangular, triangular, or hexagonal grids
#'
#' Creates a regular grid of tiles of specified resolution and geometry.
#' This function is called by various ggcube stats that generate surfaces, but can
#' also be used directly. Returns tile vertex data formatted for geoms like
#' `geom_poygon()` and `geom_poygon_3d()`.
#'
#' @param xlim,ylim Length-two numeric vectors defining bounding box over which to
#'   generate the grid.
#' @inheritParams grid_generation
#'
#' @details Grids are constructed such that tiles are approximately equilateral
#'   when scaled to a square domain, unless `n` gives separate resolution values
#'   for the two dimensions. For triangular and hexagonal grids, this means that
#'   `n` is only approximate.
#'
#' @return A data frame with the following columns: `x`, `y`, `group` (integer denoting
#'   unique polygon id), and `order` (integer giving vertex order for plotting; vertices
#'   are in counter-clockwise winding order).
#'
#' @examples
#' # direct use
#' g <- make_tile_grid("tri", xlim = c(0, 5), ylim = c(-100, 100))
#' head(g)
#'
#' # use from within ggcube stat
#' ggplot() +
#'   stat_function_3d(
#'     grid = "hex", n = 20, xlim = c(-2, 2), ylim = c(-2, 2),
#'     fun = function(x, y) - x^2 - y^2,
#'     fill = "black", color = "white", light = NULL) +
#'   coord_3d()
#'
#' @seealso [stat_function_3d()], [stat_smooth_3d()], and [stat_density_3d()] for ggcube layers that
#'   use `make_tile_grid()` to generate gridded surfaces.
#' @export
make_tile_grid <- function(grid = c("triangle", "rectangle", "hexagon"),
                           n = 40,
                           direction = c("x", "y"),
                           xlim, ylim,
                           trim = TRUE) {

      grid <- match.arg(grid)
      direction <- match.arg(direction)

      if(is.null(n)) n <- 40
      n <- as.integer(n)
      if(any(n < 2)) stop("`n` must be at least 2")
      if(! length(n) %in% 1:2) stop("`n` must be a vector of length 1 or 2")
      if(direction == "y") n <- rev(n)

      tiles <- switch(grid,
                      rectangle = make_rect_tiles(n),
                      triangle = make_tri_tiles(n),
                      hexagon = make_hex_tiles(n))

      tiles <- tiles %>%
            trim_grid_edges(grid, trim) %>%
            transpose_grid(direction) %>%
            rescale_grid(xlim, ylim)

      return(tiles)
}

trim_grid_edges <- function(tiles, grid, trim){

      if(trim && grid == "triangle"){
            # move errant vertices
            tiles <- mutate(tiles,
                            x = case_when(x == min(x) ~ min(x[x > min(x)]),
                                          x == max(x) ~ max(x[x < max(x)]),
                                          TRUE ~ x))
      }

      if(trim && grid == "hexagon"){
            # move or remove errant vertices
            tiles <- tiles %>%
                  filter(! x %in% range(x)) %>%
                  group_by(group) %>%
                  filter((! y %in% range(tiles$y)) | length(group) == 5)
            tiles <- tiles %>%
                  mutate(y = ifelse(x %in% range(tiles$x) &
                                          y %in% range(tiles$y),
                                    mean(range(y)), y)) %>%
                  ungroup() %>%
                  filter(! y %in% range(y))
      }

      # (no clipping needed for rectangles)
      return(tiles)
}


make_rect_tiles <- function(n) {

      # Centers
      if(length(n) == 1) n <- c(n, n)
      d <- expand_grid(x = 1:n[1], y = 1:n[2])

      # Vertices
      dx <- dy <- .5
      d <- d %>%
            mutate(group = 1:nrow(.)) %>%
            group_by(group) %>%
            reframe(x = c(x+dx, x-dx, x-dx, x+dx), # ccw order
                    y = c(y+dy, y+dy, y-dy, y-dy),
                    group = group,
                    order = 1:4) %>%
            ungroup() %>%
            arrange(group, order)

      return(d)
}

make_tri_tiles <- function(n) {

      if (length(n) == 1) {
            # Equilateral triangles
            n <- ceiling(n * .75)
            n_x <- n * 2 + 1
            n_y <- max(1, round(n_x / sqrt(3)))
      } else {
            # Non-equilateral case - user specifies both dimensions
            n_x <- n[1]
            n_y <- n[2]
      }

      side_length <- 1
      tri_height <- side_length * sqrt(3) / 2

      # Create grid of triangle centers
      dx <- side_length / 2
      dy <- tri_height / 2
      x <- seq(dx, by = dx, length.out = n_x)
      y <- seq(dy, by = tri_height, length.out = n_y)
      tri <- expand_grid(row = 1:n_y, column = 1:n_x) %>%
            mutate(x = x[column],
                   y = y[row],
                   drn = sign(((row + column) %% 2) - .5))

      # Generate vertices
      tri <- tri %>%
            mutate(group = 1:nrow(.)) %>%
            group_by(group) %>%
            reframe(x = c(x - dx, x + dx, x), # ccw order
                    y = c(y - dy * drn, y - dy * drn, y + dy * drn),
                    group = group,
                    order = if(drn == 1) 1:3 else 3:1) %>%
            ungroup() %>%
            arrange(group, order)

      return(tri)
}

make_hex_tiles <- function(n) {

      hr3 <- sqrt(3)/2

      if (length(n) == 1) {
            n_x <- n
            n_y <- max(1, round(n_x * hr3))
      } else {
            n_x <- n[1]
            n_y <- n[2]
      }

      # Create grid of hex centers
      hex <- expand_grid(x = 1:n_x,
                         y = seq(1, by = hr3, length.out = n_y)) %>%
            mutate(y = ifelse(x %% 2 == 1, y, y - hr3/2))

      # Generate vertices
      dy <- hr3/2
      dx <- 1/3
      hex <- hex %>%
            mutate(group = 1:nrow(.)) %>%
            group_by(group) %>%
            reframe(x = c(x+dx, x+2*dx, x+dx, x-dx, x-2*dx, x-dx),
                    y = c(y+dy, y, y-dy, y-dy, y, y+dy),
                    group = group,
                    order = 6:1) %>% # ccw order
            ungroup() %>%
            arrange(group, order) %>%
            select(x, y, group, order)

      return(hex)
}

# Flip orientation if applicable
transpose_grid <- function(grid, direction){
      if(direction == "y"){
            grid <- grid %>%
                  mutate(temp = x,
                         x = y,
                         y = temp) %>%
                  select(-temp)
      }
      return(grid)
}

# Scale to target range
rescale_grid <- function(grid, xlim, ylim){
      grid %>%
            mutate(x = scales::rescale(x, xlim),
                   y = scales::rescale(y, ylim))
}

# Add computed surface orientation variables
compute_surface_vars <- function(tiles){
      left_join(tiles,
                compute_surface_gradients_from_vertices(tiles),
                by = join_by(group)) %>%
            mutate(slope = sqrt(dzdy^2 + dzdx^2),
                   aspect = atan2(dzdy, dzdx))
}


# Point grids --------------------------------------


#' Generate a grid of unique vertex points
#'
#' Creates a regular grid of unique vertex positions. Unlike [make_tile_grid()],
#' this returns one row per unique vertex position rather than polygon vertices
#' with duplicates.
#'
#' @param n Integer or length-2 integer vector specifying grid resolution.
#' @param xlim,ylim Length-two numeric vectors defining bounding box.
#'
#' @return A data frame with columns `x`, `y`, `row`, `column`.
#'
#' @keywords internal
make_point_grid <- function(grid = c("rectangle", "tri1", "tri2", "triangle"),
                            n = 40,
                            direction = c("x", "y"),
                            xlim, ylim,
                            trim = TRUE) {

      grid <- match.arg(grid)
      direction <- match.arg(direction)

      if (is.null(n)) n <- 40
      n <- as.integer(n)
      if (any(n < 2)) stop("`n` must be at least 2")
      if (!length(n) %in% 1:2) stop("`n` must be a vector of length 1 or 2")

      d <- switch(grid,
                  "rectangle" = make_rect_grid(n),
                  "tri1" = make_rect_grid(n),
                  "tri2" = make_rect_grid(n),
                  "triangle" = make_tri_grid(n, trim))

      d <- d %>%
            transpose_grid(direction) %>%
            rescale_grid(xlim, ylim)

      return(d)
}

make_rect_grid <- function(n){
      if (length(n) == 1) n <- c(n, n)
      tidyr::expand_grid(x = 1:n[1], y = 1:n[2])
}

make_tri_grid <- function(n, trim = FALSE){

      side_length <- 1
      tri_height <- side_length * sqrt(3) / 2

      if (length(n) == 1) {
            # Equilateral triangles
            n_x <- n
            n_y <- n * side_length / tri_height
      } else {
            # Non-equilateral case - user specifies both dimensions
            n_x <- n[1]
            n_y <- n[2]
      }

      x <- seq(0, by = side_length, length.out = n_x)
      y <- seq(0, by = tri_height, length.out = n_y)

      grid <- expand_grid(row = 1:n_y, column = 1:n_x) %>%
            mutate(x = x[column],
                   y = y[row],
                   x = ifelse(row %% 2 == 0, x, x + side_length/2)) %>%
            select(-row, -column)

      if(trim){
            xmax <- filter(grid, x == max(x)) %>%
                  mutate(x = min(grid$x))
            grid <- grid %>%
                  mutate(x = ifelse(x == max(x), max(x[x != max(x)]), x)) %>%
                  bind_rows(xmax)
      }

      return(grid)
}

is_regular_grid <- function(data){
      if(! "group" %in% names(data)) data$group <- 1
      data <- split(data, data$group)
      reg <- sapply(data, function(d){
            x_vals <- sort(unique(d$x))
            y_vals <- sort(unique(d$y))
            nrow(d) == length(x_vals) * length(y_vals)
      })
      all(reg)
}

#' Compute gradients at grid points using finite differences
#'
#' For rectangular grids, computes dz/dx and dz/dy at each grid point using
#' central differences where possible, forward/backward differences at edges.
#'
#' @param data Data frame with x, y, z and row, column indices
#'
#' @return Data frame with added columns: dzdx, dzdy, slope, aspect
#'
#' @keywords internal
compute_point_gradients <- function(data) {

      if(is_regular_grid(data)){
            data <- compute_grid_point_gradients(data)
      }else{
            data <- compute_irregular_point_gradients(data)
      }

      # Compute derived variables
      data$slope <- sqrt(data$dzdx^2 + data$dzdy^2)
      data$aspect <- atan2(data$dzdy, data$dzdx)

      return(data)
}

compute_grid_point_gradients <- function(data){
      # Add row/column indices for regular grid
      x_vals <- sort(unique(data$x))
      y_vals <- sort(unique(data$y))
      data <- data %>%
            mutate(column = match(x, x_vals),
                   row = match(y, y_vals))

      # Get grid dimensions and spacing
      n_col <- max(data$column)
      n_row <- max(data$row)

      x_vals <- sort(unique(data$x))
      y_vals <- sort(unique(data$y))
      dx <- if (length(x_vals) > 1) mean(diff(x_vals)) else 1
      dy <- if (length(y_vals) > 1) mean(diff(y_vals)) else 1

      # Create lookup matrix for z values
      z_matrix <- matrix(NA_real_, nrow = n_row, ncol = n_col)
      for (i in 1:nrow(data)) {
            z_matrix[data$row[i], data$column[i]] <- data$z[i]
      }

      # Compute gradients using finite differences
      dzdx_matrix <- matrix(NA_real_, nrow = n_row, ncol = n_col)
      dzdy_matrix <- matrix(NA_real_, nrow = n_row, ncol = n_col)

      for (r in 1:n_row) {
            for (c in 1:n_col) {
                  # dz/dx (central difference, forward/backward at edges)
                  if (c == 1) {
                        dzdx_matrix[r, c] <- (z_matrix[r, c + 1] - z_matrix[r, c]) / dx
                  } else if (c == n_col) {
                        dzdx_matrix[r, c] <- (z_matrix[r, c] - z_matrix[r, c - 1]) / dx
                  } else {
                        dzdx_matrix[r, c] <- (z_matrix[r, c + 1] - z_matrix[r, c - 1]) / (2 * dx)
                  }

                  # dz/dy (central difference, forward/backward at edges)
                  if (r == 1) {
                        dzdy_matrix[r, c] <- (z_matrix[r + 1, c] - z_matrix[r, c]) / dy
                  } else if (r == n_row) {
                        dzdy_matrix[r, c] <- (z_matrix[r, c] - z_matrix[r - 1, c]) / dy
                  } else {
                        dzdy_matrix[r, c] <- (z_matrix[r + 1, c] - z_matrix[r - 1, c]) / (2 * dy)
                  }
            }
      }

      # Add gradients to data
      data$dzdx <- mapply(function(r, c) dzdx_matrix[r, c], data$row, data$column)
      data$dzdy <- mapply(function(r, c) dzdy_matrix[r, c], data$row, data$column)

      data
}


compute_irregular_point_gradients <- function(data) {

      # Extract coordinates
      x_vals <- data$x
      y_vals <- data$y
      z_vals <- data$z

      # Perform Delaunay triangulation
      points_2d <- cbind(x_vals, y_vals)
      triangles <- geometry::delaunayn(points_2d)

      # Initialize gradient storage
      n_triangles <- nrow(triangles)
      triangle_dzdx <- numeric(n_triangles)
      triangle_dzdy <- numeric(n_triangles)
      triangle_centroids <- matrix(0, nrow = n_triangles, ncol = 2)

      # Compute gradient for each triangle
      for (i in seq_len(n_triangles)) {
            idx <- triangles[i, ]

            # Get triangle vertices
            x1 <- x_vals[idx[1]]; y1 <- y_vals[idx[1]]; z1 <- z_vals[idx[1]]
            x2 <- x_vals[idx[2]]; y2 <- y_vals[idx[2]]; z2 <- z_vals[idx[2]]
            x3 <- x_vals[idx[3]]; y3 <- y_vals[idx[3]]; z3 <- z_vals[idx[3]]

            # Edge vectors
            v1 <- c(x2 - x1, y2 - y1, z2 - z1)
            v2 <- c(x3 - x1, y3 - y1, z3 - z1)

            # Normal vector via cross product
            normal <- c(
                  v1[2] * v2[3] - v1[3] * v2[2],
                  v1[3] * v2[1] - v1[1] * v2[3],
                  v1[1] * v2[2] - v1[2] * v2[1]
            )

            # Plane equation: normal[1]*x + normal[2]*y + normal[3]*z = d
            # Gradient: dz/dx = -normal[1]/normal[3], dz/dy = -normal[2]/normal[3]
            if (abs(normal[3]) > 1e-10) {
                  triangle_dzdx[i] <- -normal[1] / normal[3]
                  triangle_dzdy[i] <- -normal[2] / normal[3]
            } else {
                  # Vertical or near-vertical triangle
                  triangle_dzdx[i] <- NA
                  triangle_dzdy[i] <- NA
            }

            # Centroid
            triangle_centroids[i, 1] <- (x1 + x2 + x3) / 3
            triangle_centroids[i, 2] <- (y1 + y2 + y3) / 3
      }

      # For each point, find adjacent triangles and compute weighted average
      n_points <- nrow(data)
      point_dzdx <- numeric(n_points)
      point_dzdy <- numeric(n_points)

      for (i in seq_len(n_points)) {
            # Find triangles containing this point
            adj_triangles <- which(apply(triangles, 1, function(tri) i %in% tri))

            if (length(adj_triangles) == 0) {
                  point_dzdx[i] <- NA
                  point_dzdy[i] <- NA
                  next
            }

            # Get gradients for adjacent triangles (remove NAs)
            valid <- !is.na(triangle_dzdx[adj_triangles])
            adj_triangles <- adj_triangles[valid]

            if (length(adj_triangles) == 0) {
                  point_dzdx[i] <- NA
                  point_dzdy[i] <- NA
                  next
            }

            # Calculate distances to centroids
            px <- x_vals[i]
            py <- y_vals[i]
            distances <- sqrt(
                  (triangle_centroids[adj_triangles, 1] - px)^2 +
                        (triangle_centroids[adj_triangles, 2] - py)^2
            )

            # Use inverse distance weighting (add small epsilon to avoid division by zero)
            epsilon <- 1e-10
            weights <- 1 / (distances + epsilon)
            weights <- weights / sum(weights)

            # Weighted average
            point_dzdx[i] <- sum(weights * triangle_dzdx[adj_triangles])
            point_dzdy[i] <- sum(weights * triangle_dzdy[adj_triangles])
      }

      # Add gradients to original data
      data$dzdx <- point_dzdx
      data$dzdy <- point_dzdy

      return(data)
}


#' Convert point grid to polygon tiles
#'
#' Takes a grid of points and tessellates into polygon tiles.
#'
#' @param data Data frame with x, y, z columns (and optionally row, column).
#' @param method Tessellation method: "grid" for regular grid, "delaunay" for
#'   triangulation, "auto" to detect.
#' @param grid_type For method="grid", type of tiles: "rectangle", "tri1", "tri2".
#' @param group_prefix String prefix for polygon group IDs.
#'
#' @return Data frame with polygon vertices including `group` and `order` columns.
#'
#' @keywords internal
points_to_tiles <- function(data,
                            method = "auto",
                            grid_type = "rectangle",
                            group_prefix = "surface__tile") {

      method <- match.arg(method, c("auto", "grid", "delaunay"))

      # Auto-detect method
      if (method == "auto") {
            if (is_regular_grid(data)) {
                  method <- "grid"
            } else {
                  method <- "delaunay"
            }

      }

      if (method == "grid") {
            tiles <- rect_points_to_tiles(data, grid_type, group_prefix)
      } else {
            tiles <- delaunay_points_to_tiles(data, group_prefix)
      }

      return(tiles)
}


#' Convert regular grid points to tiles
#' @keywords internal
rect_points_to_tiles <- function(data, grid_type, group_prefix) {

      grid_type <- match.arg(grid_type, c("rectangle", "tri1", "tri2"))

      # Ensure row/column indices exist
      if (!all(c("row", "column") %in% names(data))) {
            x_vals <- sort(unique(data$x))
            y_vals <- sort(unique(data$y))
            data <- data %>%
                  mutate(column = match(x, x_vals),
                         row = match(y, y_vals))
      }

      n_col <- max(data$column)
      n_row <- max(data$row)

      if (n_col < 2 || n_row < 2) {
            stop("Need at least 2 points in each dimension to create tiles")
      }

      # Create tile_id for each cell
      tiles <- tidyr::expand_grid(tile_col = 1:(n_col - 1), tile_row = 1:(n_row - 1)) %>%
            mutate(tile_id = row_number())

      # Get four corners for each tile in CCW order when viewed from above:
      # bottom-left -> bottom-right -> top-right -> top-left
      corner_offsets <- data.frame(
            d_col = c(0, 1, 1, 0),
            d_row = c(0, 0, 1, 1),
            order = 1:4
      )

      tile_vertices <- tiles %>%
            tidyr::crossing(corner_offsets) %>%
            mutate(column = tile_col + d_col,
                   row = tile_row + d_row) %>%
            left_join(data, by = c("column", "row")) %>%
            select(-d_col, -d_row, -tile_col, -tile_row)

      # Handle triangle splitting
      if (grid_type %in% c("tri1", "tri2")) {
            tile_vertices <- split_quads_to_triangles(tile_vertices, grid_type)
      }

      # Create hierarchical group column
      if ("group" %in% names(data) && !all(data$group == data$group[1])) {
            tile_vertices <- tile_vertices %>%
                  mutate(group = paste0(group_prefix, tile_id, "::", group))
      } else {
            tile_vertices <- tile_vertices %>%
                  mutate(group = paste0(group_prefix, tile_id))
      }

      tile_vertices <- tile_vertices %>%
            select(-column, -row, -tile_id) %>%
            arrange(group, order)

      return(tile_vertices)
}


#' Split quadrilateral tiles into triangles
#' @keywords internal
split_quads_to_triangles <- function(tile_vertices, grid_type) {
      # Input quad vertices in CCW order when viewed from above:
      # 1 = bottom-left, 2 = bottom-right, 3 = top-right, 4 = top-left
      #
      # tri1: diagonal from bottom-left(1) to top-right(3)
      #   Triangle A: 1 -> 2 -> 3 (CCW from above)
      #   Triangle B: 1 -> 3 -> 4 (CCW from above)
      #
      # tri2: diagonal from bottom-right(2) to top-left(4)
      #   Triangle A: 1 -> 2 -> 4 (CCW from above)
      #   Triangle B: 2 -> 3 -> 4 (CCW from above)

      i <- switch(grid_type,
                  tri1 = c(1, 2, 3, 1, 3, 4),
                  tri2 = c(1, 2, 4, 2, 3, 4))

      tile_vertices %>%
            group_by(tile_id) %>%
            arrange(order) %>%
            reframe(
                  across(everything(), ~ .x[i]),
                  tile_id = tile_id[1] + rep(c(0.1, 0.2), each = 3),
                  order = rep(1:3, 2)
            ) %>%
            ungroup()
}


#' Convert irregular points to triangular tiles via Delaunay triangulation
#' @keywords internal
delaunay_points_to_tiles <- function(data, group_prefix) {

      if (!requireNamespace("geometry", quietly = TRUE)) {
            stop("Delaunay triangulation requires the 'geometry' package.\n",
                 "Install with: install.packages('geometry')")
      }

      all_data <- data

      tile_vertices <- lapply(unique(all_data$group), function(i){
            data <- filter(all_data, group == i)

            if (nrow(data) < 3) {
                  stop("Need at least 3 points for triangulation")
            }

            # Normalize coordinates to avoid numerical issues
            x_range <- range(data$x, na.rm = TRUE)
            y_range <- range(data$y, na.rm = TRUE)
            x_scale <- max(diff(x_range), 1e-10)
            y_scale <- max(diff(y_range), 1e-10)

            x_norm <- (data$x - x_range[1]) / x_scale
            y_norm <- (data$y - y_range[1]) / y_scale

            # Perform Delaunay triangulation with robust options
            tri <- tryCatch({
                  geometry::delaunayn(cbind(x_norm, y_norm), options = "Qt Qc Qz Qbb")
            }, error = function(e) {
                  # Fallback without options
                  geometry::delaunayn(cbind(x_norm, y_norm))
            })

            n_tri <- nrow(tri)
            if (is.null(n_tri) || n_tri == 0) {
                  stop("Delaunay triangulation produced no triangles")
            }

            # Ensure CCW winding order for all triangles
            for (i in 1:n_tri) {
                  idx <- tri[i, ]
                  x1 <- data$x[idx[1]]; y1 <- data$y[idx[1]]
                  x2 <- data$x[idx[2]]; y2 <- data$y[idx[2]]
                  x3 <- data$x[idx[3]]; y3 <- data$y[idx[3]]

                  # Signed area: positive = CCW, negative = CW
                  signed_area <- (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

                  if (signed_area < 0) {
                        tri[i, ] <- tri[i, c(1, 3, 2)]
                  }
            }

            # Build tile data
            tile_vertices <- data.frame(
                  tile_id = rep(1:n_tri, each = 3),
                  order = rep(1:3, n_tri),
                  .vertex_idx = as.vector(t(tri))
            )

            # Join with original data
            data$.vertex_idx <- 1:nrow(data)
            tile_vertices <- tile_vertices %>%
                  left_join(data, by = ".vertex_idx") %>%
                  select(-.vertex_idx)

            # Create group column
            # if ("group" %in% names(data) && !all(data$group == data$group[1])) {
            tile_vertices <- tile_vertices %>%
                  mutate(group = paste0(group_prefix, tile_id, "::", group))
            # } else {
            #       tile_vertices <- tile_vertices %>%
            #             mutate(group = paste0(group_prefix, tile_id))
            # }
      })

      tile_vertices <- bind_rows(tile_vertices) %>%
            select(-tile_id) %>%
            arrange(group, order)

      return(tile_vertices)
}


#' Convert point grid to ridgeline polygons
#'
#' Takes a grid of points and creates ridgeline polygons where each slice
#' along one axis becomes a filled polygon.
#'
#' @param data Data frame with x, y, z columns.
#' @param direction Direction of ridges: "x" means one ridge per unique x value
#'   (ridge varies in y), "y" means one ridge per unique y value (ridge varies in x).
#' @param base Z-value for the bottom of ridgeline polygons. If NULL, uses min(z).
#' @param group_prefix String prefix for polygon group IDs.
#'
#' @return Data frame with polygon vertices for ridgeline rendering.
#'
#' @keywords internal
points_to_ridgelines <- function(data,
                                 direction = "x",
                                 base = NULL,
                                 group_prefix = "ridgeline__") {

      direction <- match.arg(direction, c("x", "y"))

      # Get unique points (in case input has duplicates)
      points <- data %>%
            mutate(.x_round = round(x, 10),
                   .y_round = round(y, 10)) %>%
            distinct(.x_round, .y_round, .keep_all = TRUE) %>%
            select(-.x_round, -.y_round)

      # Compute base if not provided
      if (is.null(base)) {
            base <- min(points$z, na.rm = TRUE)
      }

      # Determine slice and ridge variables
      if (direction == "x") {
            slice_var <- "x"
            ridge_var <- "y"
      } else {
            slice_var <- "y"
            ridge_var <- "x"
      }

      # Get unique slice values
      slice_vals <- sort(unique(round(points[[slice_var]], 10)))

      if (length(slice_vals) < 1) {
            stop("No data points found for ridgeline generation")
      }

      # Build ridgeline polygons
      all_ridges <- vector("list", length(slice_vals))

      for (i in seq_along(slice_vals)) {
            slice_val <- slice_vals[i]

            # Get points for this slice
            slice_data <- points[abs(points[[slice_var]] - slice_val) < 1e-9, ]

            if (nrow(slice_data) < 2) {
                  warning("Slice at ", slice_var, " = ", slice_val,
                          " has fewer than 2 points; skipping")
                  next
            }

            # Sort by ridge variable
            slice_data <- slice_data[order(slice_data[[ridge_var]]), ]

            ridge_min <- min(slice_data[[ridge_var]])
            ridge_max <- max(slice_data[[ridge_var]])
            zmax <- max(slice_data$z, na.rm = TRUE)
            n_ridge_pts <- nrow(slice_data)

            # Build closed polygon vertices in CCW order when viewed from front with default projection
            # For direction="x", front faces negative x (toward typical viewer)
            # For direction="y", front faces negative y (toward typical viewer)

            if (direction == "x") {
                  # Polygon in y-z plane at constant x, front faces -x
                  # Looking from -x toward +x: y increases right, z increases up
                  # CCW from -x view: (y_max, base) -> ridge points decreasing y -> (y_min, base)
                  poly_x <- rep(slice_val, n_ridge_pts + 2)
                  poly_y <- c(ridge_min, slice_data$y, ridge_max)
                  poly_z <- c(base, slice_data$z, base)
            } else {
                  # Polygon in x-z plane at constant y, front faces -y
                  # Looking from -y toward +y: x increases right, z increases up
                  # CCW from -y view: (x_max, base) -> ridge points decreasing x -> (x_min, base)
                  poly_x <- c(ridge_min, slice_data$x, ridge_max)
                  poly_y <- rep(slice_val, n_ridge_pts + 2)
                  poly_z <- c(base, slice_data$z, base)
            }

            ridge_df <- data.frame(
                  x = poly_x,
                  y = poly_y,
                  z = poly_z,
                  zmax = zmax,
                  stringsAsFactors = FALSE
            ) %>% distinct()
            nr <- nrow(ridge_df)

            # # reorder so that the first 4 vertices, which are used for backface identification
            # # are as relevant as possible
            # ridge_df <- ridge_df[c(nr-1, nr, 1:(nr-2)), ]
            ridge_df$order <- 1:nr

            # Add face_type for axis-aligned normal computation
            # Ridgelines are vertical planes, so normals point along the slice axis
            # We want front faces to point toward typical viewer (negative x/y)
            ridge_df$face_type <- if (direction == "x") "xmin" else "ymin"

            # Preserve ggplot2 internal columns
            internal_cols <- c("PANEL", "cull_backfaces", "lighting_spec")
            for (int_col in internal_cols) {
                  if (int_col %in% names(slice_data)) {
                        ridge_df[[int_col]] <- slice_data[[int_col]][1]
                  }
            }

            # Create group name
            if ("group" %in% names(slice_data)) {
                  base_group <- sub(".*::", "", slice_data$group[1])
                  ridge_df$group <- paste0(group_prefix, i, "::", base_group)
            } else {
                  ridge_df$group <- paste0(group_prefix, i)
            }

            # Preserve aesthetic columns
            aes_cols <- intersect(names(slice_data),
                                  c("fill", "colour", "color", "alpha",
                                    "linewidth", "linetype", "size"))
            for (aes_col in aes_cols) {
                  vals <- slice_data[[aes_col]]
                  ridge_df[[aes_col]] <- if (is.numeric(vals)) mean(vals, na.rm = TRUE) else vals[1]
            }

            # split self-intersecting polygons
            if(direction == "x"){
                  polys <- polyclip::polysimplify(
                        A = list(x = ridge_df$y, y = ridge_df$z),
                        fillRule = "evenodd")
                  polys <- polys %>% lapply(function(p)
                        data.frame(x = ridge_df$x[1],
                                   y = p$x,
                                   z = p$y,
                                   order = 1:length(p$x)))
            } else {
                  polys <- polyclip::polysimplify(
                        A = list(x = ridge_df$x, y = ridge_df$z),
                        fillRule = "evenodd")
                  polys <- polys %>% lapply(function(p)
                        data.frame(x = p$x,
                                   y = ridge_df$y[1],
                                   z = p$y,
                                   order = 1:length(p$x)))
            }
            for(j in 1:length(polys)) polys[[j]]$group <- paste0(ridge_df$group[1], "::", j)
            ridge_df <- do.call("rbind", polys) %>%
                  bind_cols(distinct(select(ridge_df, -x, -y, -z, -order, -group)))

            all_ridges[[i]] <- ridge_df
      }

      # Combine results
      all_ridges <- all_ridges[!sapply(all_ridges, is.null)]
      if (length(all_ridges) == 0) {
            stop("No valid ridgeline slices could be created")
      }

      result <- do.call(rbind, all_ridges)
      rownames(result) <- NULL
      # ggplot(result, aes(y, z, group = group, order = order, color = order)) + facet_wrap(~x) + geom_path() + scale_color_viridis_c()
      return(result)
}
