
#' Generate rectangular, triangular, or hexagonal grids
#'
#' Creates a regular grid of tiles of specified resolution and geometry.
#' This function is called by various ggcube stats that generate surfaces, but can
#' also be used directly. Returns tile vertex data formatted for geoms like
#' `geom_poygon()` and `geom_poygon_3d()`.
#'
#' @param grid Character argument specifying geometry of grid to generate.
#'   Options include `"rect"` (the default) for rectangular grid, `"tri"` for
#'   triangular grid, or `"hex"` for hexagonal grid.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for separate x and y resolutions.
#'   Default is `40`. Higher values create smoother surfaces but slower rendering.
#' @param direction Either `"x"` (the default) or `"y"`, specifying the orientation
#'   of tile rows. Ignored for rectangular grids.
#' @param xlim,ylim Length-two numeric vectors defining bounding box over which to
#'   generate the grid.
#'
#' @details Grids are constructed such that tiles are approximately equilateral
#'   when scaled to a square domain, unless `n` gives separate resolution values
#'   for the two dimensions. For triangular and hexagonal grids, this means that
#'   `n` is only approximate.
#'
#' @return A data frame with the following columns: `x`, `y`, `group` (integer denoting
#'   unique polygon id), and `order` (integer giving vertex order, for plotting).
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
make_tile_grid <- function(grid = c("rect", "tri", "hex"),
                           n = 40,
                           direction = c("x", "y"),
                           xlim, ylim) {

      grid <- match.arg(grid)
      direction <- match.arg(direction)

      if(is.null(n)) n <- 40
      n <- as.integer(n)
      if(any(n < 2)) stop("`n` must be at least 2")
      if(! length(n) %in% 1:2) stop("`n` must be a vector of length 1 or 2")
      if(direction == "y") n <- rev(n)

      tiles <- switch(grid,
             rect = make_rect_tiles(n),
             tri = make_tri_tiles(n),
             hex = make_hex_tiles(n))

      tiles <- tiles %>%
            transpose_grid(direction) %>%
            rescale_grid(xlim, ylim)

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
            reframe(x = c(x+dx, x+dx, x-dx, x-dx),
                    y = c(y+dy, y-dy, y-dy, y+dy),
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
      tri <- expand_grid(row = 1:n_y, col = 1:n_x) %>%
            mutate(x = x[col],
                   y = y[row],
                   drn = sign(((row + col) %% 2) - .5))

      # Generate vertices
      tri <- tri %>%
            mutate(group = 1:nrow(.)) %>%
            group_by(group) %>%
            reframe(x = c(x - dx, x + dx, x),
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
                    order = 1:6) %>%
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
