
#' Generate regular grids
#'
#' Creates a rectangular, hexagonal, or triangular grid of specified proportions.
#' Returns tile vertex data formatted for `geom_poygon()`
#'
#' @param grid Character argument specifying geometry of grid to generate.
#'   Options include `"rect"` for rectangular grid (the default), `"tri"` for
#'   triangular grid, or `"hex"` for hexagonal grid.
#' @param n Either a single integer specifying grid resolution in both dimensions,
#'   or a vector of length 2 specifying `c(nx, ny)` for different resolutions.
#'   Default is 40. Higher values create smoother surfaces but slower rendering.
#' @param direction Either `"x"` (the default) or `"y"`, specifying the orientation
#'   of tile rows. Ignored for rectangular grids.
#' @param xlim,ylim Length-two numeric vectors bounding box over which to
#'   generate the grid.
#'
#' @details Grids are constructed such that tiles are approximately equilateral
#'   when scaled to a square domain, unless `n` gives separate resolution values
#'   for the two dimensions. For triangular and hexagonal grids, this means that
#'   `n` is only approximate.
#'
#' @return A data frame with the following columns: `x`, `y`, `group` (integer denoting)
#'   unique polygon id, and `order` (integer giving vertex order, for plotting).
#'
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

      switch(grid,
             rect = make_rect_tiles(xlim, ylim, n),
             tri = make_tri_tiles(xlim, ylim, n, direction),
             hex = make_hex_tiles(xlim, ylim, n, direction),
             stop("unknown argument to `grid`"))
}

make_rect_tiles <- function(xlim, ylim, n) {

      if(length(n) == 1) n <- c(n, n)
      x_seq <- seq(xlim[1], xlim[2], length.out = n[1])
      y_seq <- seq(ylim[1], ylim[2], length.out = n[2])

      data <- expand_grid(x = x_seq, y = y_seq)

      data <- data %>%
            ungroup() %>%
            mutate(group = 1:nrow(.))

      dy <- data %>%
            group_by(x) %>%
            mutate(y = lag(y)) %>%
            ungroup()

      dx <- data %>%
            group_by(y) %>%
            mutate(x = lag(x)) %>%
            ungroup()

      dxy <- data.frame(x = dx$x,
                        y = dy$y) %>%
            left_join(data, by = join_by(x, y)) %>%
            mutate(group = dx$group)

      d <- bind_rows(data, dx, dxy, dy) %>%
            na.omit() %>%
            group_by(group) %>%
            filter(n() == 4) %>%
            arrange(x, y) %>%
            mutate(order = c(1, 2, 4, 3)) %>%
            ungroup() %>%
            arrange(group, order) %>%
            as.data.frame()

      return(d)
}

make_tri_tiles <- function(xlim, ylim, n, direction = "x") {

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

      # Flip orientation if applicable
      if(direction == "y") tri <- tri %>%
            mutate(temp = x,
                   x = y,
                   y = temp) %>%
            select(-temp)

      # Scale
      tri <- tri %>%
            mutate(x = scales::rescale(x, xlim),
                   y = scales::rescale(y, ylim))

      return(tri)
}

make_hex_tiles <- function(xlim, ylim, n, direction = "x") {

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

      # Flip orientation if applicable
      if(direction == "y") hex <- hex %>%
            mutate(temp = x,
                   x = y,
                   y = temp) %>%
            select(-temp)

      # Scale
      hex <- hex %>%
            mutate(x = scales::rescale(x, xlim),
                   y = scales::rescale(y, ylim))

      return(hex)
}
