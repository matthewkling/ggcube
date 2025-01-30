
#' Define a 3D plotting projection
#'
#' This function creates a `proj` object containing the parameters needed to
#' project 3D data onto a 2D surface.
#'
#' @param pitch,roll,yaw rotation angles around the x-, y, and z-axes, respectively, in degrees.
#' @param shear_xy,shear_xz,shear_yz bivariate shear factors in each dimension.
#' @param persp logical indicating whether to project the data using perspective (i.e., with
#'    objects farther from the viewer drawn smaller). Default is FALSE.
#' @param dist scaled distance on the rotated z-axis from the center of the data to viewer;
#'    only used if `persp = TRUE`.
#' @param hjust,vjust scaled distance on the rotated x- and y-axes from the center of the
#'    data to the viewer; only used if `persp = TRUE`.
#' @param data data.frame from which to compute axis limits and breaks. The first 3 variables
#'    should corresponding to the x, y, and z variables that will be supplied to `aes()`.
#' @return an object of class `proj`.
#' @export
projection <- function(pitch = 0, roll = 0, yaw = 0,
                       shear_xy = 0, shear_xz = 0, shear_yz = 0,
                       persp = FALSE, dist = 1, hjust = .5, vjust = .5,
                       xlim = NULL, ylim = NULL, zlim = NULL,
                       xbreaks = NULL, ybreaks = NULL, zbreaks = NULL,
                       data = NULL){

      # placeholder for parameter checks and messages

      prj <- list(yaw = yaw, pitch = pitch, roll = roll,
                  shear_xy = shear_xy, shear_xz = shear_xz, shear_yz = shear_yz,
                  persp = persp, dist = dist, hjust = hjust, vjust = vjust,
                  xlim = xlim, ylim = ylim, zlim = zlim)
      class(prj) <- "proj"
      if(!is.null(data)) prj <- add_limits(prj, data)
      prj
}


project <- function(data, prj = projection(), expand = 0) {

        # rescale data to unit cube with origin at midpoint
        if(is.null(prj$xlim)) prj$xlim <- range(data[, "x"])
        if(is.null(prj$ylim)) prj$ylim <- range(data[, "y"])
        if(is.null(prj$zlim)) prj$zlim <- range(data[, "z"])
        rscl <- function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) - .5
        data[, "x"] <- scales::rescale(data[, "x"], c(-.5, .5), prj$xlim)
        data[, "y"] <- scales::rescale(data[, "y"], c(-.5, .5), prj$ylim)
        data[, "z"] <- scales::rescale(data[, "z"], c(-.5, .5), prj$zlim)
        # data[,c("x", "y", "z")] <- apply(data[,c("x", "y", "z")], 2, rscl)
        if(expand != 0) data[, c("x", "y", "z")] <- data[, c("x", "y", "z")] * (1 + expand)

        # shear
        shr <- diag(3)
        # shr[2, 1] <- prj$shear
        shr[2, 1] <- prj$shear_xy
        shr[3, 1] <- prj$shear_xz
        shr[3, 2] <- prj$shear_yz

        # rotate
        yaw <- prj$yaw / 180 * pi
        pitch <- prj$roll / 180 * pi
        roll <- prj$pitch / 180 * pi
        rot <- matrix(c(cos(yaw)*cos(pitch), cos(yaw)*sin(pitch)*sin(roll)-sin(yaw)*cos(roll), cos(yaw)*sin(pitch)*cos(roll)+sin(yaw)*sin(roll),
                        sin(yaw)*cos(pitch), sin(yaw)*sin(pitch)*sin(roll)+cos(yaw)*cos(roll), sin(yaw)*sin(pitch)*cos(roll)-cos(yaw)*sin(roll),
                        -sin(pitch),         cos(pitch)*sin(roll),                             cos(pitch)*cos(roll)),
                      nrow = 3, byrow = T)

        y <- as.matrix(data[,c("x", "y", "z")]) %*% shr %*% rot

        # perspective
        if(prj$persp){
                p1 <- matrix(0, 4, 4)
                n <- prj$dist
                f <- prj$dist + 1
                lr <- .5
                tb <- .5
                p1[1, 1] <- n / lr
                p1[2, 2] <- n / tb
                p1[3, 3] <- (f + n) / (n - f)
                p1[3, 4] <- 2 * f * n / (n - f)
                p1[4, 3] <- -1
                y[,3] <- y[,3] - .5 - prj$dist
                y[,2] <- y[,2] + .5 - prj$vjust
                y[,1] <- y[,1] + .5 - prj$hjust
                y <- cbind(y, 1) %*% p1
                y <- apply(y, 1, function(x) x / x[4]) %>% t()
                y[, 3] <- scales::rescale(-y[, 3])
        }

        data[,c("x", "y", "z")] <- y[,1:3]
        data
}



add_limits <- function(prj, dlim){

      if(class(prj) != "proj") stop("`prj` must be a `proj` class object but is not.")

      if(is.null(prj$xlim)) prj$xlim <- range(dlim[, 1])
      if(is.null(prj$ylim)) prj$ylim <- range(dlim[, 2])
      if(is.null(prj$zlim)) prj$zlim <- range(dlim[, 3])

      breaks <- prj[c("xlim", "ylim", "zlim")] %>%
            lapply(function(x) labeling::extended(x[1], x[2], 5, only.loose = T) )
      prj$xbreaks <- breaks$xlim
      prj$ybreaks <- breaks$ylim
      prj$zbreaks <- breaks$zlim

      prj
}
