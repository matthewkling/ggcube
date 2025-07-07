facets <- function(proj, effective_ratios = c(1, 1, 1)){

      # Apply aspect ratios to cube corners
      corners <- expand.grid(x = c(-.5, .5) * effective_ratios[1],
                             y = c(-.5, .5) * effective_ratios[2],
                             z = c(-.5, .5) * effective_ratios[3])

      corners_p <- transform_3d_standard(corners, proj)

      facet_grid <- expand.grid(x = c("xmin", "xmax"),
                                y = c("ymin", "ymax"),
                                z = c("zmin", "zmax"),
                                stringsAsFactors = FALSE)  # Prevent factors

      f <- bind_cols(setNames(facet_grid, c("fx", "fy", "fz")), corners_p) %>%
            gather(facet, value, fx:fz) %>%
            mutate(facet = str_remove(facet, "f"),
                   value = str_remove(value, facet),
                   x = scales::rescale(x),
                   y = scales::rescale(y),
                   z = scales::rescale(z)) %>%
            unite("facet", facet, value, sep = "") %>%
            arrange(facet)
      f
}

vertices <- function(proj, effective_ratios = c(1, 1, 1)){
      f <- facets(proj, effective_ratios)

      v <- f %>%
            mutate(on_hull = 1:nrow(.) %in% chull(x, y)) %>%
            group_by(x,y,z) %>%
            mutate(on_hull = any(on_hull)) %>%
            group_by(facet) %>%
            mutate(far_corner = any(z == 1),
                   near_corner = any(z == 0)) %>%
            ungroup()
      v <- v %>%
            select(-facet) %>%
            filter(on_hull) %>%
            distinct() %>%
            mutate(vert = 1:nrow(.)) %>%
            full_join(v, by = join_by(x, y, z, on_hull,
                                      near_corner, far_corner))
      v
}

face_info <- function(proj, effective_ratios = c(1, 1, 1)){

      v <- vertices(proj, effective_ratios)
      h <- v %>%
            group_by(facet) %>%
            summarize(xmin = min(x), xmean = mean(x), xmax = max(x),
                      ymin = min(y), ymean = mean(y), ymax = max(y),
                      zmin = min(z), zmean = mean(z), zmax = max(z),

                      xfar = mean(x[z == min(z)]), # x' coordinate of facet corner farthest from viewer
                      xnear = mean(x[z == max(z)]),
                      yfar = mean(y[z == min(z)]),
                      ynear = mean(y[z == max(z)]),

                      nvchull = length(unique(na.omit(vert))),
                      near = any(near_corner),
                      far = any(far_corner),
                      .groups = "drop")

      # Foreground/background logic:
      # -FACE VIEW (hull has 4 corners): nearest face is fg, all others bg
      # -EDGE VIEW (hull has 6 corners, two faces have 4 hull corners): faces with 4 hull corners are fg, all others bg
      # -CORNER VIEW (hull has 6 corners, all faces have 3 hull corners):
      #       faces touching near corner are fg, faces touching far corner are bg
      view <- case_when((sum(v$on_hull) / 3) == 4 ~ "face",
                        length(unique(h$nvchull)) == 1 ~ "corner",
                        TRUE ~ "edge")

      h <- h %>%
            mutate(foreground = case_when(
                  view == "face" & zmean == min(zmean) ~ TRUE,
                  view == "edge" & nvchull == 4 ~ TRUE,
                  view == "edge" & nvchull != 4 ~ FALSE,
                  view == "corner" & zmin == min(zmin) ~ TRUE,
                  TRUE ~ FALSE))

      h %>% arrange(zmean, foreground) # rendering order
}

select_visible_faces <- function(faces_param, proj, effective_ratios = c(1, 1, 1)){

      rules <- c("all", "none", "near", "far", "background", "foreground",
                 "left", "right", "top", "bottom", "front", "back")
      facets <- c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")
      faces <- match.arg(faces_param, c(rules, facets), several.ok = TRUE)
      rules <- rules[rules %in% faces]
      facets <- facets[facets %in% faces]

      f <- face_info(proj, effective_ratios)
      rules <- sapply(rules, function(x){
            switch(x,
                   all = f$facet,
                   none = NULL,
                   near = f$facet[f$near],
                   far = f$facet[f$far],
                   background = f$facet[!f$foreground],
                   foreground = f$facet[f$foreground],
                   left = f$facet[f$xmean == min(f$xmean)][1],
                   right = f$facet[f$xmean == max(f$xmean)][1],
                   top = f$facet[f$ymean == max(f$ymean)][1],
                   bottom = f$facet[f$ymean == min(f$ymean)][1],
                   front = f$facet[f$zmean == max(f$zmean)][1],
                   back = f$facet[f$zmean == min(f$zmean)][1]
            )
      })
      faces <- unique(c(rules, facets))

      list(bg =  f$facet[f$facet %in% faces & !f$foreground],
           fg =  f$facet[f$facet %in% faces & f$foreground])
}

get_face_corners <- function(face_name) {
      # Return the 4 corners of a cube face
      if (face_name == "xmin") {
            return(data.frame(
                  x = c(-0.5, -0.5, -0.5, -0.5),
                  y = c(-0.5,  0.5, -0.5,  0.5),
                  z = c(-0.5, -0.5,  0.5,  0.5)
            ))
      } else if (face_name == "xmax") {
            return(data.frame(
                  x = c(0.5, 0.5, 0.5, 0.5),
                  y = c(-0.5, 0.5, -0.5, 0.5),
                  z = c(-0.5, -0.5, 0.5, 0.5)
            ))
      } else if (face_name == "ymin") {
            return(data.frame(
                  x = c(-0.5,  0.5, -0.5,  0.5),
                  y = c(-0.5, -0.5, -0.5, -0.5),
                  z = c(-0.5, -0.5,  0.5,  0.5)
            ))
      } else if (face_name == "ymax") {
            return(data.frame(
                  x = c(-0.5, 0.5, -0.5, 0.5),
                  y = c(0.5, 0.5, 0.5, 0.5),
                  z = c(-0.5, -0.5, 0.5, 0.5)
            ))
      } else if (face_name == "zmin") {
            return(data.frame(
                  x = c(-0.5,  0.5, -0.5,  0.5),
                  y = c(-0.5, -0.5,  0.5,  0.5),
                  z = c(-0.5, -0.5, -0.5, -0.5)
            ))
      } else if (face_name == "zmax") {
            return(data.frame(
                  x = c(-0.5, 0.5, -0.5, 0.5),
                  y = c(-0.5, -0.5, 0.5, 0.5),
                  z = c(0.5, 0.5, 0.5, 0.5)
            ))
      }
}

