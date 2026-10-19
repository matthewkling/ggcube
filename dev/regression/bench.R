# Rendering benchmarks
#
# Times the full render of each benchmark plot (build, gtable, and drawing to
# an off-screen PNG device, so that draw-time work is included) with one
# installed version of ggcube. Each plot runs in a fresh R session with a
# timeout; plots that exceed it are reported as timed out. Memory is R's peak
# heap use during the first render (`gc()` "max used"), which covers
# allocations made by R but not by compiled code.
#
#   Rscript dev/regression/bench.R <name> [timeout_seconds] [case ...]
#
# Results are written to out/bench/<name>.csv.

if (!exists("out_dir")) source(file.path("dev", "regression", "harness.R"))

bench_cases <- function() {
      cloud <- function(n) {
            set.seed(1)
            data.frame(x = runif(n), y = runif(n), z = runif(n))
      }
      surf <- function(n) {
            d <- expand.grid(x = seq(-2, 2, length.out = n), y = seq(-2, 2, length.out = n))
            d$z <- with(d, sin(x * 1.3) * cos(y))
            d
      }
      points <- function(n, method) function() {
            ggplot(cloud(n), aes(x, y, z)) + geom_point_3d(sort_method = method) + coord_3d()
      }
      list(
            points_300_painter = points(300, "painter"),
            points_300_pairwise = points(300, "pairwise"),
            points_1k_painter = points(1000, "painter"),
            points_1k_pairwise = points(1000, "pairwise"),
            points_3k_painter = points(3000, "painter"),
            points_3k_pairwise = points(3000, "pairwise"),
            points_10k_painter = points(10000, "painter"),
            points_10k_pairwise = points(10000, "pairwise"),

            points_30k = points(30000, "pairwise"),
            points_10k_size4 = function() {
                  ggplot(cloud(10000), aes(x, y, z)) + geom_point_3d(size = 4) + coord_3d()
            },
            path_5k = function() {
                  t <- seq(0, 40 * pi, length.out = 5000)
                  ggplot(data.frame(x = sin(t), y = cos(t), z = t), aes(x, y, z)) +
                        geom_path_3d() + coord_3d()
            },
            voxel_15 = function() {
                  set.seed(3)
                  v <- expand.grid(x = 1:15, y = 1:15, z = 1:15)
                  v <- v[runif(nrow(v)) < 0.3, ]
                  ggplot(v, aes(x, y, z)) + geom_voxel_3d() + coord_3d()
            },
            points_reflines_plane_300 = function() {
                  ggplot(cloud(300), aes(x, y, z)) +
                        geom_point_3d(ref_lines = TRUE, sort_method = "pairwise",
                                      annotate = annotate_3d("plane", zintercept = 0.5, alpha = 0.6)) +
                        coord_3d()
            },

            surface_30_planes_pairwise = function() {
                  ggplot(surf(30), aes(x, y, z)) +
                        geom_surface_3d(sort_method = "pairwise",
                                        annotate = list(annotate_3d("plane", zintercept = 0),
                                                        annotate_3d("plane", xintercept = 0.5))) +
                        coord_3d()
            },
            surface_50_painter = function() {
                  ggplot(surf(50), aes(x, y, z)) + geom_surface_3d(sort_method = "painter") + coord_3d()
            },
            surface_50_pairwise = function() {
                  ggplot(surf(50), aes(x, y, z)) + geom_surface_3d(sort_method = "pairwise") + coord_3d()
            },
            surface_100_painter = function() {
                  ggplot(surf(100), aes(x, y, z)) + geom_surface_3d(sort_method = "painter") + coord_3d()
            },
            surface_100_pairwise = function() {
                  ggplot(surf(100), aes(x, y, z)) + geom_surface_3d(sort_method = "pairwise") + coord_3d()
            },

            multi_matlib_diagram = function() {
                  vecs <- data.frame(x = 0, y = 0, z = 0, xend = c(1, 0, 0.6), yend = c(0, 1, 0.6),
                                     zend = c(0, 0, 1), v = c("a", "b", "c"))
                  tri <- data.frame(x = c(1, 0, 0.6), y = c(0, 1, 0.6), z = c(0, 0, 1))
                  ggplot() +
                        geom_polygon_3d(data = tri, aes(x, y, z), fill = "gold",
                                        annotate = annotate_3d("plane", zintercept = 0.4)) +
                        geom_segment_3d(data = vecs, aes(x, y, z, xend = xend, yend = yend, zend = zend),
                                        arrow = grid::arrow()) +
                        geom_text_3d(data = transform(vecs, x = xend, y = yend, z = zend),
                                     aes(x, y, z, label = v)) +
                        coord_3d()
            },
            multi_surface_30_scatter_300 = function() {
                  set.seed(2)
                  pts <- data.frame(x = runif(300, -2, 2), y = runif(300, -2, 2))
                  pts$z <- with(pts, sin(x * 1.3) * cos(y)) + rnorm(300, 0, 0.4)
                  ggplot(surf(30), aes(x, y, z)) +
                        geom_surface_3d() +
                        geom_point_3d(data = pts, colour = "red") +
                        coord_3d()
            }
      )
}

bench_version <- function(name, cases = NULL, timeout = 300, reps = 3) {
      lib <- out_dir("libs", name)
      all <- names(bench_cases())
      if (!is.null(cases)) all <- intersect(cases, all)
      bench_file <- file.path(harness_dir(), "bench.R")
      harness_file <- file.path(harness_dir(), "harness.R")

      rows <- lapply(all, function(nm) {
            res <- tryCatch(callr::r(function(lib, nm, bench_file, harness_file, reps) {
                  .libPaths(c(lib, .libPaths()))
                  suppressPackageStartupMessages({ library(ggplot2); library(ggcube) })
                  source(harness_file, local = TRUE)
                  source(bench_file, local = TRUE)
                  make <- bench_cases()[[nm]]
                  run <- function() {
                        f <- tempfile(fileext = ".png")
                        ragg::agg_png(f, width = 6, height = 5, units = "in", res = 100)
                        on.exit({ grDevices::dev.off(); unlink(f) })
                        suppressWarnings(print(make()))
                  }
                  gc(reset = TRUE)
                  t1 <- system.time(run())[["elapsed"]]
                  peak <- gc()[, 6]
                  times <- t1
                  if (t1 < 5 && reps > 1) {
                        for (r in seq_len(reps - 1)) times <- c(times, system.time(run())[["elapsed"]])
                  }
                  data.frame(seconds = round(stats::median(times), 3),
                             peak_mb = round(sum(peak), 1),
                             status = "ok")
            }, args = list(lib = lib, nm = nm, bench_file = bench_file,
                           harness_file = harness_file, reps = reps), timeout = timeout),
            error = function(e) {
                  data.frame(seconds = NA_real_, peak_mb = NA_real_,
                             status = if (inherits(e, "callr_timeout_error")) paste0("timeout >", timeout, "s")
                                      else paste("error:", conditionMessage(e)))
            })
            cbind(case = nm, res)
      })
      out <- do.call(rbind, rows)
      utils::write.csv(out, file.path(out_dir("bench"), paste0(name, ".csv")), row.names = FALSE)
      out
}

if (sys.nframe() == 0) {
      args <- commandArgs(trailingOnly = TRUE)
      timeout <- if (length(args) > 1) as.numeric(args[2]) else 300
      cases <- if (length(args) > 2) args[-(1:2)] else NULL
      print(bench_version(args[1], cases = cases, timeout = timeout), row.names = FALSE)
}
