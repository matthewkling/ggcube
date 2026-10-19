# Regression harness: render cases with a given version of ggcube and compare
# renders pixel by pixel
#
# Each version is installed into its own library under `out/libs/`, and every
# render runs in a fresh R session (via callr) with that library first on the
# path, so versions never mix within one session.

harness_dir <- function() {
      d <- getOption("ggcube.regression.dir")
      if (is.null(d)) d <- file.path(getwd(), "dev", "regression")
      normalizePath(d, mustWork = TRUE)
}

repo_root <- function() normalizePath(file.path(harness_dir(), "..", ".."))

out_dir <- function(...) {
      d <- file.path(harness_dir(), "out", ...)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      d
}


# Installing versions -----------------------------------------------------------

#' Install a version of ggcube into its own library
#'
#' @param ref A git ref (commit, branch, or tag), or "working" for the
#'   working tree as it is on disk.
#' @param name Name for the library and render set.
#' @return Path to the library.
install_version <- function(ref = "working", name = ref) {
      lib <- out_dir("libs", name)
      src <- repo_root()
      tmp <- NULL
      if (!identical(ref, "working")) {
            tmp <- tempfile("ggcube-src-")
            dir.create(tmp)
            archive <- tempfile(fileext = ".tar")
            status <- system2("git", c("-C", shQuote(src), "archive", "--format=tar",
                                       "-o", shQuote(archive), shQuote(ref)))
            if (status != 0) stop("git archive failed for ref ", ref)
            utils::untar(archive, exdir = tmp)
            src <- tmp
      }
      status <- system2(file.path(R.home("bin"), "R"),
                        c("CMD", "INSTALL", "--no-docs", "--no-multiarch",
                          paste0("--library=", shQuote(lib)), shQuote(src)),
                        stdout = file.path(lib, "install.log"), stderr = file.path(lib, "install.log"))
      if (!is.null(tmp)) unlink(tmp, recursive = TRUE)
      if (status != 0) stop("Installing ", ref, " failed; see ", file.path(lib, "install.log"))
      writeLines(c(ref, if (!identical(ref, "working")) {
            system2("git", c("-C", shQuote(repo_root()), "rev-parse", shQuote(ref)), stdout = TRUE)
      }), file.path(lib, "REF"))
      lib
}


# Rendering ---------------------------------------------------------------------

#' Render every case with one installed version
#'
#' @param name Name of an installed version (see `install_version()`).
#' @param cases Optional character vector of case names to render.
#' @param width,height,res Device size in inches and resolution.
#' @return Data frame of per-case timings and conditions (also written to
#'   `manifest.csv` in the render directory).
render_version <- function(name, cases = NULL, width = 6, height = 5, res = 100) {
      lib <- out_dir("libs", name)
      dir <- out_dir("renders", name)
      cases_file <- file.path(harness_dir(), "cases.R")

      callr::r(function(lib, dir, cases_file, cases, width, height, res) {
            .libPaths(c(lib, .libPaths()))
            suppressPackageStartupMessages({
                  library(ggplot2)
                  library(ggcube)
            })
            source(cases_file, local = TRUE)
            all <- regression_cases()
            if (!is.null(cases)) all <- all[intersect(cases, names(all))]

            rows <- lapply(names(all), function(nm) {
                  warn <- character(0)
                  err <- NA_character_
                  file <- file.path(dir, paste0(nm, ".png"))
                  t <- system.time(withCallingHandlers(
                        tryCatch({
                              p <- all[[nm]]$plot()
                              ragg::agg_png(file, width = width, height = height,
                                            units = "in", res = res, background = "white")
                              on.exit(grDevices::dev.off(), add = TRUE)
                              print(p)
                        }, error = function(e) err <<- conditionMessage(e)),
                        warning = function(w) {
                              warn <<- c(warn, conditionMessage(w))
                              invokeRestart("muffleWarning")
                        }))[["elapsed"]]
                  data.frame(case = nm, tags = paste(all[[nm]]$tags, collapse = " "),
                             seconds = round(t, 3), warnings = paste(unique(warn), collapse = " | "),
                             error = err, stringsAsFactors = FALSE)
            })
            out <- do.call(rbind, rows)
            utils::write.csv(out, file.path(dir, "manifest.csv"), row.names = FALSE)
            out
      }, args = list(lib = lib, dir = dir, cases_file = cases_file, cases = cases,
                     width = width, height = height, res = res),
      show = FALSE)
}


# Comparing ---------------------------------------------------------------------

#' Compare two render sets pixel by pixel
#'
#' Writes a diff image per case (unchanged pixels greyed out, changed pixels
#' in magenta), a `summary.csv`, and an `index.html` showing each case's
#' baseline, current render, and diff side by side.
#'
#' @param base,current Names of render sets.
#' @param tol Per-channel tolerance (0-1) below which a pixel counts as
#'   unchanged. The default counts any change.
#' @return Summary data frame, worst cases first.
compare_versions <- function(base, current, tol = 0) {
      bdir <- out_dir("renders", base)
      cdir <- out_dir("renders", current)
      ddir <- out_dir("diffs", paste0(base, "_vs_", current))

      bman <- utils::read.csv(file.path(bdir, "manifest.csv"), stringsAsFactors = FALSE)
      cman <- utils::read.csv(file.path(cdir, "manifest.csv"), stringsAsFactors = FALSE)
      cases <- union(bman$case, cman$case)

      rows <- lapply(cases, function(nm) {
            bf <- file.path(bdir, paste0(nm, ".png"))
            cf <- file.path(cdir, paste0(nm, ".png"))
            res <- data.frame(case = nm, status = NA_character_, changed_px = NA_integer_,
                              changed_frac = NA_real_, max_delta = NA_real_,
                              base_s = bman$seconds[match(nm, bman$case)],
                              current_s = cman$seconds[match(nm, cman$case)],
                              stringsAsFactors = FALSE)
            if (!file.exists(bf) || !file.exists(cf)) {
                  res$status <- if (!file.exists(bf)) "new" else "missing"
                  return(res)
            }
            a <- png::readPNG(bf)[, , 1:3, drop = FALSE]
            b <- png::readPNG(cf)[, , 1:3, drop = FALSE]
            if (!identical(dim(a), dim(b))) {
                  res$status <- "size differs"
                  return(res)
            }
            delta <- apply(abs(a - b), c(1, 2), max)
            changed <- delta > tol
            res$changed_px <- sum(changed)
            res$changed_frac <- mean(changed)
            res$max_delta <- max(delta)
            res$status <- if (any(changed)) "changed" else "identical"

            grey <- 0.3 * a[, , 1] + 0.59 * a[, , 2] + 0.11 * a[, , 3]
            img <- array(0.6 + 0.4 * grey, dim = dim(a))
            img[, , 1][changed] <- 1
            img[, , 2][changed] <- 0
            img[, , 3][changed] <- 1
            png::writePNG(img, file.path(ddir, paste0(nm, ".png")))
            res
      })
      summary <- do.call(rbind, rows)
      summary <- summary[order(-ifelse(is.na(summary$changed_px), Inf, summary$changed_px)), ]
      utils::write.csv(summary, file.path(ddir, "summary.csv"), row.names = FALSE)
      write_diff_index(summary, base, current, ddir, bman, cman)
      summary
}

write_diff_index <- function(summary, base, current, ddir, bman, cman) {
      rel <- function(set, nm) file.path("..", "..", "renders", set, paste0(nm, ".png"))
      esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x))
      rows <- vapply(seq_len(nrow(summary)), function(i) {
            s <- summary[i, ]
            note <- c(bman$warnings[match(s$case, bman$case)], cman$warnings[match(s$case, cman$case)],
                      bman$error[match(s$case, bman$case)], cman$error[match(s$case, cman$case)])
            note <- note[!is.na(note) & nzchar(note)]
            sprintf(paste0(
                  '<section class="%s"><h2>%s <span>%s, %s px (%.3f%%), %.2fs &rarr; %.2fs</span></h2>',
                  '%s<div class="row"><figure><img src="%s"><figcaption>%s</figcaption></figure>',
                  '<figure><img src="%s"><figcaption>%s</figcaption></figure>',
                  '<figure><img src="%s.png"><figcaption>diff</figcaption></figure></div></section>'),
                  s$status, s$case, s$status, format(s$changed_px), 100 * (s$changed_frac %||% 0),
                  s$base_s %||% NA, s$current_s %||% NA,
                  if (length(note)) paste0('<p class="note">', esc(paste(note, collapse = " | ")), "</p>") else "",
                  rel(base, s$case), base, rel(current, s$case), current, s$case)
      }, character(1))
      html <- c('<!doctype html><meta charset="utf-8"><title>ggcube regression</title>',
                '<style>body{font-family:sans-serif;margin:16px}section{margin-bottom:24px}',
                'h2{font-size:16px}h2 span{font-weight:normal;color:#666}',
                '.row{display:flex;gap:8px;flex-wrap:wrap}figure{margin:0}',
                'img{width:360px;border:1px solid #ccc}figcaption{font-size:12px;color:#666}',
                '.identical h2{color:#2a7}.changed h2{color:#c2185b}.note{color:#a60;font-size:12px}',
                '#hide:checked~section.identical{display:none}</style>',
                sprintf('<h1>%s vs %s</h1>', base, current),
                '<input type="checkbox" id="hide" checked> <label for="hide">hide identical</label>',
                rows)
      writeLines(html, file.path(ddir, "index.html"))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
