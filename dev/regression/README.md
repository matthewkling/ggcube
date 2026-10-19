# Regression harness

Tools for checking that changes to ggcube's rendering pipeline don't alter
output unintentionally, and for measuring how well primitives are depth
ordered. Excluded from the package build (`.Rbuildignore`), with outputs in
`out/` (git-ignored). Run everything from the package root.

## Files

- `cases.R`: the regression cases, a named list of plot constructors tagged
  `single` (one ggcube layer), `multi` (several), `sort`, and `ann`.
- `harness.R`: installs versions, renders cases, and compares renders.
- `validate.R`: the ground-truth ordering validator.
- `oracle.R`: compares the pairwise constraints and draw orders of two
  versions on random sort-stage scenes, scoring both with the validator.
- `bench.R`: rendering benchmarks.
- `run.R`: command-line entry point.

## Typical use

```sh
# Install and render a baseline commit, and the working tree
Rscript dev/regression/run.R install 3bb6095 base
Rscript dev/regression/run.R render base
Rscript dev/regression/run.R install working working
Rscript dev/regression/run.R render working

# Pixel diff: writes out/diffs/base_vs_working/index.html
Rscript dev/regression/run.R compare base working

# Ordering validation: writes out/validation/<name>/validation.csv and error maps
Rscript dev/regression/run.R validate base
Rscript dev/regression/run.R validate working

# Benchmarks (timeout in seconds per plot): writes out/bench/<name>.csv
Rscript dev/regression/bench.R base 300
Rscript dev/regression/bench.R working 300

# Constraint and draw-order comparison on 200 random scenes (seed 1)
Rscript dev/regression/oracle.R base working 200 1

# Or render, compare, and validate in one go
Rscript dev/regression/run.R all 3bb6095
```

Every version is installed into its own library under `out/libs/`, and each
render, validation, or benchmark runs in a fresh R session with that library
first on the search path. Renders use ragg at 6 x 5 in, 100 dpi. Renders are
only comparable when made on the same machine, since fonts and graphics
libraries affect pixels.

## Pixel comparison

`compare` reports, per case, the number and fraction of changed pixels and
the largest per-channel change, and writes a diff image (unchanged pixels
greyed out, changed pixels magenta). The HTML report shows baseline, current,
and diff side by side, with any warnings or errors either render raised.

## Ordering validation

The validator casts exact lines of sight (in standardized cube space, using
the coord's projection) through a regular grid of screen samples in each
panel. At each sample it compares the primitive that is truly nearest the
viewer with the one drawn last there. Disagreements beyond a small depth
tolerance are ordering errors; it reports the fraction of covered samples in
error, the most frequent misordered pairs, and an error map of the first
panel (grey: correct, red: error).

Polygons use their best-fit plane, with errors within a polygon's warp
tolerated. Segments and point markers cover the samples within their drawn
width, and are treated as tubes and spheres of that width in depth, so a
polygon passing within that distance of them doesn't count. Text labels are
not validated. Marker and line sizes are as drawn when the installed version
provides the validation hook, and approximated otherwise, so error counts of
older versions are approximate.

The draw order and cube-space coordinates come from
`ggcube:::validation_scene()` when the installed version provides it, and
otherwise from tracing `sort_by_depth()` (which works for versions up to and
including 0.3.0.9000).
