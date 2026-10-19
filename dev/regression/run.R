# Command-line entry point for the regression harness. Run from the package
# root:
#
#   Rscript dev/regression/run.R install <ref|working> [name]
#   Rscript dev/regression/run.R render <name> [case ...]
#   Rscript dev/regression/run.R compare <base> <current>
#   Rscript dev/regression/run.R validate <name> [case ...]
#   Rscript dev/regression/run.R all <base-ref> [current-ref]
#
# `all` installs and renders both versions (current defaults to the working
# tree), compares them, and runs the ordering validator on both. Outputs go
# to dev/regression/out/: renders/<name>/, diffs/<base>_vs_<current>/index.html,
# and validation/<name>.csv.

source(file.path("dev", "regression", "harness.R"))
source(file.path("dev", "regression", "validate.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: see the header of dev/regression/run.R")
cmd <- args[1]
rest <- args[-1]

show <- function(df) print(df, row.names = FALSE, right = FALSE)

switch(cmd,
       install = {
             ref <- rest[1]
             name <- if (length(rest) > 1) rest[2] else ref
             cat("Installed", ref, "into", install_version(ref, name), "\n")
       },
       render = {
             show(render_version(rest[1], cases = if (length(rest) > 1) rest[-1]))
       },
       compare = {
             s <- compare_versions(rest[1], rest[2])
             show(s)
             cat("\nIdentical:", sum(s$status == "identical"), "of", nrow(s),
                 "\nReport:", file.path(out_dir("diffs", paste0(rest[1], "_vs_", rest[2])), "index.html"), "\n")
       },
       validate = {
             show(validate_version(rest[1], cases = if (length(rest) > 1) rest[-1]))
       },
       all = {
             base_ref <- rest[1]
             cur_ref <- if (length(rest) > 1) rest[2] else "working"
             base <- paste0("base-", substr(base_ref, 1, 12))
             cur <- if (cur_ref == "working") "working" else paste0("cur-", substr(cur_ref, 1, 12))
             install_version(base_ref, base)
             install_version(cur_ref, cur)
             render_version(base)
             render_version(cur)
             s <- compare_versions(base, cur)
             show(s)
             vb <- validate_version(base)
             vc <- validate_version(cur)
             v <- merge(vb[c("case", "error_frac")], vc[c("case", "error_frac")],
                        by = "case", suffixes = c(".base", ".current"), all = TRUE)
             cat("\nOrdering errors (fraction of covered samples):\n")
             show(v)
       },
       stop("Unknown command: ", cmd))
