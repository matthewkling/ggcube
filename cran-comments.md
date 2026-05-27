## Resubmission Notes

This is a resubmission of ggcube. I have made the following changes to
address issues flagged in the previous review:

* All `\dontrun{}` tags have been changed to `\donttest{}`.

* All functions now have a return `\value` tag.

* The `file_renderer_3d()` function now writes to `tempdir()` by 
default instead of `"."`.

The resubmission also includes a few minor bugfixes, and a refactor that
avoids importing internal ggplot2 functions.


## R CMD check results

0 errors | 0 warnings | 1 note

* New submission


## Test environments

* Local: macOS, R-release
* win-builder: R-devel
* GitHub Actions (ubuntu-latest): R-release, R-oldrel
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macos-latest): R-release
* GitHub Actions with _R_CHECK_DEPENDS_ONLY_=true: R-release
