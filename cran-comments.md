## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* The package uses `ggplot2:::default_expansion` and
  `ggplot2:::expand_limits_scale` from the `ggplot2` internals. These
  are needed because `coord_3d()` extends `CoordCartesian` and must
  reproduce ggplot2's exact scale expansion behavior to ensure
  consistent axis limits between 2D and 3D coordinate systems.
  Reimplementing this logic in the package would create a risk of
  silent drift from ggplot2's behavior across versions, with no
  user-visible benefit. The ggplot2 extension documentation notes
  that coord extensions may require access to internal coordinate
  system functionality.
  
## Test environments

* GitHub Actions (ubuntu-latest): R-release, R-oldrel
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macos-latest): R-release
* GitHub Actions with _R_CHECK_DEPENDS_ONLY_=true: R-release
