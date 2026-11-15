## Submission summary

This is a minor release (0.2.0 -> 0.3.0). It

* restores compatibility with recent and forthcoming versions of S7
  (contributed by Tomasz Kalinowski), and
* adds color gradients for arrows, plus two new exported functions,
  `geom_arrow_segment_gradient()` and `geom_arrow_gradient()`.

The minimum R version is now 4.4.0, because the package uses base R's `%||%`.

## Test environments

* Local: Windows 11 x64, R 4.6.2, S7 0.2.2
* win-builder: R-devel Status: OK
* R-hub: macOS (arm64 and x86_64), R-release: Status: OK

## R CMD check results

0 errors | 0 warnings | 0 notes

* On some platforms, the installed package size may be reported as about 10 Mb
  (the R directory is about 9 Mb). This comes from the number of S7 classes and
  methods the package defines. It contains no data or compiled code.

## Reverse dependencies

We checked 0 reverse dependencies (revdepcheck). There are currently no reverse
dependencies on CRAN.
