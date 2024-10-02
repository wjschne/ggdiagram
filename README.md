
# ggdiagram <a href="https://wjschne.github.io/ggdiagram/"><img src="man/figures/logo.png" align="right" height="120" alt="ggdiagram website" /></a>


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/ggdiagram)](https://CRAN.R-project.org/package=ggdiagram)
<!-- badges: end -->

The ggdiagram package takes an object-oriented approach to making diagrams and plots the results using [ggplot2](https://ggplot2.tidyverse.org/).

This package is in the early stages of construction. It has some structural beams and a bit of plumbing but is not yet capable of fulfilling its eventual purpose. 

My motivation in making ggdiagram was to find a way to approach the functionality of TikZ but with the flexibility and convenience of R. The ggdiagram package is built atop [S7](https://rconsortium.github.io/S7/) and is integrated with [ggplot2](https://ggplot2.tidyverse.org/), making heavy use of [ggtext](https://wilkelab.org/ggtext/), [ggforce](https://ggforce.data-imaginist.com/), and [ggarrow](https://teunbrand.github.io/ggarrow/).

The ggdiagram package is not a replacement for the standard ggplot2 functions. The underlying *grammar* of ggplot2 is well designed for visualizing data. The ggdiagram functions are best suited for creating a small number of visual objects that interrelate. If anything, they can be thought of as extensions of `ggplot2::annotate`.

## Installation

You can install the development version of ggpathdiagramr like so:

``` r
remotes::install_github("wjschne/ggdiagram")
```

