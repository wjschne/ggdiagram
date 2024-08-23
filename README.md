
# ggdiagram

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/ggdiagram)](https://CRAN.R-project.org/package=ggdiagram)
<!-- badges: end -->

The ggdiagram package takes an object-oriented approach to making diagrams (using [S7](https://rconsortium.github.io/S7/index.html)) and plots the results using [ggplot2](https://ggplot2.tidyverse.org/).

This package is in a very preliminary state. It has some structural beams and a bit of plumbing but is not yet capable of fulfilling its eventual purpose. 

My motivation in making ggdiagram was to find a way to approach the functionality of TikZ but with the flexibility and convenience R. The ggdiagram package is built atop [S7](https://rconsortium.github.io/S7/) is integrated with [ggplot2](https://ggplot2.tidyverse.org/), making use of [ggtext](https://wilkelab.org/ggtext/), [ggforce](https://ggforce.data-imaginist.com/), and [ggarrow](https://teunbrand.github.io/ggarrow/).

The ggdiagram package is not a replacement for the standard ggplot2 functions. The underlying "grammar" of ggplot2 is well designed for visualizing data. The ggdiagram functions are designed creating a small number of objects that are interrelated. If anything, they can be thought of as extensions of ggplot2::annotate.

## Installation

You can install the development version of ggpathdiagramr like so:

``` r
remotes::install_github("wjschne/ggdiagram")
```

