
# mizerExperimental

<!-- badges: start -->

<!-- badges: end -->

This is an extension package for the mizer package
(<https://sizespectrum.org/mizer/>) that contains additional features
contributed by the mizer community. These features can then be further
improved while being used by the community. Once matured,
frequently-used features can be moved to the core mizer package.

If you have code for working with mizer that you feel might be useful to
other mizer users, please describe it in [a new issue on the issue
tracker](https://github.com/sizespectrum/mizerExperimental/issues/new)
and we’ll comment on how it can be included in this package.

## Installation

<!--
You can install the released version of mizerStarvation from [CRAN](https://CRAN.R-project.org) with: 
``` r 
install.packages("mizerStarvation") 
```
-->

First you need a recent development version of the mizer package
(version \>= 1.0.1.9001). You can install this from GitHub with

``` r
devtools::install_github("sizespectrum/mizer")
```

Then you can install the development version of mizerExperimental from
GitHub with

``` r
devtools::install_github("sizespectrum/mizerExperimental")
```

## Features

  - Animated plot of mizer simulation with `animateSpectra()`

  - Showing two plots side-by-side with the same axes and the same
    legend. This is made possible for some of the plots via the
    `displayFrames()` function.

  - New wrapper function `newSheldonParams()` that sets up a single
    species in a power-law background.

  - Functions and a shiny app for interactively tuning parameters.
