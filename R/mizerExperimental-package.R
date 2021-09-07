#' mizerExperimental: Extends the mizer package with experimental features
#'
#' @description
#' This is an extension package for the
#' [mizer package](https://sizespectrum.org/mizer/)
#' that contains additional features
#' contributed by the mizer community. These features can then be further
#' improved while being used by the community. Once matured, frequently-used
#' features can be moved to the core mizer package.
#'
#' If you have code for working with mizer that you feel might be useful to
#' other mizer users, please describe it in
#' [a new issue on the issue tracker](https://github.com/sizespectrum/mizerExperimental/issues/new)
#' and we'll comment on how it can be included in this package.
#'
#' @import mizer methods dplyr ggplot2 assertthat shiny rintrojs
#' @importFrom stats runif
#' @importFrom utils globalVariables
#' @rawNamespace import(plotly, except = last_plot)
#' @md
#' @keywords internal
"_PACKAGE"


globalVariables(c("expect_equal", "other", "yield"))
