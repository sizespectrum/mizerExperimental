#' Launch shiny gadget for tuning parameters
#'
#' This function has moved to the mizerEcopath package. If mizerEcopath is
#' installed, the call is forwarded automatically. Otherwise you will be
#' prompted to install mizerEcopath.
#'
#' @param ... All arguments are forwarded to `mizerEcopath::tuneParams()`.
#'
#' @return The tuned MizerParams object
#' @keywords internal
#' @export
tuneParams <- function(...) {
    ecopath_or_stop("tuneParams")
    mizerEcopath::tuneParams(...)
}

#' Tune Growth
#'
#' This function has moved to the mizerEcopath package. If mizerEcopath is
#' installed, the call is forwarded automatically. Otherwise you will be
#' prompted to install mizerEcopath.
#'
#' @param ... All arguments are forwarded to `mizerEcopath::tuneGrowth()`.
#' @return The tuned MizerParams object
#' @keywords internal
#' @export
tuneGrowth <- function(...) {
    ecopath_or_stop("tuneGrowth")
    mizerEcopath::tuneGrowth(...)
}

#' Launch a customisable shiny gadget for tuning mizer parameters
#'
#' This function has moved to the mizerEcopath package. If mizerEcopath is
#' installed, the call is forwarded automatically. Otherwise you will be
#' prompted to install mizerEcopath.
#'
#' @param ... All arguments are forwarded to `mizerEcopath::tuningGadget()`.
#' @return The tuned MizerParams object
#' @export
#' @keywords internal
tuningGadget <- function(...) {
    ecopath_or_stop("tuningGadget")
    mizerEcopath::tuningGadget(...)
}

#' Check that mizerEcopath is available and stop with a helpful message if not
#' @param fun_name Name of the function being called
#' @noRd
ecopath_or_stop <- function(fun_name) {
    if (!requireNamespace("mizerEcopath", quietly = TRUE)) {
        stop("`", fun_name, "()` has moved to the mizerEcopath package.\n",
             "Install it with:\n",
             '  remotes::install_github("gustavdelius/mizerEcopath")\n',
             "Then load it with:\n",
             "  library(mizerEcopath)",
             call. = FALSE)
    }
}
