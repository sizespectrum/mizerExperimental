#' Abundance tab for tuning gadget
#' 
#' The Abundance tab shows:
#' * A plot of total abundance for each species, compared to
#' observed abundances when available, using [plotAbundanceObservedVsModel()].
#' * Buttons "Calibrate" and "Match" that trigger a call to 
#' [calibrateAbundance()] or [matchAbundances()] respectively.
#' 
#' Clicking on a species in the abundance plot makes that species the selected
#' species. Double-clicking on a species selects that species __and__
#' changes its abundance.
#' @inheritParams spectraTab
biomassAndAbundanceTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    biomassTab(input, output, session,
               params, logs, trigger_update, ...)
    abundanceTab(input, output, session,
               params, logs, trigger_update, ...)
}

#' @rdname biomassAndAbundanceTab
biomassAndAbundanceTabUI <- function(params, ...) {
    tagList(h1("Biomass"),
            biomassTabUI(params, ...),
            h1("Abundance"),
            abundanceTabUI(params, ...))
}
