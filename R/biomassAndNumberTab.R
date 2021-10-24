#' Number tab for tuning gadget
#' 
#' The Number tab shows:
#' * A plot of total number for each species, compared to
#' observed numbers when available, using [plotNumberObservedVsModel()].
#' * Buttons "Calibrate" and "Match" that trigger a call to 
#' [calibrateNumber()] or [matchNumbers()] respectively.
#' 
#' Clicking on a species in the number plot makes that species the selected
#' species. Double-clicking on a species selects that species __and__
#' changes its number.
#' @inheritParams spectraTab
biomassAndNumberTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    biomassTab(input, output, session,
               params, logs, trigger_update, ...)
    numberTab(input, output, session,
               params, logs, trigger_update, ...)
}

#' @rdname biomassAndNumberTab
biomassAndNumberTabUI <- function(params, ...) {
    tagList(h1("Biomasses"),
            biomassTabUI(params, ...),
            h1("Numbers"),
            numberTabUI(params, ...))
}
