#' Abundance tab for tuning gadget
#' 
#' The Abundance tab combines the [biomassTab()] and the [numberTab()] 
#' on a single tab.
#' @inheritParams spectraTab
abundanceTab <- function(input,  ...) {
    biomassTab(input, ...)
    numberTab(input, ...)
}

#' @rdname abundanceTab
abundanceTabUI <- function(params, ...) {
    tagList(h1("Biomasses"),
            biomassTabUI(params, ...),
            h1("Numbers"),
            numberTabUI(params, ...))
}
