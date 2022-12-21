#' Set initial abundances to single-species steady state abundances
#'
#' `r lifecycle::badge("deprecated")` This function has been renamed to
#' [mizer::steadySingleSpecies()].
#' 
#' This first calculates growth and death rates that arise from the current
#' initial abundances. Then it uses these growth and death rates to 
#' determine the steady-state abundances of the selected species. 
#' 
#' The result of applying this function is of course not a multi-species steady
#' state, because after changing the abundances of the selected species the
#' growth and death rates will have changed.
#' 
#' @param params A MizerParams object
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a numeric vector with
#'   the species indices, or a logical vector indicating for each species
#'   whether it is to be selected (TRUE) or not.
#' @param keep A string determining which quantity is to be kept constant. The
#'   choices are "egg" which keeps the egg density constant, "biomass" which 
#'   keeps the total biomass of the species constant and "number" which keeps
#'   the total number of individuals constant.
#' @return A MizerParams object in which the initial abundances of the selected
#'   species are changed to their single-species steady state abundances.
#' @keywords internal
#' @export
singleSpeciesSteady <- function(params, species = NULL,
                                keep = c("egg", "biomass", "number")) {
    
    lifecycle::deprecate_warn("2.4.0", "singleSpeciesSteady()", 
                              "mizer::steadySingleSpecies()")
    steadySingleSpecies(params, species, keep)
}