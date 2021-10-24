# The following is a copy of the code for `matchBiomasses()` just with
# the text replacements "Biomass" -> "Number" and "biomass" to "number" and
# the removal of the `params@w` factor in the calculations.

#' Match numbers to observations
#' 
#' `r lifecycle::badge("experimental")`
#' The function adjusts the numbers of the species in the model so that their numbers
#' match with observations. 
#' 
#' The function works by multiplying for each species the number density
#' at all sizes by the same factor. This will of course not give a steady
#' state solution, even if the initial number densities were at steady state.
#' So after using this function you may want to use `steady()` to run the model 
#' to steady state, after which of course the numbers will no longer match
#' exactly. You could then iterate this process. This is described in the
#' blog post at https://bit.ly/2YqXESV.
#' 
#' Before you can use this function you will need to have added a
#' `number_observed` column to your model which gives the observed number in
#' grams.  For species for which you have no observed number, you should set
#' the value in the `number_observed` column to 0 or NA.
#'
#' Number observations usually only include individuals above a certain size.
#' This size should be specified in a `number_cutoff` column of the species
#' parameter data frame. If this is missing, it is assumed that all sizes are
#' included in the observed number, i.e., it includes larval number.
#' 
#' @param params A MizerParams object
#' @param species The species to be affected. Optional. By default all observed
#'   numbers will be matched. A vector of species names, or a numeric vector
#'   with the species indices, or a logical vector indicating for each species
#'   whether it is to be affected (TRUE) or not.
#' @return A MizerParams object
#' @export
#' @examples 
#' params <- NS_params
#' species_params(params)$number_observed <- 
#'     c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
#' species_params(params)$number_cutoff <- 10
#' params <- calibrateNumber(params)
#' params <- matchNumbers(params)
#' plotNumberObservedVsModel(params)
matchNumbers <- function(params, species = NULL) {
    if (!("number_observed" %in% names(params@species_params))) {
        return(params)
    }
    species <- valid_species_arg(params, species = species, 
                                 return.logical = TRUE) &
        !is.na(params@species_params$number_observed) &
        params@species_params$number_observed > 0
    for (sp in (1:nrow(params@species_params))[species]) {
        cutoff <- params@species_params$number_cutoff[[sp]]
        if (is.null(cutoff) || is.na(cutoff)) {
            cutoff <- 0
        }
        total <- sum((params@initial_n[sp, ] * params@dw)
                     [params@w >= cutoff])
        factor <- params@species_params$number_observed[[sp]] / total
        params@initial_n[sp, ] <- params@initial_n[sp, ] * factor
    }
    
    setBevertonHolt(params)
}