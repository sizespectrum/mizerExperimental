#' Match biomasses to observations
#' 
#' @param params A MizerParams object
#' @param species The species to be affected. Optional. By default all observed
#'   biomasses will be matched. A vector of species names, or a numeric vector
#'   with the species indices, or a logical vector indicating for each species
#'   whether it is to be affected (TRUE) or not.
#' @return A MizerParams object
#' @export
#' @examples 
#' params <- NS_params
#' species_params(params)$biomass_observed <- 
#'     c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
#' species_params(params)$biomass_cutoff <- 10
#' params <- calibrateBiomass(params)
#' params <- matchBiomass(params)
#' plotBiomassVsSpecies(params)
matchBiomasses <- function(params, species = NULL) {
    if (!("biomass_observed" %in% names(params@species_params))) {
        return(params)
    }
    species <- valid_species_arg(params, species = species, 
                                 return.logical = TRUE) &
        !is.na(params@species_params$biomass_observed) &
        params@species_params$biomass_observed > 0
    for (sp in (1:nrow(params@species_params))[species]) {
        cutoff <- params@species_params$biomass_cutoff[[sp]]
        if (is.null(cutoff) || is.na(cutoff)) {
            cutoff <- params@species_params$w_mat[[sp]] / 20
        }
        total <- sum((params@initial_n[sp, ] * params@w * params@dw)
                     [params@w >= cutoff])
        n0_old <- params@initial_n[sp, params@w_min_idx[[sp]]]
        n0 <- n0_old * params@species_params$biomass_observed[[sp]] / total
        # rescale abundance to new egg density
        params@initial_n[sp, ] <- params@initial_n[sp, ] * n0 / n0_old
    }
    params
}