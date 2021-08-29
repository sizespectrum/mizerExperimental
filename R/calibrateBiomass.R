#' Calibrate model biomass to observed biomass
#' 
#' Given a MizerParams object `params` for which biomass observations are
#' available for at least some species via 
#' `species_params(params)$biomass_observed`, this function returns an
#' updated MizerParams object which is rescaled so that the total biomass
#' of the observed species in the model agrees with the total observed
#' biomass. 
#' 
#' Observed biomasses usually only include individuals above a certain size.
#' This size should either be specified in 
#' `species_params(params)$biomass_cutoff` in grams, or else a cutoff size
#' of `w_mat/20` is assumed.
#' 
#' @param params A MizerParams object
#' @return A MizerParams object
#' @export
#' @examples 
#' params <- NS_params
#' species_params(params)$biomass_observed <- 
#'     c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
#' species_params(params)$biomass_cutoff <- 10
#' params2 <- calibrateBiomass(params)
#' plotBiomassVsSpecies(params2)
calibrateBiomass <- function(params) {
    if ((!("biomass_observed" %in% names(params@species_params))) ||
        all(is.na(params@species_params$biomass_observed))) {
        return(params)
    }
    no_sp <- nrow(params@species_params)
    cutoff <- params@species_params$biomass_cutoff
    # When no cutoff known, set it to 0
    if (is.null(cutoff)) cutoff <- rep(0, no_sp)
    cutoff[is.na(cutoff)] <- 0
    observed <- params@species_params$biomass_observed
    observed_total <- sum(observed, na.rm = TRUE)
    sp_observed <- which(!is.na(observed))
    model_total <- 0
    for (sp_idx in sp_observed) {
        model_total <- 
            model_total + 
            sum((params@initial_n[sp_idx, ] * params@w * params@dw)
                [params@w >= cutoff[[sp_idx]]])
    }
    scaleModel(params, factor = observed_total / model_total)
}
