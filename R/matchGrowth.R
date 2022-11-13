#' Adjust model to produce observed growth
#'
#' Scales the search volume, the maximum consumption rate and the metabolic rate
#' all by the same factor in order to achieve a growth rate that allows
#' individuals to reach their maturity size by their maturity age while keeping
#' the feeding level and the critical feeding level unchanged.
#'
#' Maturity size and age are taken from the `w_mat` and `age_mat` columns in the
#' species_params data frame. If `age_mat` is missing, mizer calculates it from
#' the von Bertalanffy growth curve parameters using `age_mat_vB()`. If those
#' are not available either for a species, the growth rate for that species will
#' not be changed.
#'
#' @param params A MizerParams object
#' @param species The species to be affected. Optional. By default all species for
#'   which growth information is available will be affected. A vector of species
#'   names, or a numeric vector with the species indices, or a logical vector
#'   indicating for each species whether it is to be affected (TRUE) or not.
#' @return A modified MizerParams object with rescaled search volume, maximum
#'   consumption rate and metabolic rate and rescaled species parameters
#'   `gamma`,`h`, `ks` and `k`.
#' @export
matchGrowth <- function(params, species = NULL) {
    assert_that(is(params, "MizerParams"))
    sel <- valid_species_arg(params, species = species, 
                                 return.logical = TRUE)
    sp <- params@species_params
    
    # If age at maturity is not specified, calculate it from von Bertlanffy
    sp <- set_species_param_default(sp, "age_mat", age_mat_vB(params))
    # Don't affect species where no age at maturity is available
    sel <- sel & !is.na(sp$age_mat)
    
    factor <- age_mat(params)[sel] / sp$age_mat[sel]

    params@search_vol[sel, ] <- params@search_vol[sel, ] * factor
    params@intake_max[sel, ] <- params@intake_max[sel, ] * factor
    params@metab[sel, ] <- params@metab[sel, ] * factor
    params@species_params$gamma[sel] <- sp$gamma[sel] * factor
    params@species_params$h[sel] <- sp$h[sel] * factor
    if ("ks" %in% names(sp)) {
        params@species_params$ks[sel] <- sp$ks[sel] * factor
    }
    if ("k" %in% names(sp)) {
        params@species_params$k[sel] <- sp$k[sel] * factor
    }
    
    setBevertonHolt(params)
}