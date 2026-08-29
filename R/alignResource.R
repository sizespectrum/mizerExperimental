#' Rescale resource to be in line with fish community
#' 
#' This function sets the coefficient of the resource power law so that the
#' resource spectrum aligns approximately with the fish community spectrum and it
#' then rescales the search volumes of all fish species so that their resource
#' encounter rate stays the same. This will therefore modify the encounter rate
#' with other prey.
#'
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @param params A MizerParams object
#' @param ... Not used.
#' @return A MizerParams object with updated resource carrying capacity,
#'   resource initial value, resource parameter lambda and species parameter
#'   gamma.
#' @export
alignResource <- function(params, ...) UseMethod("alignResource")

#' @export
alignResource.MizerParams <- function(params, ...) {
    
    sc <- colSums(params@initial_n) * 
        params@w ^ params@resource_params$lambda
    min_w_mat <- min(params@species_params$w_mat)
    max_w_mat <- max(params@species_params$w_mat)
    sel <- (params@w > min_w_mat) & (params@w < max_w_mat)
    factor <- sum(sc[sel]) / sum(sel) / params@resource_params$kappa 
    
    params@cc_pp <- params@cc_pp * factor
    params@resource_params$kappa <- params@resource_params$kappa * factor
    params@initial_n_pp <- params@initial_n_pp * factor
    params@search_vol = params@search_vol / factor

    # The rescaled `gamma` has to be recorded as a given species parameter,
    # otherwise the next recalculation of the species parameters would put the
    # old value back and silently undo the rescaling. `recalculate = FALSE`
    # records it without rebuilding the rates, because the search volume has
    # already been rescaled by hand above. This is the same treatment that
    # `scaleModel()` received in mizer 3.4.
    sp <- params@species_params
    sp$gamma <- sp$gamma / factor
    species_params(params, recalculate = FALSE) <- sp

    params
}