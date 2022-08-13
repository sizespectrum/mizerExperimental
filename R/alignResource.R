#' Rescale resource to be in line with fish community
#' 
#' This function sets the coefficient of the resource power law so that the
#' resource spectrum aligns approximatelywith the fish community spectrum and it
#' then rescales the search volumes of all fish species so that their resource
#' consumption stays the same.
#' 
#' @param params A MizerParams object
#' @return A MizerParams object with updated resource carrying capacity,
#'   resource initial value, resource parameter lambda and species parameter
#'   gamma.
#' @export
alignResource <- function(params) {
    
    sc <- colSums(params@initial_n) * 
        params@w ^ params@resource_params$lambda
    min_w_mat = min(params@species_params$w_mat)
    max_w_mat = max(params@species_params$w_mat)
    sel <- (params@w > min_w_mat) & (params@w < max_w_mat)
    factor <- sum(sc[sel]) / sum(sel) / params@resource_params$kappa 
    
    params@cc_pp <- params@cc_pp * factor
    params@resource_params$kappa <- params@resource_params$kappa * factor
    params@initial_n_pp <- params@initial_n_pp * factor
    params@search_vol = params@search_vol / factor
    params@species_params$gamma <- params@species_params$gamma / factor
    
    params
}