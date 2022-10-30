#' Calculate age at maturity for all species
#' 
#' Uses the age at maturity that is implied by the von Bertalanffy growth
#' curve specified by the `w_inf`, `k_vb`, `t0` parameters in the 
#' species_params data frame.
#' 
#' @param params A MizerParams object or a species_params data frame
#' @return A named vector. The names are the species names and the values are
#'   the ages at maturity.
#' @export
a_mat <- function(params) {
    if (is(params, "MizerParams")) {
        sp <- params@species_params
    } else {
        if (!is.data.frame(params)) {
            stop("The first argument must be either a MizerParams object or a species_params data frame.")
        }
        sp <- validSpeciesParams(params)
    }
    if (!all(c("a", "b", "k_vb") %in% names(sp))) {
        stop("The species_params data frame needs to contain the weight-length parameters `a` and `b` and the von Bertalanffy parameter `k_vb`.")
    }
    sp <- set_species_param_default(sp, "t0", 0)

    a_mat <- -log(1 - (sp$w_mat / sp$w_inf) ^ (1/sp$b)) / sp$k_vb + sp$t0
    names(a_mat) <- sp$species
    a_mat
}