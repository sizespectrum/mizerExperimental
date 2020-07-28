#' Choose egg production to keep egg density constant
#'
#' The new egg production is set to compensate for the loss of individuals from
#' the smallest size class through growth and mortality. The result should not
#' be modified by density dependence, so this should be used together with
#' the `noRDD()` function, see example.
#'
#' @param params A MizerParams object
#' @param n A matrix of species abundances (species x size).
#' @param e_growth A two dimensional array (species x size) holding the energy
#'   available for growth as calculated by [mizerEGrowth()].
#' @param mort A two dimensional array (species x size) holding the mortality
#'   rate as calculated by [mizerMort()].
#' @param ... Unused
#'
#' @export
#' @examples
#' \dontrun{
#' # choose an example params object
#' params <- NS_params
#' # We set the reproduction rate functions
#' params <- setRateFunction(params, "RDI", "constantEggRDI")
#' params <- setRateFunction(params, "RDD", "noRDD")
#' # Now the egg density should stay fixed no matter how we fish
#' sim <- project(params, effort = 10, progress_bar = FALSE)
#' # To check that indeed the egg densities have not changed, we first construct
#' # the indices for addressing the egg densities
#' no_sp <- nrow(params@@species_params)
#' idx <- (params@w_min_idx - 1) * no_sp + (1:no_sp)
#' # Now we can check equality between egg densities at the start and the end
#' all.equal(finalN(sim)[idx], initialN(params)[idx])
#' }
constantEggRDI <- function(params, n, e_growth, mort, ...) {
    no_sp <- nrow(params@species_params) # number of species
    # Hacky shortcut to access the correct element of a 2D array using 1D notation
    idx <- (params@w_min_idx - 1) * no_sp + (1:no_sp)
    rdi <- n[idx] * (e_growth[idx] + mort[idx] * params@dw[params@w_min_idx])
    rdi
}
