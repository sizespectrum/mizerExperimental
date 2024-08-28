#' Match observed yields
#' 
#' This function matches the observed yields of all the gears for all the
#' species by scaling the catchabilities by the ratio between current modelled
#' yield and observed yield. 
#' 
#' @param params A MizerParams object
#' @param keep A string determining which quantity is to be kept constant. The
#'   choices are "egg" which keeps the egg density constant, "biomass" which 
#'   keeps the total biomass of the species constant and "number" which keeps
#'   the total number of individuals constant.
#' @return A MizerParams object with updated catchabilities
#' @export
matchYield <- function(params,
                       keep = c("egg", "biomass", "number")) {
    assert_that(is(params, "MizerParams"))
    keep <- match.arg(keep)
    
    biomass <- getBiomass(params)
    number <- getN(params)
    
    gp <- params@gear_params
    if (is.null(gp$yield_observed)) {
        stop("You need to specify `yield_observed` in the gear parameter dataframe.")
    }
    gp <- gp[!is.na(gp$yield_observed), ]
    yields <- getYieldGear(params)
    for (i in seq_len(nrow(gp))) {
        gear <- as.character(gp$gear[i])
        species <- as.character(gp$species[i])
        ratio <- gp$yield_observed[i] / yields[gear, species]
        if (!is.nan(ratio)) {
            gear_params(params)[i, "catchability"] <- gp$catchability[i] * ratio
        }
    }
    
    params <- steadySingleSpecies(params)
    
    if (keep == "biomass") {
        factor <- biomass / getBiomass(params)
        params@initial_n <- params@initial_n * factor
    }
    if (keep == "number") {
        factor <- number / getN(params)
        params@initial_n <- params@initial_n * factor
    }
    
    setBevertonHolt(params)
}
