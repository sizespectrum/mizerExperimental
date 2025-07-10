#' Match observed yields
#' 
#' This function matches the observed yields of all the gears for all the
#' species by scaling the catchabilities by the ratio between current modelled
#' yield and observed yield. 
#' 
#' @param params A MizerParams object
#' @param species The species to be affected. Optional. By default all observed
#'   biomasses will be matched. A vector of species names, or a numeric vector
#'   with the species indices, or a logical vector indicating for each species
#'   whether it is to be affected (TRUE) or not.
#' @param gears The gears to be affected. Optional. By default all gears will be
#'   affected. A vector of gear names.
#' @param keep A string determining which quantity is to be kept constant. The
#'   choices are "egg" which keeps the egg density constant, "biomass" which 
#'   keeps the total biomass of the species constant and "number" which keeps
#'   the total number of individuals constant.
#' @return A MizerParams object with updated catchabilities
#' @export
matchYield <- function(params, species = NULL, gears = NULL,
                       keep = c("egg", "biomass", "number")) {
    assert_that(is(params, "MizerParams"))
    keep <- match.arg(keep)
    species_selected <- valid_species_arg(params, species = species)
    gears_selected <- valid_gears_arg(params, gears = gears)
    biomass <- getBiomass(params)
    number <- getN(params)
    
    gp <- params@gear_params
    gp_sel <- gp$species %in% species_selected & 
        gp$gear %in% gears_selected &
        !is.na(gp$yield_observed)
    gp <- gp[gp_sel, ]
    if (is.null(gp$yield_observed)) {
        stop("You need to specify `yield_observed` in the gear parameter dataframe.")
    }
    yields <- getYieldGear(params)
    for (i in seq_len(nrow(gp))) {
        gear <- as.character(gp$gear[i])
        species <- as.character(gp$species[i])
        ratio <- gp$yield_observed[i] / yields[gear, species]
        if (!is.nan(ratio)) {
            gp[i, "catchability"] <- gp$catchability[i] * ratio
        }
    }
    gear_params(params)[gp_sel, ] <- gp
    
    params <- steadySingleSpecies(params, species = species_selected,
                                  keep = keep)
    
    erepro <- params@species_params[species_selected, "erepro"]
    names(erepro) <- species_selected
    setBevertonHolt(params, erepro = erepro)
}
