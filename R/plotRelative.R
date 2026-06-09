#' Plot change in biomass over time
#'
#' `r lifecycle::badge("deprecated")` A similar plot can now be produced with
#' mizer's [mizer::plotRelative()], which works directly on the arrays returned
#' by [mizer::getBiomass()], for example
#' `plotRelative(getBiomass(sim_original), getBiomass(sim))`. Note that
#' [mizer::plotRelative()] shows the symmetric relative difference
#' \eqn{2(N_2 - N_1)/(N_1 + N_2)} rather than the percentage change relative to
#' the reference.
#'
#' @param sim A MizerSim object
#' @param sim_original Another MizerSim object to compare the biomasses to. If
#'   NULL (default) then the biomasses are compared to the initial biomasses in
#'   `sim`.
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a numeric vector with
#'   the species indices, or a logical vector indicating for each species
#'   whether it is to be selected (TRUE) or not.
#' @param ... Parameters passed to `getBiomass()`
#' @export
#' @examples
#' plotBiomassRelative(NS_sim)
#' plotBiomassRelative(NS_sim, species = c("Cod", "Sole"))
plotBiomassRelative <- function(sim, sim_original = NULL, species = NULL, ...) {
    lifecycle::deprecate_soft(
        "3.0.0", "plotBiomassRelative()", "mizer::plotRelative()",
        details = "For example `plotRelative(getBiomass(sim_original), getBiomass(sim))`."
    )
    sel <- valid_species_arg(sim, species, return.logical = TRUE)
    if (is.null(sim_original)) {
        biomass_original <- getBiomass(sim@params, ...)[sel]
    } else {
        biomass_original <- t(getBiomass(sim_original, ...))[sel, , drop = FALSE]
    }
    biomass <- t(getBiomass(sim, ...))[sel, , drop = FALSE]
    rel_diff <- (biomass - biomass_original) / biomass_original * 100
    df <- melt(rel_diff) |>
        transmute(Year = time, `Change %` = value, Species = sp)
    plotDataFrame(df, sim@params)
}

#' @rdname plotBiomassRelative
#' @export
plotlyBiomassRelative <- function(sim, sim_original = NULL, species = NULL, ...) {
    lifecycle::deprecate_soft(
        "3.0.0", "plotlyBiomassRelative()", "mizer::plotRelative()",
        details = "For example `plotRelative(getBiomass(sim_original), getBiomass(sim))`."
    )
    ggplotly(plotBiomassRelative(sim, sim_original, species = species, ...),
             tooltip = c("Species", "Year", "Change %"))
}

#' Plot change in yield over time
#'
#' `r lifecycle::badge("deprecated")` A similar plot can now be produced with
#' mizer's [mizer::plotRelative()], which works directly on the arrays returned
#' by [mizer::getYield()], for example
#' `plotRelative(getYield(object_original), getYield(sim))`. Note that
#' [mizer::plotRelative()] shows the symmetric relative difference
#' \eqn{2(N_2 - N_1)/(N_1 + N_2)} rather than the percentage change relative to
#' the reference.
#'
#' @param sim A MizerSim object
#' @param object_original A MizerParams or MizerSim object to calculate
#'   differences from.
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a numeric vector with
#'   the species indices, or a logical vector indicating for each species
#'   whether it is to be selected (TRUE) or not.
#' @param ... Parameters passed to `getYield()`
#' @export
#' @examples
#' plotYieldRelative(NS_sim, NS_params)
#' plotYieldRelative(NS_sim, NS_params, species = c("Cod", "Sole"))
plotYieldRelative <- function(sim, object_original, species = NULL, ...) {
    lifecycle::deprecate_soft(
        "3.0.0", "plotYieldRelative()", "mizer::plotRelative()",
        details = "For example `plotRelative(getYield(object_original), getYield(sim))`."
    )
    sel <- valid_species_arg(sim, species, return.logical = TRUE)
    if (is(object_original, "MizerParams")) {
        yield_original <- getYield(object_original, ...)[sel]
    } else if (is(object_original, "MizerSim")) {
        yield_original <- t(getYield(object_original, ...))[sel, , drop = FALSE]
    }
    yield <- t(getYield(sim, ...))[sel, , drop = FALSE]
    rel_diff <- (yield - yield_original) / yield_original * 100
    df <- melt(rel_diff) |>
        transmute(Year = time, `Change %` = value, Species = sp)
    plotDataFrame(df, sim@params)
}

#' @rdname plotYieldRelative
#' @export
plotlyYieldRelative <- function(sim, object_original, species = NULL, ...) {
    lifecycle::deprecate_soft(
        "3.0.0", "plotlyYieldRelative()", "mizer::plotRelative()",
        details = "For example `plotRelative(getYield(object_original), getYield(sim))`."
    )
    ggplotly(plotYieldRelative(sim, object_original, species = species, ...),
             tooltip = c("Species", "Year", "Change %"))
}

