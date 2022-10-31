#' Plot the relative difference between two spectra
#' 
#' This plots the difference between the spectra relative to their average. So
#' if we denote the number density from the first object as \eqn{N_1(w)} and
#' that from the second object as \eqn{N_2(w)}, then this plot shows
#' \deqn{2 (N_2(w) - N_1(w)) / (N_2(w) + N_1(w)).}
#' 
#' The individual spectra are calculated by the [plotSpectra()] function which
#' is passed all additional arguments you supply. So you can for example
#' determine a size range over which to average the simulation results via the
#' `time_range` argument. See [plotSpectra()] for more options.
#' 
#' Note that it does not matter whether the relative difference is calculated
#' for the number density or the biomass density or the biomass density in log
#' weight because the factors of \eqn{w} by which the densities differ cancels 
#' out in the relative difference.
#' 
#' @param object1 An object of class MizerSim or MizerParams
#' @param object2 An object of class MizerSim or MizerParams
#' @param ... Parameters passed to `plotSpectra()`
#' 
#' @return A ggplot2 object
#' @export
#' @examples
#' sim1 <- project(NS_params, t_max = 10)
#' sim2 <- project(NS_params, effort = 0.5, t_max = 10)
#' plotSpectraRelative(sim1, sim2)
plotSpectraRelative <- function(object1, object2, ...) {
    
    # TODO: Add checks that the two objects are compatible
    
    sf1 <- mizer::plotSpectra(object1, return_data = TRUE, ...)
    sf2 <- mizer::plotSpectra(object2, return_data = TRUE, ...)
    
    sf <- left_join(sf1, sf2, by = c("w", "Legend")) |>
        mutate(rel_diff = (value.y - value.x) / (value.x + value.y))
    
    if (is(object1, "MizerSim")) {
        params <- object1@params
    } else {
        params <- object1
    }
    legend_levels <- intersect(names(params@linecolour),
                               unique(sf$Legend))
    linecolours <- params@linecolour[legend_levels]
    
    ggplot(sf,
           aes(x = w, y = rel_diff, colour = Legend)) +
        geom_line() +
        labs(x = "Weight [g]", y = "Relative difference") +
        scale_x_log10() +
        scale_color_manual(values = linecolours) +
        geom_hline(yintercept = 0, linetype = 1,
                   colour = "dark grey", size = 0.75)
}

#' @rdname plotSpectraRelative
#' @export
plotlySpectraRelative <- function(object1, object2, ...) {
    ggplotly(plotSpectraRelative(object1, object2, ...),
             tooltip = c("Legend", "w", "rel_diff"))
}

#' Plot change in biomass over time
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
    ggplotly(plotBiomassRelative(sim, sim_original, ...),
             tooltip = c("Species", "Year", "Change %"))
}

#' Plot change in yield over time
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
    ggplotly(plotYieldRelative(sim, object_original, ...),
             tooltip = c("Species", "Year", "Change %"))
}

