#' Plot the relative difference between two spectra
#' 
#' If we denote the number density from the first object as \eqn{N_1(w)} and
#' that from the second object as \eqn{N_2(w)}, then this plot shows
#' \deqn{(N_2(w) - N_1(w)) / (N_2(w) - N_1(w)).}
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
    
    sf1 <- mizer::plotSpectra(object1, return_data = TRUE, resource = FALSE, ...)
    sf2 <- mizer::plotSpectra(object2, return_data = TRUE, resource = FALSE, ...)
    
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
#' @param ... Parameters passed to `getBiomass()`
#' @export
plotBiomassRelative <- function(sim, sim_original = NULL, ...) {
    if (is.null(sim_original)) {
        biomass_original <- getBiomass(sim@params, ...)
    } else {
        biomass_original <- t(getBiomass(sim_original, ...))
    }
    biomass <- t(getBiomass(sim, ...))
    rel_diff <- (biomass - biomass_original) / biomass_original * 100
    df <- melt(rel_diff) |>
        transmute(Year = time, `Change %` = value, Species = sp)
    plotDataFrame(df, sim@params)
}

#' @rdname plotBiomassRelative
#' @export
plotlyBiomassRelative <- function(sim, sim_original = NULL, ...) {
    ggplotly(plotBiomassRelative(sim, sim_original, ...),
             tooltip = c("Species", "Year", "Change %"))
}

#' Plot change in yield over time
#'
#' @param sim A MizerSim object
#' @param object_original A MizerParams or MizerSim object to calculate
#'   differences from.
#' @param ... Parameters passed to `getYield()`
#' @export
plotYieldRelative <- function(sim, object_original, ...) {
    if (is(object_original, "MizerParams")) {
        yield_original <- getYield(params, ...)
    } else if (is(object_original, "MizerSim")) {
        yield_original <- t(getYield(params, ...))
    }
    yield <- t(getYield(sim, ...))
    rel_diff <- (yield - yield_original) / yield_original * 100
    df <- melt(rel_diff) |>
        transmute(Year = time, `Change %` = value, Species = sp)
    plotDataFrame(df, sim@params)
}

#' @rdname plotYieldRelative
#' @export
plotlyYieldRelative <- function(sim, object_original, ...) {
    ggplotly(plotYieldRelative(sim, object_original, ...),
             tooltip = c("Species", "Year", "Change %"))
}

