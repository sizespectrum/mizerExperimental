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