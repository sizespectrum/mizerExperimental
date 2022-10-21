#' Show two size spectra in the same plot
#' 
#' @param object1 First MizerParams or MizerSim object.
#' @param object2 Second MizerParams or MizerSim object.
#' @param name1 An optional string with the name for the first model, to be used
#'   in the legend. Set to "First" by default.
#' @param name2 An optional string with the name for the second model, to be
#'   used in the legend. Set to "Second" by default.
#' @param power The abundance is plotted as the number density times the weight
#'   raised to this power. The default power = 1 gives the biomass density,
#'   whereas power = 2 gives the biomass density with respect to logarithmic
#'   size bins.
#' @param ... Parameters to pass to `plotSpectra()`
#' @return A ggplot2 object
#' @export
#' 
#' @examples
#' sim1 <- project(NS_params, t_max = 10)
#' sim2 <- project(NS_params, effort = 0.5, t_max = 10)
#' plotSpectra2(sim1, sim2, "Original", "Effort = 0.5")
plotSpectra2 <- function(object1, object2, name1 = "First", name2 = "Second",
                         power = 1, ...) {
    
    sf1 <- mizerMR::plotSpectra(object1, power = power, return_data = TRUE, ...)
    sf1$Model <- name1
    sf2 <- mizerMR::plotSpectra(object2, power = power, return_data = TRUE, ...)
    sf2$Model <- name2
    sf <- rbind(sf1, sf2)
    
    if (is(object1, "MizerSim")) {
        params <- object1@params
    } else {
        params <- object1
    }
    legend_levels <- intersect(names(params@linecolour), unique(sf$Legend))
    linecolours <- params@linecolour[legend_levels]
    
    if (power %in% c(0, 1, 2)) {
        y_label <- c("Number density [1/g]", "Biomass density", 
                     "Biomass density [g]")[power + 1]
    }
    else {
        y_label <- paste0("Number density * w^", power)
    }
    
    ggplot(sf, aes(x = w, y = value, colour = Legend, linetype = Model)) +
        geom_line() +
        scale_x_log10("Weight [g]") +
        scale_y_log10(y_label) + 
        scale_colour_manual(values = linecolours)
        
}