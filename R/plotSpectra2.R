#' Show two size spectra in the same plot
#' 
#' @param object1 First MizerParams or MizerSim object
#' @param name1 An optional string with the name for the first model to be used
#'   in the legend. Set to "First" by default.
#' @param object2 Second MizerParams or MizerSim object
#' @param name2 An optional string with the name for the first model to be used
#'   in the legend. Set to "Second" by default.
#' @param power The abundance is plotted as the number density times the weight
#'   raised to power. The default power = 1 gives the biomass density, whereas
#'   power = 2 gives the biomass density with respect to logarithmic size bins.
#' @param ... Parameters pass to `plotSpectra()`
#' @return A ggplot2 object
#' @export
plotSpectra2 <- function(object1, name1, object2, name2, power = 1, ...) {
    
    # Determine label for y axis
    if (power %in% c(0, 1, 2)) {
        y_label <- c("Number density [1/g]", "Biomass density", 
                     "Biomass density [g]")[power + 1]
    }
    else {
        y_label <- paste0("Number density * w^", power)
    }
    
    # Determine colours for plot and legends
    if (is(object1, "MizerSim")) {
        params <- object1@params
    } else {
        params <- object1
    }
    legend_levels <- intersect(names(params@linecolour),
                               unique(sf$Legend))
    linecolours <- params@linecolour[legend_levels]
    
    sf1 <- plotSpectra(params1, power = power, return_data = TRUE, ...)
    sf1$Model <- name1
    sf2 <- plotSpectra(params2, power = power, return_data = TRUE, ...)
    sf2$Model <- name2
    
    ggplot(rbind(sf1, sf2),
           aes(x = w, y = value, colour = Legend, linetype = Model)) +
        geom_line() +
        scale_x_log10("Weight [g]") +
        scale_y_log10(y_label) + 
        scale_colour_manual(values = linecolours)
        
}