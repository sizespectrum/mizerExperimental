#' Show two size spectra in the same plot
#' 
#' @param params1 MizerParams object for the first model
#' @param name1 An optional string with the name for the first model to be used in the legend. Set to "First" by default.
#' @param params2 MizerParams object for the second model
#' @param name2 An optional string with the name for the first model to be used in the legend. Set to "Second" by default.
#' @param power The abundance is plotted as the number density times the weight raised to power. The default power = 1 gives the biomass density, whereas power = 2 gives the biomass density with respect to logarithmic size bins.
#' @param ... Parameters pass to `plotSpectra()`
#' @return A ggplot2 object
#' @export
plotSpectra2 <- function(params1, name1, params2, name2, power = 1, ...) {
    assert_that(is(params1, "MizerParams"),
                is(params2, "MizerParams"),
                is.number(power))
    
    if (power %in% c(0, 1, 2)) {
        y_label <- c("Number density [1/g]", "Biomass density", 
                     "Biomass density [g]")[power + 1]
    }
    else {
        y_label <- paste0("Number density * w^", power)
    }
    
    sf1 <- plotSpectra(params1, power = power, return_data = TRUE, ...)
    sf1$Model <- name1
    sf2 <- plotSpectra(params2, power = power, return_data = TRUE, ...)
    sf2$Model <- name2
    ggplot(rbind(sf1, sf2),
           aes(x = w, y = value, colour = Legend, linetype = Model)) +
        geom_line() +
        scale_x_log10("Weight [g]") +
        scale_y_log10(y_label) + 
        scale_colour_manual(values = getColours(params1), limits = force)
        
}