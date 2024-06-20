#' Plot biomass flux
#' 
#' @param params A MizerParams object
#' @param ylim A numeric vector of length 2 giving the lower and upper limits
#'   for the y-axis. If the lower limit is NA, it is set to 1e-20. If the upper
#'   limit is NA, it is set to the maximum value of the biomass flux.
#'   Default is c(NA, NA).
#' @param total A logical indicating whether to add a line for the total biomass
#'   flux. Default is FALSE.
#'  
#' @export
#' @examples
#' \dontrun{
#'   params <- newMultispeciesParams(NS_species_params_gears, inter)
#'   plotBiomassFlux(params)
#' }
plotBiomassFlux <- function(params,
                            ylim = c(NA, NA),
                            total = FALSE) {
    params <- validParams(params)
    assert_that(length(ylim) == 2,
                is.logical(total))
    
    # Calculate biomass flux
    growth <- getEGrowth(params)
    flux <- growth * initialN(params) * w(params)[col(growth)]
    
    # Create data frame with column order as needed by `plotDataFrame()`
    df <- melt(flux)[, c(2,3,1)]
    
    # Impose ylim
    if (!is.na(ylim[2])) {
        df <- df[df$value <= ylim[2], ]
    }
    if (is.na(ylim[1])) {
        ylim[1] <- 1e-20
    }
    df <- df[df$value > ylim[1], ]
    
    # Add total column
    if (total) {
        df_total <- df |>
            group_by(w) |>
            summarise(value = sum(value)) |>
            mutate(sp = "Total")
        df <- rbind(df, df_total)
    }
    
    # Make plot
    plotDataFrame(df, params, xtrans = "log10", ytrans = "log10")
}
