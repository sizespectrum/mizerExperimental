#' Plot biomass flux
#' 
#' This function plots the biomass flux for each species in the model
#' as a function of body size. To understand the meaning of the biomass flux
#' one should think of the growth of the fish as transporting biomass from
#' the small to the large individuals. The biomass flux is the rate at which
#' biomass is transported through a particular size. So it is given as the
#' product of the growth rate \eqn{g(w)}, the body mass \eqn{w} and the 
#' density of individuals \eqn{N(w)} of that size,
#' \deqn{J_B(w) = g(w) w N(w).}
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
#' plotBiomassFlux(NS_params, ylim = c(1e7, NA), total = TRUE)
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
