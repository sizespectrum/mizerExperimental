#' Calculate a yield-vs-F curve for a species.
#'
#' Calculates the yield of a species for a range of fishing mortalities
#' for species across all gears at each
#' simulation time step. **Note:** The function just returns the yield
#' at the last time step of the simulation, which might not have converged,
#' or might oscillate.
#'
#' @param params An object of class `MizerParams`.
#' @param ixSpecies The species number to make the calculation over
#' @param nSteps The number of steps in F to calculate
#' @param Fmax The maximum fishing mortality
#' @param Frange The range of fishing mortalities to run over
#' @param bPlot Boolean that indicates whether a plot is to be made
#'
#' @return An list with yields and fishing mortalities
#' @export
#' @family summary functions
#' @concept summary_function
#' @seealso [getYield()]
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' y <- getYieldVsFcurve(params,11,bPlot=TRUE)
#' }
getYieldVsFcurve <- function(params,
                             ixSpecies,
                             nSteps = 10,
                             Fmax=3,
                             bPlot = FALSE) {
    Frange = seq(0, Fmax, length.out = nSteps)
    # First make a new gear for that specific species
    gear = gear_params(params)
    levels(gear$gear) = c(levels(gear$gear), 'tmp')
    gear[ixSpecies, 'gear'] = 'tmp'
    gear_params(params) <- gear
    # First run with zero fishing mortality for 150 years
    s = project(params, t_max=150, c(getInitialEffort(params), tmp=0))
    yield = 0*Frange
    # Loop over fishing mortalities:
    for (i in 2:length(Frange)) {
        efforts = c(getInitialEffort(params), tmp=Frange[i])
        s = project(params, t_max=40, effort=efforts)
        y = getYield(s)
        yield[i] = y[ dim(y)[1], ixSpecies ]
    }
    # Make a plot if requested
    if (bPlot) {
        plot(Frange, yield, type="b", lwd=3,
             xlab="Fishing mortality (1/yr)", ylab="Yield",
             main=species_params(params)$species[ixSpecies])
    }

    return( list(F = Frange, yield=yield) )
}
