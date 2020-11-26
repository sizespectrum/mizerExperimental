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
#' @md
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' y <- getYieldVsFcurve(params,11,bPlot=TRUE)
#' }
getYieldVsFcurve <- function(params,
                             ixSpecies,
                             nSteps = 10,
                             Fmax=3,
                             Frange = seq(0, Fmax, length.out = nSteps),
                             bPlot = FALSE) {
    # Check parameters
    params <- validParams(params)
    ixSpecies <- as.integer(ixSpecies)
    assert_that(
        ixSpecies > 0,
        ixSpecies <= nrow(params@species_params),
        is.numeric(Frange))
    # First make a new gear for that specific species
    sp_name <- params@species_params$species[[ixSpecies]]
    gp <- gear_params(params)
    gp$gear <- as.character(gp$gear)
    gps <- gp$species == sp_name
    gp_extra <- gp[gps, ]
    if (nrow(gp_extra) > 1) {
        stop("This function only works in the case where the targt species ",
             "is selected by a single gear only")
    }
    gp_extra$gear <- "tmp"
    gp_extra$catchability <- 1
    gp$catchability[gps] <- 0
    gear_params(params) <- rbind(gp, gp_extra)
    # First run with zero fishing mortality for 150 years
    s <- project(params, t_max = 150, c(getInitialEffort(params), tmp = 0),
                 progress_bar = FALSE)
    yield <- 0 * Frange
    # Loop over fishing mortalities:
    for (i in 2:length(Frange)) {
        efforts <- c(getInitialEffort(params), tmp = Frange[i])
        s <- project(params, t_max = 40, effort = efforts,
                     progress_bar = FALSE)
        y = getYield(s)
        yield[i] = y[dim(y)[[1]], ixSpecies]
    }
    # Make a plot if requested
    if (bPlot) {
        plot(Frange, yield, type = "b", lwd = 3,
             xlab = "Fishing mortality (1/yr)", ylab = "Yield",
             main = species_params(params)$species[[ixSpecies]])
    }

    return(list(F = Frange, yield = yield))
}
