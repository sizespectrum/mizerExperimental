#' Calculate a yield-vs-F curve
#'
#' Calculates the yield of a species for a range of fishing mortalities for
#' that species while the fishing mortalities for the other species are held
#' fixed.
#'
#' @param params An object of class `MizerParams`.
#' @param species Name of the target species
#' @param F_range A sequence of fishing mortalities at which to evaluate the
#'   yield. If missing, it is set to
#'   `seq(from = 0, to = F_max, length.out = no_steps)`.
#' @param F_max The maximum fishing mortality. Used only if `F_range` is missing.
#' @param no_steps The number of steps to use. Only used if `F_range` is missing.
#'
#' @return A data frame with columns `F` and `yield`.
#' @export
#' @family summary functions
#' @concept summary_function
#' @seealso plotYieldVsF
#' @md
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' y <- getYieldVsF(params, "Cod", F_max = 1, no_steps = 5)
#' }
getYieldVsF <- function(params,
                        species,
                        no_steps = 10,
                        F_max = 3,
                        F_range = seq(0, F_max, length.out = no_steps)) {
    # Check parameters
    params <- validParams(params)
    idx_species <- which(params@species_params$species == species)
    if (length(idx_species) != 1) {
        stop("Invalid species name")
    }
    assert_that(is.numeric(F_range))
    # First make a new gear for that specific species
    sp_name <- params@species_params$species[[idx_species]]
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
    effort <- getInitialEffort(params)
    yield <- 0 * F_range
    # Loop over fishing mortalities:
    for (i in seq_along(F_range)) {
        params@initial_effort["tmp"] <- F_range[i]
        sim <- projectToSteady(params, t_max = 100,
                     return_sim = TRUE, progress_bar = FALSE)
        y <- getYield(sim)
        yield[i] <- y[dim(y)[[1]], idx_species]
        params <- setInitialValues(params, sim)
    }

    return(data.frame(F = F_range, yield = yield))
}

#' Plot a yield-vs-F curve
#'
#' @inherit getYieldVsF
#'
#' @inheritParams getYieldVsF
#'
#' @return A ggplot object
#' @export
#' @family plotting functions
#' @seealso getYieldVsF
#' @md
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' plotYieldVsFcurve(params, "Cod")
#' }
plotYieldVsF <- function(params,
                         species,
                         no_steps = 10,
                         F_max = 3,
                         F_range = seq(0, F_max, length.out = no_steps)) {

    curve <- getYieldVsF(params,
                         species = species,
                         F_range = F_range)

    ggplot(curve, aes(x = F, y = yield)) +
        geom_line() +
        xlab("Fishing mortality (1/yr)") +
        ylab("Yield") +
        ggtitle(species)
}
