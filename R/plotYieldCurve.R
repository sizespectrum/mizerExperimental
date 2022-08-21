#' Calculate yield curve
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculates SSB, RDI, RDD, and the yield of a species for a range of
#' fishing mortalities for that species while the fishing mortalities for the
#' other species are held fixed.
#'
#' @param params An object of class `MizerParams`.
#' @param species Name of the target species
#' @param F_range A sequence of fishing mortalities at which to evaluate the
#'   SSB. If missing, it is set to
#'   `seq(from = 0, to = F_max, length.out = no_steps)`.
#' @param F_max The maximum fishing mortality. Used only if `F_range` is
#'   missing.
#' @param no_steps The number of steps to use. Only used if `F_range` is
#'   missing.
#' @param tol The `projectToSteady` function stops when the relative change in
#'   the egg production RDI over t_per years is less than tol for every species.
#' @param t_max The longest time to run project to find steady state.
#' 
#' @return A data frame with columns `F`, `SY`, `SSB`, `RDD`.
#' @export
#' @family summary functions
#' @concept summary_function
#' @seealso plotSSBVsF, getYieldVsF
#' @md
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' y <- getSSBVsF(params, "Cod", F_max = 1, no_steps = 5)
#' }
getYieldCurve <- function(params,
                          species,
                          F_range,
                          no_steps = 20,
                          F_max,
                          tol = 0.001,
                          t_max = 100) {
    # Check parameters
    params <- validParams(params)
    species <- valid_species_arg(params, species)
    if (length(species) > 1) {
        stop("You can only make this plot for one species at a time.")
    }
    idx_species <- which(params@species_params$species == species)
    if (length(idx_species) != 1) {
        stop("Invalid species specification")
    }
    
    # Project to steady with the current fishing
    params <- projectToSteady(params, t_max = t_max, progress_bar = FALSE,
                              tol = tol)
    
    # First make a new gear for that specific species
    sp_name <- params@species_params$species[[idx_species]]
    gp <- gear_params(params)
    gp$gear <- as.character(gp$gear)
    gps <- gp$species == sp_name
    gp_extra <- gp[gps, ]
    if (nrow(gp_extra) > 1) {
        stop("This function only works in the case where the target species ",
             "is selected by a single gear only")
    }
    current_FMort <- params@initial_effort[gp$gear[gps]] * gp_extra$catchability
    gp_extra$gear <- "tmp"
    gp_extra$catchability <- 1
    gp$catchability[gps] <- 0
    gear_params(params) <- rbind(gp, gp_extra)
    initial_effort(params)["tmp"] <- current_FMort
    effort <- getInitialEffort(params)
    
    if (!missing(F_max)) {
        F_range = seq(0, F_max, length.out = no_steps)
    }
    
    assert_that(is.numeric(F_range))
    # split into the F values smaller than current fishing
    # and those larger or equal to current fishing
    sel <- F_range < current_FMort
    F_range1 <- sort(F_range[sel], decreasing = TRUE)
    F_range2 <- sort(F_range[!sel])
    
    # First work down from current fishing
    df1 <- calc_vals(params = params, F_range = F_range1, 
                     idx_species = idx_species,
                     tol = tol, t_max = t_max)
    # Then work up from current fishing
    df2 <- calc_vals(params = params, F_range = F_range2, 
                     idx_species = idx_species,
                     tol = tol, t_max = t_max)
    
    return(rbind(df1, df2))
}

#' Plot normalised SBB, RDD and sustainable yield versus F
#'
#' @inherit getYieldCurve
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
#' plotYieldCurve(params, "Cod")
#' }
plotYieldCurve <- function(params,
                           species,
                           no_steps = 20,
                           F_max,
                           F_range,
                           tol = .001,
                           t_max = 100) {
    # Check parameters
    params <- validParams(params)
    species <- valid_species_arg(params, species)
    
    curve <- getYieldCurve(params,
                           species = species,
                           F_range = F_range,
                           no_steps = no_steps,
                           F_max = F_max,
                           tol = tol,
                           t_max = t_max) |>
        mutate(SSB = SSB / max(SSB),
               RDD = RDD / max(RDD),
               SY = SY / max(SY)) |>
        pivot_longer(!F , names_to = "Quantity", values_to = "values") |>
        mutate(Quantity = factor(Quantity, levels = c("SY", "SSB", "RDD")))
    
    ggplot(curve) +
        geom_line(aes(x = F, y = values, linetype = Quantity)) +
        labs(x = "Fishing mortality (1/yr)",
             y = "Scaled quantities") +
        ggtitle(species)
}

#' Calculates SSB, RDD and yield at a range of fishing mortalities
#'
#' @description
#' This function replaces a loop used multiple times within
#' `getYieldCurve`
#'
#' @inheritParams getYieldVsF
#'
#' @return A data frame with columns `F`, `SY`, `SSB`, `RDD`
#'
calc_vals <- function(params, F_range, idx_species,
                      tol = 0.001, t_max = 100) {
    SSB_vec <- F_range # To get the right length
    RDD_vec <- F_range
    SY_vec <- F_range
    for (i in seq_along(F_range)) {
        message(paste("F =", F_range[i]))
        params@initial_effort["tmp"] <- F_range[i]
        params <- projectToSteady(params, t_max = t_max, progress_bar = FALSE,
                                  tol = tol)
        
        SSB_vec[i] <- getSSB(params)[idx_species]
        RDD_vec[i] <- getRDD(params)[idx_species]
        SY_vec[i] <- getYield(params)[idx_species]
    }
    
    data.frame(F = F_range, SY = SY_vec, SSB = SSB_vec, RDD = RDD_vec)
}
