#' Calculate a yield-vs-F curve
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#' @param effort_it The width of steps used to find maximum fisheries mortality,
#' higher values means a faster calculation but less accuarate maximum
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
                        F_range = seq(0, F_max, length.out = no_steps),
                        effort_it = 1) {
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
    assert_that(is.numeric(F_range))
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
    gp_extra$gear <- "tmp"
    gp_extra$catchability <- 1
    gp$catchability[gps] <- 0
    gear_params(params) <- rbind(gp, gp_extra)
    initial_effort(params)["tmp"] <- initial_effort(params)[gp$gear[gps]] # setting initial effort same as original gear
    effort <- getInitialEffort(params)
    yield <- 0 * F_range
    # Loop over fishing mortalities:

    # Find where species biomass crashes using getMaxF()
    maxFdf <- getMaxF(params = params, idx_species = idx_species, effort_it = effort_it)
    # getMaxF already took a look at some yield values,
    # need to complete to get a smoother curve
    # need also to calculate values below initial F
    # how many time steps do we want by the end?

    #TODO below section - completing the effort vector with more values if needed
    if(no_steps > dim(maxFdf)) # if the user asked for more steps than already calculate by getMaxF
    {



    } else { # already above max resolution but need to calculate from intitial F to 0

        myEffort <- initial_effort(params)["tmp"] - effort_it

        if(myEffort > 0) effort_vec <- seq(0,myEffort,by = effort_it) else effort_vec <- 0

        for(iEffort in effort_vec)
        {


            params@initial_effort["tmp"] <- iEffort
            sim <- projectToSteady(params, t_max = 100,
                                   return_sim = TRUE, progress_bar = FALSE)
            y <- getYield(sim)


            ft <- idxFinalT(sim)
            # print("number of time steps")
            # print(ft)

            if (ft < 66) { # if convergence use final yield
                yield_vec <- c(yield_vec, y[ft, idx_species])
            } else { # otherwise average over last 30 years (t_per = 1.5)
                yield_vec <- c(yield_vec, mean(y[(ft - 20):ft, idx_species]))
            }
            # Start next simulation with final state from current in the hope that
            # this will be quicker.
            params <- setInitialValues(params, sim)
            # print("biomass left")
            # print(sum(params@initial_n[idx_species,which(params@w >= params@gear_params$knife_edge_size[idx_species])]))


        }



    }



    for (i in seq_along(F_range)) {
        params@initial_effort["tmp"] <- F_range[i]
        sim <- projectToSteady(params, t_max = 100,
                     return_sim = TRUE, progress_bar = FALSE)
        y <- getYield(sim)
        ft <- idxFinalT(sim)

        if (ft < 66) { # if convergence use final yield
            yield[i] <- y[ft, idx_species]
        } else { # otherwise average over last 30 years (t_per = 1.5)
            yield[i] <- mean(y[(ft - 20):ft, idx_species])
        }
        # Start next simulation with final state from current in the hope that
        # this will be quicker.
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
#' plotYieldVsF(params, "Cod")
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
