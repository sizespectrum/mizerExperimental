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
#' @param F_min The minimum fishing mortality. Used only if `F_range` is missing.
#' @param no_steps The number of steps to use. Only used if `F_range` is missing.
#' @param distance_func A function that will be called after every `t_per` years
#'   with both the previous and the new state and that should return a number
#'   that in some sense measures the distance between the states. By default
#'   this uses the function [distanceSSLogN()] that you can use as a model for your
#'   own distance function.
#' @param tol The `projectToSteady` function stops when the relative change in the egg production
#' RDI over t_per years is less than tol for every species.
#' @param t_max The longest time to run project to find steady state.
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
                        F_range,
                        F_max = 1,
                        F_min = 0,
                        no_steps = 10,
                        distance_func = distanceSSLogN,
                        tol = 0.001,
                        t_max = 100
                        ) {
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
                              distance_func = distance_func, tol = tol)

    # First make a new gear for that specific species
    sp_name <- params@species_params$species[[idx_species]]
    gp <- gear_params(params)
    gp$gear <- as.character(gp$gear)
    sp_sel <- which(gp$species == sp_name)
    if (length(sp_sel) == 0) {
        stop(species, " is not selected by any gear.")
    }
    current_FMort <- sum(params@initial_effort[gp$gear[sp_sel]] * 
                             gp$catchability[sp_sel])
    # base the new gear on the first gear that catches this species
    # TODO: think about how to improve this arbitrary choice.
    gp_extra <- gp[sp_sel[1], ]
    gp_extra$gear <- "tmp"
    gp_extra$catchability <- 1
    gp$catchability[sp_sel] <- 0
    gear_params(params) <- rbind(gp, gp_extra)
    initial_effort(params)["tmp"] <- 1

    if (missing(F_range)) {
        F_range = seq(F_min, F_max, length.out = no_steps)
    }
    
    assert_that(is.numeric(F_range))
    sel <- F_range < current_FMort
    F_range1 <- sort(F_range[sel], decreasing = TRUE)
    F_range2 <- sort(F_range[!sel])
    
    # First work down from current fishing
    yield_vec1 <- 
        yieldCalculator(params = params, effort_vec = F_range1, 
                        idx_species = idx_species,
                        distance_func = distance_func, 
                        tol = tol, t_max = t_max)
    # Then work up from current fishing
    yield_vec2 <- 
        yieldCalculator(params = params, effort_vec = F_range2, 
                        idx_species = idx_species,
                        distance_func = distance_func, 
                        tol = tol, t_max = t_max)
    
    return(data.frame(yield = c(yield_vec1, yield_vec2), 
                      F = c(F_range1, F_range2)))
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
                         F_range,
                         F_max = 1,
                         F_min = 0,
                         no_steps = 10,
                         distance_func = distanceSSLogN,
                         tol = 0.001,
                         t_max = 100) {
    # Check parameters
    params <- validParams(params)
    species <- valid_species_arg(params, species)

    curve <- getYieldVsF(params,
                         species = species,
                         F_range = F_range,
                         F_max = F_max,
                         F_min = F_min,
                         no_steps = no_steps,
                         distance_func = distance_func,
                         tol = tol,
                         t_max = t_max
                         )
    
    pl <- ggplot(curve, aes(x = F, y = yield)) +
        geom_line() +
        xlab("Fishing mortality (1/yr)") +
        ylab("Yield") +
        ggtitle(species)
    
    if ("F_MSY" %in% names(params@species_params)) {
        F_MSY = params@species_params$F_MSY
        pl <- pl + 
            geom_vline(xintercept = F_MSY, linetype = dashed, colour = "grey")
    }
    
    pl
}

#' Measure distance between current and previous state in terms of yield.
#'
#' @description
#' Calculates the proportional difference between getYield() outputs of current
#' and previous state. This function can be used in projectToSteady() to decide
#' when sufficient convergence to steady state has been achieved.
#'
#' @param params MizerParams
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @param criterion TODO: document
#'
#' @return proportional difference between current and previous state
#' @family distance functions
#' @export

distanceSSLogYield <- function(params, current, previous, criterion = "SSE")
{
    effort <- params@initial_effort
    time_range <- 0
    t <- min(time_range)
    yield <- list()

    for (sim_version in c("current","previous")) {
        switch(sim_version,
               "current" = {
                   biomass <- sweep(current$n, 2, params@w * params@dw, "*")
                   n <- current$n
                   n_pp <- current$n_pp
                   n_other <- current$n_other},
               "previous" = {
                   biomass <- sweep(previous$n, 2, params@w * params@dw, "*")
                   n <- previous$n
                   n_pp <- previous$n_pp
                   n_other <- previous$n_other},
               {}
        )
        no_gears <- dim(params@catchability)[[1]]
        f <- get(params@rates_funcs$FMort)
        if (length(dim(effort)) == 2) {
            times <- dimnames(effort)$time
            f_mort <- array(0,
                            dim = c(dim(effort)[[1]], dim(params@initial_n)),
                            dimnames = c(list(time = times),
                                         dimnames(params@initial_n)))
            times <- as.numeric(times)
            for (i in 1:dim(effort)[1]) {
                f_mort[i, , ] <-
                    f(params, n = n, n_pp = n_pp, n_other = n_other,
                      effort = effort[i, ], t = times[i],
                      e_growth = getEGrowth(params, n = n, n_pp = n_pp,
                                            n_other = n_other, t = times[i]),
                      pred_mort = getPredMort(params, n = n, n_pp = n_pp,
                                              n_other = n_other,
                                              time_range = times[i]))
            }
        } else if (length(effort) <= 1) {
            f_mort <- f(params, n = n, n_pp = n_pp, n_other = n_other,
                        effort = rep(effort, no_gears), t = t,
                        e_growth = getEGrowth(params, n = n, n_pp = n_pp,
                                              n_other = n_other, t = t),
                        pred_mort = getPredMort(params, n = n, n_pp = n_pp,
                                                n_other = n_other,
                                                time_range = t))
            dimnames(f_mort) <- dimnames(params@metab)
        } else if (length(effort) == no_gears) {
            f_mort <- f(params, n = n, n_pp = n_pp, n_other = n_other,
                        effort = effort, t = t,
                        e_growth = getEGrowth(params, n = n, n_pp = n_pp,
                                              n_other = n_other, t = t),
                        pred_mort = getPredMort(params, n = n, n_pp = n_pp,
                                                n_other = n_other,
                                                time_range = t))
            dimnames(f_mort) <- dimnames(params@metab)

        }

        yield[[sim_version]] <- apply(f_mort * biomass,
                                      c(1), sum)


    }

    sel <- yield$current > 0 & yield$previous > 0

    if (criterion == "SSE")
        res <- sum((log(yield$current[sel]) - log(yield$previous[sel]))^2) # SSLog
    else if (criterion == "proportion")
        res <- abs(sum(yield$current[sel] - yield$previous[sel])) / sum(yield$previous[sel]) # propotion change

    return(res)
}


#' Calculates yield from steady sim
#'
#' @description
#' This function replaces a loop used multiple times within
#' [getYieldVsF()]
#'
#' @inheritParams getYieldVsF
#' @param effort_vec TODO: document
#' @param idx_species TODO: document
#'
#' @return a vector of yield value of same length as `effort_vec`
#'
yieldCalculator <- function(params, effort_vec, idx_species,
                            distance_func = distanceSSLogN,
                            tol = 0.001, t_max = 100) {
    yield_vec <- effort_vec # To get the right length
    for (i in seq_along(effort_vec)) {
        message(paste("F =", effort_vec[i]))
        params@initial_effort["tmp"] <- effort_vec[i]
        sim <- projectToSteady(params, t_max = t_max, t_per = 1.5,
                               return_sim = TRUE, progress_bar = FALSE, 
                               distance_func = distance_func, tol = tol)
        y <- getYield(sim)
        ft <- idxFinalT(sim)
        if (ft < t_max - 1) { # if convergence use final yield
            yield_vec[i] <- y[ft, idx_species]
            params <- setInitialValues(params, sim)
        } else { # otherwise average over last 45 years (t_per = 1.5)
            yield_vec[i] <- mean(y[(ft - 30):ft, idx_species])
            params <- setInitialValues(params, sim,
                                       time_range = c(65, 100))
        }
    }

    return(yield_vec)
}
