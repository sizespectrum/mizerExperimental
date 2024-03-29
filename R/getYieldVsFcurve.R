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
#' higher values means a faster calculation but less accurate maximum.
#' @param distance_func A function that will be called after every `t_per` years
#'   with both the previous and the new state and that should return a number
#'   that in some sense measures the distance between the states. By default
#'   this uses the function [distanceSSLogN()] that you can use as a model for your
#'   own distance function.
#' @param tol The `projectToSteady` function stops when the relative change in the egg production
#' RDI over t_per years is less than tol for every species.
#' @param max_func A function used in `getMaxF` to determines the threshold to use
#' when stopping the fisheries effort.
#' @param threshold_var A numeric value used in max_func to calculate the threshold
#' from the `params` object. If supplied, the function returns the value of the
#' threshold using `threshold_var`. If not supplied, the function returns the
#' unchanged value, which can be compared against the version which uses `threshold_var`.
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
                        no_steps = 10,
                        F_max = 1,
                        F_min = 0,
                        effort_it = 1,
                        distance_func = distanceSSLogN,
                        tol = 0.001,
                        max_func = maxFthreshold,
                        threshold_var = 10,
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

    if(missing(F_range) & missing(F_max))
    {
        maxFdf <- getMaxF(params = params, idx_species = idx_species,
                          effort_it = effort_it, distance_func = distance_func,
                          tol = tol, max_func = max_func,
                          threshold_var = threshold_var) # find maximum viable fisheries effort
        maxFdf = maxFdf %>% rename("F" = effort) # make effort name F

        if(no_steps > dim(maxFdf)[1]) # if the user asked for more steps than already calculated by getMaxF
        {
            add_steps <- no_steps - dim(maxFdf)[1] # number of effort values to calculate
            opt_seq <- seq(0,max(maxFdf$effort), length.out = no_steps) # what a uniform effort value vector looks like
            # remove values close to those getMaxF already calculated
            dupes <- NULL
            for(iEffort in maxFdf$effort)
                dupes <- rbind(dupes,c(abs(opt_seq[which.min(abs(opt_seq - iEffort))]- iEffort),
                                       which.min(abs(opt_seq - iEffort))))

            dupes <- dupes[order(-dupes[,1]),]
            # if need more effort values than already calculates, don't add in between getMaxF's efforts
            if(add_steps > dim(maxFdf)[1])
                effort_todo <- opt_seq[-dupes[,2]] else
                    # if need to add a few efforts inbetween the one already calculated, add where their neighbors are furthest
                    effort_todo <- opt_seq[dupes[1:add_steps,2]]

            # dividing effort vector between "before initial effort" and "after" to reach steady state faster
            effort_vec <- effort_todo[order(effort_todo)] # order so steady states are close to each other
            effort_down <- effort_vec[effort_vec < initial_effort(params)["tmp"]]
            effort_down <- effort_down[order(effort_down,decreasing = TRUE)]
            effort_up <- effort_vec[effort_vec > initial_effort(params)["tmp"]]

            yield_down <- yieldCalculator(params = params, effort_vec = effort_down, idx_species = idx_species,
                                          distance_func = distance_func, tol = tol)
            yield_up <- yieldCalculator(params = params, effort_vec = effort_up, idx_species = idx_species,
                                        distance_func = distance_func, tol = tol)

            yield_vec <- c(yield_down,yield_up)
            effort_vec <- c(effort_down,effort_up)

        } else { # already above maximum number of steps but need to calculate from initial F to 0
            myEffort <- initial_effort(params)["tmp"] - effort_it
            # if effort_it is too big, just need to calculate the 0 value
            if(myEffort > 0) effort_vec <- seq(0,myEffort,by = effort_it) else effort_vec <- 0

            yield_vec <- yieldCalculator(params = params, effort_vec = effort_vec, idx_species = idx_species,
                                         distance_func = distance_func, tol = tol)

        }
        return(rbind(maxFdf,data.frame("yield" = yield_vec, "F" = effort_vec)))
    } else {

        if (!missing(F_max)) {
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
                         F_max,
                         F_min = 0,
                         F_range,
                         effort_it = 1,
                         distance_func = distanceSSLogN,
                         tol = .001,
                         max_func = maxFthreshold,
                         threshold_var = 10,
                         t_max = 100) {
    # Check parameters
    params <- validParams(params)
    species <- valid_species_arg(params, species)

    curve <- getYieldVsF(params,
                         species = species,
                         F_range = F_range,
                         no_steps = no_steps,
                         F_max = F_max,
                         effort_it = effort_it,
                         distance_func = distance_func,
                         tol = tol,
                         max_func = max_func,
                         threshold_var = threshold_var,
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


#' Maximum viable fisheries effort for species
#'
#' @description
#' Calculate the maximum fisheries effort before reaching a specific threshold
#' Default case is biomass exposed to fisheries lower than 10% of original biomass
#'
#' @inheritParams getYieldVsF
#' @param idx_species TODO: document
#'
#' @return dataframe
#' @export
getMaxF <- function(params, idx_species, effort_it = 1,
                    distance_func = distanceSSLogN,
                    tol = 0.001,
                    max_func = maxFthreshold,
                    threshold_var = 10, t_max = 100)
{
    iEffort <- effort_init <- initial_effort(params)["tmp"]
    # threshold to stop the while loop
    biom_threshold <- max_func(params, idx_species, threshold_var)
    yield_vec <- NULL
    while (max_func(params,idx_species) >= biom_threshold)
    {
        params@initial_effort["tmp"] <- iEffort
        sim <- projectToSteady(params, t_max = t_max,
                               return_sim = TRUE, progress_bar = FALSE, distance_func = distance_func, tol = tol)
        y <- getYield(sim)
        ft <- idxFinalT(sim)
        if (ft < 66) { # if convergence use final yield
            yield_vec <- c(yield_vec, y[ft, idx_species])
        } else { # otherwise average over last 30 years (t_per = 1.5)
            yield_vec <- c(yield_vec, mean(y[(ft - 20):ft, idx_species]))
        }
        # Start next simulation with final state from current in the hope that
        # this will be quicker.
        params <- setInitialValues(params, sim)
        iEffort <- iEffort + effort_it
    }
    res <- data.frame("yield" = yield_vec, "effort" = seq(effort_init,length.out = length(yield_vec), by = effort_it))
    # last value of res$effort is max_F
    return(res)

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

    for(sim_version in c("current","previous"))
    {
        switch (sim_version,
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

    if(criterion == "SSE")
        res <- sum((log(yield$current[sel]) - log(yield$previous[sel]))^2) # SSLog
    else if(criterion == "proportion")
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

#' Determines maximum the threshold where
#' fisheries effort should stop
#'
#' @description
#' This function creates a threshold signaling `getMaxF` when
#' effort is considered at it's maximum. In this case it is
#' when the fished biomass is less than 10% of the initial
#' biomass exposed to fisheries.
#'
#'
#' @inheritParams getYieldVsF
#' @param idx_species TODO: document
#'
#' @return a numeric value
#'
maxFthreshold <- function(params, idx_species, threshold_var)
{
    if(missing(threshold_var))
    sum(params@initial_n[idx_species,which(params@w >= params@gear_params$knife_edge_size[idx_species])]) else
        sum(params@initial_n[idx_species,which(params@w >= params@gear_params$knife_edge_size[idx_species])])/threshold_var
}
