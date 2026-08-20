#' Calculate a yield-vs-F curve
#'
#' Calculates the yield of a species for a range of fishing mortalities for
#' that species while the fishing mortalities for the other species are held
#' fixed.
#'
#' If the target species is caught by several gears you can use the `gear`
#' argument to select the gear whose fishing mortality is to be varied. The
#' fishing mortality that the other gears exert on the target species is then
#' held fixed and the `F` values in the result refer only to the fishing
#' mortality from the selected gear. If you do not select a gear, the fishing
#' mortality from all gears on the target species is replaced by the fishing
#' mortality given by `F_range`, using the selectivity of the first gear that
#' catches the species.
#'
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @section How the yield at each F is determined:
#'
#' For every value in `F_range` the model is projected until it settles on an
#' attractor, using [mizer::projectToSteady()]. That function stops as soon as
#' the state has stopped changing *or* it recognises that the model has settled
#' onto a limit cycle, and it reports which of the two happened in the
#' `"convergence"` attribute of the object it returns. This function then reads
#' the yield off that attractor:
#'
#' * **Fixed point** (`type = "below_tolerance"`): the yield is read straight
#'   off the settled state, with no further projection.
#' * **Limit cycle** (`type = "cycle"`): the model is projected for exactly one
#'   period of the detected cycle and the yield is averaged over that period,
#'   which is the long-term average yield. The minimum and maximum yield over
#'   the cycle are also returned.
#' * **Neither** (`type = "not_converged"` or `"extinction"`): the model never
#'   settled within `t_max` years. The yield is averaged over the last
#'   `t_sample` years (10 by default) and a message names the F values
#'   concerned, because those points of the curve should not be trusted.
#'
#' Averaging over exactly one period is both faster and more accurate than
#' averaging over a fixed number of years: a window that is not a whole number
#' of periods leaves a residue of the oscillation in the average, which shows
#' up as a jagged yield curve.
#'
#' Each F value is started from the attractor reached at the previous F value,
#' which cuts out most of the transient. The `F_range` is therefore worked
#' through outwards from the current fishing mortality of the model: first
#' downwards through the values below it and then upwards through the values
#' above it.
#'
#' @param params An object of class `MizerParams`.
#' @param species Name of the target species
#' @param gear Name of the gear whose fishing mortality on the target species
#'   is to be varied. Only needed when several gears catch the target species.
#'   If NULL (default), the fishing mortality from all gears catching the
#'   target species is varied together.
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
#' @param tol The simulation at each F value stops once the number returned by
#'   `distance_func` for two states `t_per` years apart drops below `tol`. This
#'   is the main knob for trading accuracy against speed: loosening it to
#'   mizer\'s own default of `0.1 * t_per` typically takes about half as long,
#'   and the `residual` column of the result tells you whether the states you
#'   settled on are still drifting.
#' @param t_max The longest time to run the projection at each F value while
#'   looking for an attractor.
#' @param t_per The interval in years at which [mizer::projectToSteady()]
#'   checks for convergence. Should be an odd multiple of `dt`.
#' @param dt The time step used in the projections.
#' @param t_sample The number of years over which to average the yield when the
#'   model has not settled onto an attractor within `t_max` years. When the
#'   model has settled onto a limit cycle the yield is averaged over exactly
#'   one period of that cycle instead. Default 10.
#' @param amplitude_tol The minimum relative biomass amplitude for a persistent
#'   oscillation to be treated as a limit cycle rather than as a fixed point.
#'   Passed to [mizer::projectToSteady()].
#' @param extinction_threshold A species is treated as going extinct, stopping
#'   the projection, once its reproduction rate falls below this fraction of
#'   its value at the start of the projection. Passed to
#'   [mizer::projectToSteady()].
#' @param progress_bar Whether to show a text progress bar over the F values.
#'   Default TRUE in interactive sessions.
#' @param ... Further arguments are passed on to `distance_func`.
#'
#' @return A data frame with one row for each value in `F_range` and the
#'   columns
#'   \describe{
#'     \item{F}{The fishing mortality. If a `gear` was selected then this is
#'       the fishing mortality exerted by that gear alone, otherwise it is the
#'       total fishing mortality on the target species.}
#'     \item{yield}{The long-term average yield of the target species. On a
#'       fixed point this is simply the yield in the steady state, on a limit
#'       cycle it is the average over one period.}
#'     \item{yield_min, yield_max}{The smallest and largest yield over the
#'       averaging window. These equal `yield` on a fixed point and give the
#'       range of the oscillation on a limit cycle.}
#'     \item{type}{The kind of attractor that was reached, as reported by
#'       [mizer::projectToSteady()]: `"below_tolerance"`, `"cycle"`,
#'       `"not_converged"` or `"extinction"`.}
#'     \item{period}{The period of the limit cycle in years, or `NA` if the
#'       model settled on a fixed point.}
#'     \item{residual}{How far the state reached still is from a fixed point,
#'       as the largest per-capita rate of change in 1/year, from
#'       [mizer::getSteadyResidual()]. A value of 0.01 means the abundances
#'       were still drifting by 1% per year when the yield was read off. On a
#'       limit cycle the state keeps moving by construction, so this is only
#'       informative for the `"below_tolerance"` rows.}
#'   }

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
getYieldVsF <- function(params, species, gear = NULL, F_range, F_max = 1,
                        F_min = 0, no_steps = 10,
                        distance_func = distanceSSLogN,
                        tol = 0.001, t_max = 100, t_per = 1.5, dt = 0.1,
                        t_sample = 10, amplitude_tol = 0.01,
                        extinction_threshold = 1e-6,
                        progress_bar = interactive(), ...)
    UseMethod("getYieldVsF")

#' @export
getYieldVsF.MizerParams <- function(params,
                        species,
                        gear = NULL,
                        F_range,
                        F_max = 1,
                        F_min = 0,
                        no_steps = 10,
                        distance_func = distanceSSLogN,
                        tol = 0.001,
                        t_max = 100,
                        t_per = 1.5,
                        dt = 0.1,
                        t_sample = 10,
                        amplitude_tol = 0.01,
                        extinction_threshold = 1e-6,
                        progress_bar = interactive(),
                        ...) {
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
    assert_that(is.number(tol), tol > 0,
                is.number(t_max), t_max > 0,
                is.number(t_per), t_per > 0,
                is.number(dt), dt > 0,
                is.number(t_sample), t_sample > 0,
                is.number(amplitude_tol), amplitude_tol > 0,
                is.number(extinction_threshold), extinction_threshold >= 0,
                is.flag(progress_bar))

    # Determine the gear params rows whose fishing mortality is to be varied
    sp_name <- params@species_params$species[[idx_species]]
    gp <- gear_params(params)
    gp$gear <- as.character(gp$gear)
    sp_sel <- which(gp$species == sp_name)
    if (length(sp_sel) == 0) {
        stop(species, " is not selected by any gear.")
    }
    if (!is.null(gear)) {
        gear <- as.character(gear)
        if (length(gear) != 1) {
            stop("You can only select a single gear.")
        }
        gear_sel <- sp_sel[gp$gear[sp_sel] == gear]
        if (length(gear_sel) == 0) {
            stop("The gear ", gear, " does not catch ", sp_name, ".")
        }
        # Only the fishing mortality from the selected gear will be varied,
        # that from the other gears is kept fixed.
        sp_sel <- gear_sel
    } else if (length(sp_sel) > 1) {
        message("Several gears catch ", sp_name,
                ". The fishing mortality from all of them will be replaced. ",
                "Use the `gear` argument if you want to vary the fishing ",
                "mortality from only one gear.")
    }

    # Make sure we start from an attractor of the current fishing. There is no
    # need to project if the model is already at its steady state.
    if (!isSteady(params)) {
        params <- projectToSteady(params, t_max = t_max, t_per = t_per,
                                  dt = dt, progress_bar = FALSE,
                                  distance_func = distance_func, tol = tol,
                                  amplitude_tol = amplitude_tol,
                                  extinction_threshold = extinction_threshold,
                                  info_level = 0, ...)
    }

    current_FMort <- sum(params@initial_effort[gp$gear[sp_sel]] *
                             gp$catchability[sp_sel])
    # Make a new gear that exerts the fishing mortality that is to be varied,
    # based on the first of the selected gears.
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
    # Work outwards from the current fishing mortality in both directions, so
    # that each projection can start from the attractor found at the
    # neighbouring F value.
    F_range1 <- sort(F_range[sel], decreasing = TRUE)
    F_range2 <- sort(F_range[!sel])

    pb <- NULL
    if (isTRUE(progress_bar) && length(F_range) > 0) {
        pb <- utils::txtProgressBar(min = 0, max = length(F_range), style = 3)
        on.exit(close(pb), add = TRUE)
    }

    # First work down from current fishing
    result1 <- yieldCalculator(params, effort_vec = F_range1,
                               idx_species = idx_species,
                               distance_func = distance_func, tol = tol,
                               t_max = t_max, t_per = t_per, dt = dt,
                               t_sample = t_sample,
                               amplitude_tol = amplitude_tol,
                               extinction_threshold = extinction_threshold,
                               pb = pb, pb_offset = 0, ...)
    # Then work up from current fishing
    result2 <- yieldCalculator(params, effort_vec = F_range2,
                               idx_species = idx_species,
                               distance_func = distance_func, tol = tol,
                               t_max = t_max, t_per = t_per, dt = dt,
                               t_sample = t_sample,
                               amplitude_tol = amplitude_tol,
                               extinction_threshold = extinction_threshold,
                               pb = pb, pb_offset = length(F_range1), ...)

    curve <- rbind(result1, result2)
    curve <- curve[order(curve$F), , drop = FALSE]
    rownames(curve) <- NULL

    cycles <- curve$type == "cycle"
    if (any(cycles)) {
        message("The model settled onto a limit cycle at F = ",
                paste(signif(curve$F[cycles], 3), collapse = ", "),
                ". The yield at those F values is the average over one period ",
                "of the cycle.")
    }
    unsettled <- curve$type == "not_converged"
    if (any(unsettled)) {
        message("The model did not settle onto an attractor within ", t_max,
                " years at F = ",
                paste(signif(curve$F[unsettled], 3), collapse = ", "),
                ". The yield at those F values is only the average over the ",
                "last ", t_sample, " years and should not be relied on. ",
                "Increase `t_max` to give the model more time to settle, or ",
                "loosen `tol`. The `residual` column says how fast the ",
                "abundances were still changing, in 1/year.")
    }
    extinct <- curve$type == "extinction"
    if (any(extinct)) {
        message("A species was on its way to extinction at F = ",
                paste(signif(curve$F[extinct], 3), collapse = ", "),
                ", which stopped the projection early. The yield at those F ",
                "values is only the average over the ", t_sample,
                " years following that point.")
    }

    return(curve)
}
#' Plot a yield-vs-F curve
#'
#' @inherit getYieldVsF
#'
#' @inheritParams getYieldVsF
#'
#' @details
#' Where the model settles onto a limit cycle rather than a fixed point, the
#' line shows the yield averaged over one period of the cycle and a shaded band
#' shows the range from the smallest to the largest yield over that cycle.
#'
#' If the species parameters contain an `F_MSY` column, the value for the
#' plotted species is marked by a vertical dashed line.
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
plotYieldVsF <- function(params, species, gear = NULL, F_range, F_max = 1,
                         F_min = 0, no_steps = 10,
                         distance_func = distanceSSLogN,
                         tol = 0.001, t_max = 100, t_per = 1.5, dt = 0.1,
                         t_sample = 10, amplitude_tol = 0.01,
                         extinction_threshold = 1e-6,
                         progress_bar = interactive(), ...)
    UseMethod("plotYieldVsF")

#' @export
plotYieldVsF.MizerParams <- function(params,
                         species,
                         gear = NULL,
                         F_range,
                         F_max = 1,
                         F_min = 0,
                         no_steps = 10,
                         distance_func = distanceSSLogN,
                         tol = 0.001,
                         t_max = 100,
                         t_per = 1.5,
                         dt = 0.1,
                         t_sample = 10,
                         amplitude_tol = 0.01,
                         extinction_threshold = 1e-6,
                         progress_bar = interactive(),
                         ...) {
    # Check parameters
    params <- validParams(params)
    species <- valid_species_arg(params, species)

    curve <- getYieldVsF(params,
                         species = species,
                         gear = gear,
                         F_range = F_range,
                         F_max = F_max,
                         F_min = F_min,
                         no_steps = no_steps,
                         distance_func = distance_func,
                         tol = tol,
                         t_max = t_max,
                         t_per = t_per,
                         dt = dt,
                         t_sample = t_sample,
                         amplitude_tol = amplitude_tol,
                         extinction_threshold = extinction_threshold,
                         progress_bar = progress_bar,
                         ...
                         )

    xlabel <- if (is.null(gear)) {
        "Fishing mortality (1/yr)"
    } else {
        paste0("Fishing mortality from ", gear, " (1/yr)")
    }

    pl <- ggplot(curve, aes(x = F, y = yield))
    # Where the model oscillates, show the range of the oscillation around the
    # average yield.
    if (any(curve$yield_max > curve$yield_min)) {
        pl <- pl +
            geom_ribbon(aes(ymin = yield_min, ymax = yield_max),
                        fill = "grey70", alpha = 0.4) +
            labs(subtitle = paste("Shaded band: range of the yield over the",
                                  "oscillation"))
    }
    pl <- pl +
        geom_line() +
        xlab(xlabel) +
        ylab("Yield") +
        ggtitle(species)

    if ("F_MSY" %in% names(params@species_params)) {
        idx_species <- which(params@species_params$species == species)
        F_MSY <- params@species_params$F_MSY[[idx_species]]
        if (!is.na(F_MSY)) {
            pl <- pl +
                geom_vline(xintercept = F_MSY, linetype = "dashed",
                           colour = "grey")
        }
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
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @param params MizerParams
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @param criterion TODO: document
#' @param ... Not used.
#'
#' @return proportional difference between current and previous state
#' @family distance functions

#' @export
distanceSSLogYield <- function(params, current, previous,
                               criterion = "SSE", ...)
    UseMethod("distanceSSLogYield")

#' @export
distanceSSLogYield.MizerParams <- function(params, current, previous,
                                           criterion = "SSE", ...) {
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


#' Calculate the yield on the attractor for a sequence of fishing mortalities
#'
#' @description
#' This function does the work for [getYieldVsF()]. For each entry of
#' `effort_vec` in turn it projects the model until it settles onto an
#' attractor and then reads the yield of the target species off that attractor,
#' see the details section of [getYieldVsF()]. Each projection starts from the
#' attractor reached at the previous entry of `effort_vec`, so the entries
#' should be ordered by their distance from the current state of `params`.
#'
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @inheritParams getYieldVsF
#' @param effort_vec A vector of fishing mortalities to impose via the "tmp"
#'   gear, ordered so that consecutive entries are close to each other.
#' @param idx_species The index of the target species in the species params.
#' @param pb Either NULL or a `txtProgressBar` to advance after each entry of
#'   `effort_vec`.
#' @param pb_offset The number of F values already handled, used to advance
#'   `pb` to the right position.
#' @param ... Further arguments are passed on to `distance_func`.
#'
#' @return A data frame with one row for each entry of `effort_vec` and the
#'   columns `F`, `yield`, `yield_min`, `yield_max`, `type`, `period` and
#'   `residual`, as described in [getYieldVsF()].

yieldCalculator <- function(params, effort_vec, idx_species,
                            distance_func = distanceSSLogN,
                            tol = 0.001, t_max = 100, t_per = 1.5, dt = 0.1,
                            t_sample = 10, amplitude_tol = 0.01,
                            extinction_threshold = 1e-6,
                            pb = NULL, pb_offset = 0, ...)
    UseMethod("yieldCalculator")

#' @export
yieldCalculator.MizerParams <- function(params, effort_vec, idx_species,
                            distance_func = distanceSSLogN,
                            tol = 0.001, t_max = 100, t_per = 1.5, dt = 0.1,
                            t_sample = 10, amplitude_tol = 0.01,
                            extinction_threshold = 1e-6,
                            pb = NULL, pb_offset = 0, ...) {
    n <- length(effort_vec)
    result <- data.frame(F = effort_vec,
                         yield = rep(NA_real_, n),
                         yield_min = rep(NA_real_, n),
                         yield_max = rep(NA_real_, n),
                         type = rep(NA_character_, n),
                         period = rep(NA_real_, n),
                         residual = rep(NA_real_, n),
                         stringsAsFactors = FALSE)

    for (i in seq_len(n)) {
        params@initial_effort["tmp"] <- effort_vec[i]

        # Run the dynamics until they settle onto an attractor. This stops as
        # soon as either a fixed point or a limit cycle has been recognised,
        # rather than always running for the full `t_max` years.
        settled <- projectToSteady(params, t_max = t_max, t_per = t_per,
                                   dt = dt, return_sim = FALSE,
                                   progress_bar = FALSE,
                                   distance_func = distance_func, tol = tol,
                                   amplitude_tol = amplitude_tol,
                                   extinction_threshold = extinction_threshold,
                                   info_level = 0, ...)
        conv <- attr(settled, "convergence")
        result$type[i] <- conv$type
        result$period[i] <- conv$period
        result$residual[i] <- conv$residual

        if (identical(conv$type, "below_tolerance")) {
            # A fixed point: the yield in the settled state is the answer and
            # no further projection is needed.
            y <- getYield(settled)[[idx_species]]
            result$yield[i] <- y
            result$yield_min[i] <- y
            result$yield_max[i] <- y
        } else {
            # The state keeps moving, so the yield has to be averaged over a
            # window. On a limit cycle we use exactly one period, which makes
            # the average the true long-term average. Otherwise we fall back on
            # a fixed window and warn the user in getYieldVsF().
            whole_period <- identical(conv$type, "cycle") &&
                is.finite(conv$period) && conv$period > 0
            window <- if (whole_period) conv$period else t_sample
            # project() needs the window to be a whole number of time steps
            no_steps <- max(1, round(window / dt))
            sim <- project(settled, t_max = no_steps * dt, dt = dt,
                           t_save = dt, progress_bar = FALSE)
            y <- as.vector(getYield(sim)[, idx_species])
            # The final sample repeats the phase of the first one, so it must
            # be left out of the average over a whole period.
            y_mean <- if (whole_period) y[-length(y)] else y
            result$yield[i] <- mean(y_mean)
            result$yield_min[i] <- min(y)
            result$yield_max[i] <- max(y)
        }

        # Warm start: the next F value starts from the attractor just reached.
        params <- settled
        if (!is.null(pb)) utils::setTxtProgressBar(pb, pb_offset + i)
    }

    return(result)
}
