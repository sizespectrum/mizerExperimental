# Distance function based on yield
#
# `distanceSSLogYield()` measures the distance between two states by how much
# the yields differ, for use as the `distance_func` of `projectToSteady()`.
#
# The yield-versus-fishing-mortality curve that used to live in this file has
# moved to mizer, where `plotYieldVsF()` is built on `scanModel()`.

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
