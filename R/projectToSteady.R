#' Measure distance between current and previous state in terms of RDI
#'
#' This function can be used in [projectToSteady()] to decide when sufficient
#' convergence to steady state has been achieved.
#'
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @return A number expressing a distance between current and previous state.
#' @family distance functions
#' @export
distanceRDI <- function(params, current, previous) {
    current_rdi <- getRDI(params, n = current$n, n_pp = current$n_pp,
                          n_other = current$n_other)
    previous_rdi <- getRDI(params, n = previous$n, n_pp = previous$n_pp,
                          n_other = previous$n_other)
    max(abs((current_rdi - previous_rdi) / previous_rdi))
}

#' Measure mean squared distance between log(N) in current and previous state
#'
#' This function can be used in [projectToSteady()] to decide when sufficient
#' convergence to steady state has been achieved.
#'
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @return A number expressing a distance between current and previous state.
#' @family distance functions
#' @export
distanceLogN <- function(params, current, previous) {
    sum((log(current$n) - log(previous$n))^2)
}

#' Project to steady state
#'
#' Run the full dynamics, as in `project()`, but stop once the change has slowed
#' down sufficiently, in the sense that the distance between states at
#' successive timesteps is less than `tol`. You determine how the distance is
#' calculated.
#'
#' @inheritParams steady
#' @param distance_func A function that will be called after every `t_per` years
#'   with both the previous and the new state and that should return a number
#'   that in some sense measures the distance between the states. By default
#'   this uses the function  that you can use as a model for your
#'   own distance function.
#' @param ... Further arguments will be passed on to your distance function.
#' @seealso [distanceLogN()], [distanceRDI()]
#' @export
projectToSteady <- function(params,
                            distance_func = distanceRDI,
                            t_per = 1.5,
                            t_max = 100,
                            dt = 0.1,
                            tol = 0.1 * t_per,
                            return_sim = FALSE,
                            progress_bar = TRUE, ...) {
    params <- validParams(params)
    assert_that(noNA(getRDD(params)),
                t_max >= t_per,
                tol > 0)
    if ((t_per < dt) || !isTRUE(all.equal((t_per - round(t_per / dt) * dt), 0))) {
        stop("t_per must be a positive multiple of dt")
    }
    t_dimnames <-  seq(0, t_max, by = t_per)

    if (is(progress_bar, "Progress")) {
        # We have been passed a shiny progress object
        progress_bar$set(message = "Finding steady state", value = 0)
        proginc <- 1/ceiling(t_max/t_per)
    }

    if (return_sim) {
        # create MizerSim object
        sim <- MizerSim(params, t_dimnames =  t_dimnames)
        sim@n[1, , ] <- params@initial_n
        sim@n_pp[1, ] <- params@initial_n_pp
        sim@n_other[1, ] <- params@initial_n_other
        sim@effort[1, ] <- params@initial_effort
    }

    # get functions
    resource_dynamics_fn <- get(params@resource_dynamics)
    other_dynamics_fns <- lapply(params@other_dynamics, get)
    rates_fns <- lapply(params@rates_funcs, get)

    previous <- list(n = params@initial_n,
                     n_pp = params@initial_n_pp,
                     n_other = params@initial_n_other,
                     rates = getRates(params, n = params@initial_n,
                                      n_pp = params@initial_n_pp,
                                      n_other = params@initial_n_other))

    for (i in 2:length(t_dimnames)) {
        # advance shiny progress bar
        if (is(progress_bar, "Progress")) {
            progress_bar$inc(amount = proginc)
        }
        current <- project_simple(params, n = previous$n, n_pp = previous$n_pp,
                                  n_other = previous$n_other, t = 0,
                                  dt = dt, steps = round(t_per / dt),
                                  effort = params@initial_effort,
                                  resource_dynamics_fn = resource_dynamics_fn,
                                  other_dynamics_fns = other_dynamics_fns,
                                  rates_fns = rates_fns)
        if (return_sim) {
            # Store result
            sim@n[i, , ] <- current$n
            sim@n_pp[i, ] <- current$n_pp
            sim@n_other[i, ] <- current$n_other
            sim@effort[i, ] <- params@initial_effort
        }

        # Species with no reproduction are going extinct, so stop.
        extinct <- is.na(current$rates$rdd) | current$rates$rdd <= 1e-20
        if (any(extinct)) {
            warning(paste(params@species_params$species[extinct], collapse = ", "),
                    " are going extinct.")
            success <- FALSE
            distance <- NA
            break
        }

        distance <- distance_func(params,
                                  current = current,
                                  previous = previous, ...)
        success <- distance < tol
        if (success == TRUE) {
            break
        }
        previous <- current
    }
    if (!success) {
        message("Simulation run did not converge after ",
                (i - 1) * t_per,
                " years. Value returned by the distance function was: ",
                distance)
    } else {
        message("Convergence was achieved in ", (i - 1) * t_per, " years.")
    }

    params@initial_n[] <- current$n
    params@initial_n_pp[] <- current$n_pp
    params@initial_n_other[] <- current$n_other

    if (return_sim) {
        sim@params <- params
        return(sim)
    } else {
        return(params)
    }
}

#' Set initial values to a steady state for the model
#'
#' The steady state is found by running the dynamics while keeping reproduction
#' and other components constant until the size spectra no longer change (or
#' until time `t_max` is reached, if earlier). Then the reproductive efficiencies
#' are set to the values that give the level of reproduction observed in that
#' steady state.
#'
#' @param params A \linkS4class{MizerParams} object
#' @param t_max The maximum number of years to run the simulation. Default is 100.
#' @param t_per The simulation is broken up into shorter runs of `t_per` years,
#'   after each of which we check for convergence. Default value is 1.5. This
#'   should be chosen as an odd multiple of the timestep `dt` in order to be
#'   able to detect period 2 cycles.
#' @param dt The time step to use in `project()`.
#' @param tol The simulation stops when the relative change in the egg
#'   production RDI over `t_per` years is less than `tol` for every species.
#' @param return_sim If TRUE, the function returns the MizerSim object holding
#'   the result of the simulation run. If FALSE (default) the function returns
#'   a MizerParams object with the "initial" slots set to the steady state.
#' @param progress_bar A shiny progress object to implement a progress bar in a
#'   shiny app. Default FALSE.
#' @export
#' @examples
#' \dontrun{
#' params <- newTraitParams()
#' species_params(params)$gamma[5] <- 3000
#' params <- steady(params)
#' plotSpectra(params)
#' }
steady <- function(params, t_max = 100, t_per = 1.5, dt = 0.1,
                   tol = 0.1 * dt, return_sim = FALSE, progress_bar = TRUE) {
    params <- validParams(params)

    # Force the reproduction to stay at the current level
    params@species_params$constant_reproduction <- getRDD(params)
    old_rdd_fun <- params@rates_funcs$RDD
    params@rates_funcs$RDD <- "constantRDD"

    # Force other components to stay at current level
    old_other_dynamics <- params@other_dynamics
    for (res in names(params@other_dynamics)) {
        params@other_dynamics[[res]] <- "constant_other"
    }

    object <- projectToSteady(params,
                              distance_func = distanceRDI,
                              t_per = t_per,
                              t_max = t_max,
                              dt = dt,
                              tol = tol,
                              return_sim = return_sim,
                              progress_bar = progress_bar)
    if (return_sim) {
        params <- object@params
    } else {
        params <- object
    }
    # Restore original RDD and other dynamics
    params@rates_funcs$RDD <- old_rdd_fun
    params@other_dynamics <- old_other_dynamics

    # Retune the values of erepro so that we get the correct level of
    # reproduction
    params <- retune_erepro(params)

    if (return_sim) {
        object@params <- params
        return(object)
    } else {
        return(params)
    }
}

# Only needed until mizer 2.0.4
getRates <- function(params, n = initialN(params),
                     n_pp = initialNResource(params),
                     n_other = initialNOther(params),
                     effort, t = 0) {
    params <- validParams(params)
    if (missing(effort)) {
        effort <- params@initial_effort
    }

    r <- get(params@rates_funcs$Rates)(
        params, n = n, n_pp = n_pp, n_other = n_other,
        t = t, effort = effort, rates_fns = lapply(params@rates_funcs, get))
}
validParams <- function(params) {
    assert_that(is(params, "MizerParams"))
    # Check that params has all the slots
    # Can't use `slotnames(params)` to find out which slots params actually has
    # because `slotnames()` just looks at the class definition.
    has_slot <- sapply(slotNames(NS_params),
                       function(name) .hasSlot(params, name))
    if (!all(has_slot) ||
        "interaction_p" %in% names(params@species_params) ||
        "r_max" %in% names(params@species_params)) {
        params <- upgradeParams(params)
        warning("You need to upgrade your MizerParams object with `upgradeParams()`.")
    }
    validObject(params)
    params
}
