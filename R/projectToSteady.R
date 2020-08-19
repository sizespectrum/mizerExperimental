#' Measure distance between current and previous state in terms of RDI
#'
#' This function can be used in [projectToSteady()] to decide when sufficient
#' convergence to steady state has been achieved.
#'
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @return The largest absolute relative change in rdi:
#'   `max(abs((current_rdi - previous_rdi) / previous_rdi))`
#' @family distance functions
#' @export
distanceMaxRelRDI <- function(params, current, previous) {
    current_rdi <- getRDI(params, n = current$n, n_pp = current$n_pp,
                          n_other = current$n_other)
    previous_rdi <- getRDI(params, n = previous$n, n_pp = previous$n_pp,
                          n_other = previous$n_other)
    max(abs((current_rdi - previous_rdi) / previous_rdi))
}

#' Measure distance between fish abundances current and previous state
#'
#' Calculates the sum squared difference between log(N) in current and previous
#' state. This function can be used in [projectToSteady()] to decide when
#' sufficient convergence to steady state has been achieved.
#'
#' @param current A named list with entries `n`, `n_pp` and `n_other`
#'   describing the current state
#' @param previous A named list with entries `n`, `n_pp` and `n_other`
#'   describing the previous state
#' @return The sum of squares of the difference in the logs of the (nonzero)
#'   fish abundances n:
#'   `sum((log(current$n) - log(previous$n))^2)`
#' @family distance functions
#' @export
distanceSSLogN <- function(params, current, previous) {
    sel <- current$n > 0 & previous$n > 0
    sum((log(current$n[sel]) - log(previous$n[sel]))^2)
}

#' Project to steady state
#'
#' Run the full dynamics, as in [project()], but stop once the change has slowed
#' down sufficiently, in the sense that the distance between states at
#' successive timesteps is less than `tol`. You determine how the distance is
#' calculated.
#'
#' @inheritParams steady
#' @param distance_func A function that will be called after every `t_per` years
#'   with both the previous and the new state and that should return a number
#'   that in some sense measures the distance between the states. By default
#'   this uses the function [distanceSSLogN()] that you can use as a model for your
#'   own distance function.
#' @param ... Further arguments will be passed on to your distance function.
#' @seealso [distanceSSLogN()], [distanceMaxRelRDI()]
#' @export
projectToSteady <- function(params,
                            distance_func = distanceSSLogN,
                            t_per = 1.5,
                            t_max = 100,
                            dt = 0.1,
                            tol = 0.1 * t_per,
                            return_sim = FALSE,
                            progress_bar = TRUE, ...) {
    params <- validParams(params)
    assert_that(t_max >= t_per,
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
                              distance_func = distanceMaxRelRDI,
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
#' @export
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

#' @export
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

#' Check validity of gear parameters and set defaults
#'
#' The function returns a valid gear parameter data frame that can be used
#' by `setFishing()` or it gives an error message.
#'
#' The gear_params data frame is allowed to have zero rows, but if it has
#' rows, then the following requirements apply:
#' * There must be columns `species` and `gear` and any species - gear pair is
#'   allowed to appear at most once. Any species that appears must also appear
#'   in the `species_params` data frame.
#' * There must be a `sel_func` column. If a selectivity function is not
#'   supplied, it will be set to "knife_edge".
#' * There must be a `catchability` column. If a catchability is not supplied,
#'   it will be set to 1.
#' * All the parameters required by the selectivity functions must be provided.
#'
#' If gear_params is empty, then this function tries to find the necessary
#' information in the species_params data frame. This restricts each species
#' to be fished by only one gear. Defaults are used for information that can
#' not be found in the species_params dataframe, as follows:
#' * If there is no `gear` column or it is NA then a new gear named after the
#'   species is introduced.
#' * If there is no `sel_func` column or it is NA then `knife_edge` is used.
#' * If there is no `catchability` column or it is NA then this is set to 1.
#' * If the selectivity function is `knife_edge` and no `knife_edge_size` is
#'   provided, it is set to `w_mat`.
#'
#' For backwards compatibility, when `gear_params` is `NULL` and there is no
#' gear information in the `species_params`, then a gear called `knife_edge_gear`
#' is set up with a `knife_edge` selectivity for each species and a
#' `knive_edge_size` equal to `w_mat`. Catchability is set to 1 for all species.
#'
#' @param gear_params Gear parameter data frame
#' @param species_params Species parameter data frame
#' @return A valid gear parameter data frame
#' @concept helper
#' @seealso [gear_params()]
#' @export
validGearParams <- function(gear_params, species_params) {

    # This is to agree with old defaults
    if (is.null(gear_params) &&
        !("gear" %in% names(species_params) ||
          "sel_func" %in% names(species_params))) {
        gear_params <-
            data.frame(species = species_params$species,
                       gear = "knife_edge_gear",
                       sel_func = "knife_edge",
                       knife_edge_size = species_params$w_mat,
                       catchability = 1)
    }

    species_params <- validSpeciesParams(species_params)
    no_sp <- nrow(species_params)

    # If no gear_params are supplied, but there is either a gear or sel_func
    # column in the species_params data frame, then try to extract information
    # from there.
    if ((is.null(gear_params) || nrow(gear_params) == 0) &&
        ("gear" %in% names(species_params) ||
         "sel_func" %in% names(species_params))) {
        # Try to take parameters from species_params
        gear_params <-
            data.frame(species = as.character(species_params$species),
                       stringsAsFactors = FALSE)
        if ("gear" %in% names(species_params)) {
            gear_params$gear <- as.character(species_params$gear)
            gear_params$gear[is.na(gear_params$gear)] <-
                species_params$species[is.na(gear_params$gear)]
        } else {
            gear_params$gear <- species_params$species
        }
        if ("sel_func" %in% names(species_params)) {
            gear_params$sel_func <- as.character(species_params$sel_func)
            gear_params$sel_func[is.na(gear_params$sel_func)] <- "knife_edge"
        } else {
            gear_params$sel_func <- "knife_edge"
        }
        if ("catchability" %in% names(species_params)) {
            gear_params$catchability <- species_params$catchability
            gear_params$catchability[is.na(gear_params$catchability)] <- 1
        } else {
            gear_params$catchability <- 1
        }
        # copy over any selectivity function parameters
        for (g in seq_len(no_sp)) {
            args <- names(formals(as.character(gear_params[g, 'sel_func'])))
            args <- args[!(args %in% c("w", "species_params", "..."))]
            for (arg in args) {
                if (!arg %in% names(gear_params)) {
                    gear_params[[arg]] <- NA
                }
                if (arg %in% names(species_params) &&
                    !is.na(species_params[g, arg])) {
                    gear_params[g, arg] <- species_params[g, arg]
                } else if (arg == "knife_edge_size") {
                    gear_params[g, arg] <- species_params$w_mat[[g]]
                } else {
                    stop("You need to provide an `", arg, "` column in the species parameter data frame.")
                }
            }
        }
    }

    # An empty gear_params data frame is valid
    if (nrow(gear_params) == 0) {
        return(gear_params)
    }

    if (!all(c("species", "gear") %in% names(gear_params))) {
        stop("`gear_params` must have columns 'species' and 'gear'.")
    }

    # Check that every species mentioned in gear_params exists
    if (!all(gear_params$species %in% species_params$species)) {
        stop("The gear_params dataframe contains species that do not exist in the model.")
    }

    # Check that there are no duplicate gear-species pairs
    if (anyDuplicated(gear_params[, c("species", "gear")])) {
        stop("Some species - gear pairs appear more than once.")
    }

    # Default selectivity function is knife_edge
    if (!("sel_func" %in% names(gear_params))) {
        gear_params$sel_func <- "knife_edge"
    }
    gear_params$sel_func[is.na(gear_params$sel_func)] <- "knife_edge"

    # Default gear name is species name
    sel <- is.na(gear_params$gear)
    gear_params$gear[sel] <- gear_params$species[sel]

    # Ensure there is knife_edge_size columng if any knife_edge selectivity function
    if (any(gear_params$sel_func == "knife_edge") &&
        !("knife_edge_size" %in% names(gear_params))) {
        gear_params$knife_edge_size <- NA
    }

    # Check that every row is complete
    for (g in seq_len(nrow(gear_params))) {
        if ((gear_params$sel_func[[g]] == "knife_edge") &&
            is.na(gear_params$knife_edge_size[[g]])) {
            sel <- species_params$species == gear_params$species[[g]]
            gear_params$knife_edge_size[[g]] <- species_params$w_mat[sel]
        }
        # get args
        # These as.characters are annoying - but factors everywhere
        arg <- names(formals(as.character(gear_params[g, 'sel_func'])))
        arg <- arg[!(arg %in% c("w", "species_params", "..."))]
        if (!all(arg %in% colnames(gear_params))) {
            stop("Some arguments needed for the selectivity function are ",
                 "missing in the gear parameter dataframe.")
        }
        # Check that there are no missing values for selectivity parameters
        if (any(is.na(as.list(gear_params[g, arg])))) {
            stop("Some selectivity parameters are NA.")
        }
    }
    if (!("catchability" %in% names(gear_params))) {
        gear_params$catchability <- 1
    }
    gear_params$catchability[is.na(gear_params$catchability)] <- 1

    gear_params
}
#' Validate species parameter data frame
#'
#' Check validity of species parameters and set defaults for missing but
#' required parameters
#'
#' @param species_params The user-supplied species parameter data frame
#' @return A valid species parameter data frame
#'
#' This function throws an error if
#' * the `species` column does not exist or contains duplicates
#' * the `w_inf` column does not exist or contains NAs or is not numeric
#'
#' It sets default values if any of the following are missing or NA
#' * `w_mat` is set to `w_inf/4`
#' * `w_min` is set to `0.001`
#' * `alpha` is set to `0.6`
#' * `interaction_resource` is set to `1`
#'
#' Any `w_mat25` that is given that is not smaller than `w_mat` is set to
#' `w_mat * 3^(-0.1)`.
#'
#' If `species_params` was provided as a tibble it is converted back to an
#' ordinary data frame.
#'
#' @concept("helper")
#' @export
validSpeciesParams <- function(species_params) {
    assert_that(is.data.frame(species_params))
    # Convert a tibble back to an ordinary data frame
    species_params <- as.data.frame(species_params)

    # check species ----
    if (!("species" %in% colnames(species_params))) {
        stop("The species params dataframe needs a column 'species' with the species names")
    }
    species_names <- as.character(species_params$species)
    row.names(species_params) <- species_names
    no_sp <- nrow(species_params)
    if (length(unique(species_names)) != no_sp) {
        stop("The species parameter data frame has multiple rows for the same species")
    }

    # check w_inf ----
    if (!("w_inf" %in% colnames(species_params))) {
        species_params$w_inf <- rep(NA, no_sp)
    }
    missing <- is.na(species_params$w_inf)
    if (any(missing)) {
        stop("You need to specify maximum sizes for all species.")
    }
    if (!is.numeric(species_params$w_inf)) {
        stop("`w_inf` contains non-numeric values.")
    }

    # Defaults ----
    species_params <- species_params %>%
        set_species_param_default("w_mat", species_params$w_inf / 4) %>%
        set_species_param_default("w_min", 0.001) %>%
        set_species_param_default("alpha", 0.6) %>%
        set_species_param_default("interaction_resource", 1)

    # check w_mat25 ----
    # For w_mat25 it is o.k. if it is NA, but if given it must be
    #  smaller than w_mat
    wrong <- !is.na(species_params$w_mat25) &
        species_params$w_mat25 >= species_params$w_mat
    if (any(wrong)) {
        message("For the species ",
                paste(species_params$species[wrong], collapse = ", "),
                " the value for `w_mat25` is not smaller than that of `w_mat`.",
                " I have corrected that by setting it to about 90% of `w_mat.")
        species_params$w_mat25[wrong] <- species_params$w_mat[wrong]/(3^(1/10))
    }

    species_params
}
