#' Designate species as background species
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Marks the specified set of species as background species. Background species
#' are handled differently in some plots and their abundance is automatically
#' adjusted in [retuneBackground()] to keep the community close to the
#' Sheldon spectrum.
#'
#' @param object An object of class \linkS4class{MizerParams} or
#'   \linkS4class{MizerSim}.
#' @inheritParams mizer::valid_species_arg
#'
#' @return An object of the same class as the `object` argument
#' @export
#' @examples
#' \dontrun{
#' params <- newMultispeciesParams(NS_species_params_gears, inter)
#' sim <- project(params, effort=1, t_max=20, t_save = 0.2, progress_bar = FALSE)
#' sim <- markBackground(sim, species = c("Sprat", "Sandeel",
#'                                        "N.pout", "Dab", "Saithe"))
#' plotSpectra(sim)
#' }
markBackground <- function(object, species = NULL) {
    if (is(object, "MizerSim")) {
        species <- valid_species_arg(object, species)
        object@params@A[dimnames(object@params@initial_n)$sp %in% species] <- NA
    } else if (is(object, "MizerParams")) {
        species <- valid_species_arg(object, species)
        object@A[dimnames(object@initial_n)$sp %in% species] <- NA
    } else {
        stop("The `object` argument must be of type MizerParams or MizerSim.")
    }
    return(object)
}


#' Retunes abundance of background species.
#'
#'  @description
#' `r lifecycle::badge("experimental")`
#'
#' Rescales all background species in such a way that the total community
#' spectrum is as close to the Sheldon power law as possible. Background
#' species that are no longer needed are removed. The reproductive efficiencies
#' of all species are retuned.
#'
#' @param params A \linkS4class{MizerParams} object
#'
#' @return An object of type `MizerParams`
#' @seealso [markBackground()]
#' @export
retuneBackground <- function(params) {
    params <- validParams(params)
    no_sp <- nrow(params@species_params)  # Number of species
    L <- is.na(params@A)
    if (!any(L)) {
        message("There are no background species left.")
        return(params)
    }

    # We find the abundance multipliers A_i so
    # that the integral of the square of the relative distance
    # (sum_{i not in L} A_i*N_i(w) + sum_{i not in L} N_i(w) - sc(w))/sc(w)
    # over w, between our limits, is minimized, where  L is the set of all
    # retuneable species.

    # ignore zero entries in params@sc and only use region above the smallest w_mat
    region <- params@sc > 0 & params@w > min(params@species_params$w_mat)
    sc <- params@sc[region]
    # rho is the total abundance of all the non-tunable species
    rho <- colSums(params@initial_n[!L, region, drop = FALSE])

    # Use Singular Value Decomposition to find optimal abundance multipliers.
    # See Numerical Recipes section 15.4.2
    #
    # Rescale by sc
    A <- t(sweep(params@initial_n[L, region, drop = FALSE], 2, sc, "/"))
    b <- (sc - rho) / sc

    sv <- svd(A)
    di <- 1/sv$d  # inverse of singular values
    di[di > 10^8] <- 0  # cut off
    x <- sweep(sv$v, 2, di, "*") %*% t(sv$u) %*% b
    A2 <- rep(1, no_sp)
    A2[L] <- x

    # We may have to repeat this if any of the multipliers is negative or zero
    if (any(A2 <= 0)) {
        # Remove those species
        params <- removeSpecies(params, species = (A2 <= 0))
        # and try again retuning the remaining retunable species
        if (any(A2 > 0)) {
            params <- retuneBackground(params)
        } else {
            message("All background species have been removed.")
        }
    } else {
        # Use these abundance multipliers to rescale the abundance curves
        params@initial_n <- params@initial_n * A2
    }

    return(setBevertonHolt(params, reproduction_level = 1/4))
}

#' Removes species with abundance below a threshold
#'
#'  @description
#' `r lifecycle::badge("experimental")`
#'
#' This species simply removes the low-abundance species from the params object.
#' It does not recalculate the steady state for the remaining species or
#' retune their reproductive efficiencies.
#'
#' @param params A \linkS4class{MizerParams} object
#' @param cutoff Species with an abundance at maturity size that is less than
#'               cutoff times community abundance will be removed. Default 1e-3.
#'
#' @return An object of type `MizerParams`
#' @export
pruneSpecies <- function(params, cutoff = 1e-3) {
    params <- validParams(params)
    no_sp <- nrow(params@species_params)  # Number of species
    # Determine which species need to be removed
    remove <- c()
    for (i in seq_along(params@species_params$species)) {
        # index of maturity size of this species
        w_mat_idx <- min(which(params@w > params@species_params$w_mat[i]))
        # If species abundance at maturity is less than cutoof * community
        # abundance at that weight, then remove the species.
        if (params@initial_n[i, w_mat_idx] < params@sc[w_mat_idx] * cutoff) {
            remove <- c(remove, params@species_params$species[i])
        }
    }
    # Remove
    removeSpecies(params, remove)
}


#' Rescale Abundance
#'
#' Multiplies the abundances of all or of selected species by given factors and
#' then retunes the reproductive efficiencies accordingly.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @details
#' Does not run the system to steady state. For that you should call
#' [steady()] explicitly afterwards.
#'
#' @param params A mizer params object
#' @param factor The factor by which the abundance of each species is multiplied.
#'   This can be specified in two ways:
#'   \itemize{
#'   \item A named numeric vector where the name indicates the species and the
#'     value gives the factor for that species. Only the named species are
#'     affected.
#'   \item  A number that gives the factor for all foreground species.
#'   }
#'
#' @return An object of type \linkS4class{MizerParams}
#' @export
rescaleAbundance <- function(params, factor) {
    params <- validParams(params)
    assert_that(is.numeric(factor),
                all(factor > 0))
    is_foreground <- !is.na(params@A)
    no_sp <- sum(is_foreground)
    if (length(factor) == 1 && length(names(factor)) == 0) {
        factor <- rep(factor, no_sp)
        names(factor) <- params@species_params$species[is_foreground]
    }
    to_rescale <- names(factor)
    wrong <- setdiff(to_rescale, params@species_params$species)
    if (length(wrong) > 0) {
        stop(paste(wrong, collapse = ", "),
             " do not exist.")
    }
    assert_that(length(to_rescale) == length(factor))

    params@initial_n[to_rescale, ] <-
        params@initial_n[to_rescale, ] * factor

    return(setBevertonHolt(params, reproduction_level = 1/4))
}

#' Rescale System
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The abundances in mizer and some rates depend on the size of the area to
#' which they refer. So they could be given per square meter or per square
#' kilometer or for an entire study area or any other choice of yours. This
#' function allows you to change the size by automatically changing the
#' abundances and rates accordingly.
#'
#' @details
#' If you rescale the system by a factor \eqn{c} then this function makes the
#' following rescalings in the params object:
#' \itemize{
#' \item The initial abundances `initial_n`, `initial_n_pp` and
#'   `initial_n_other` are rescaled by \eqn{c}.
#' \item The search volume is rescaled by \eqn{1/c}.
#' \item The resource carrying capacity is rescaled by \eqn{c}
#' \item The maximum reproduction rate \eqn{R_{max}}, if used, is rescaled by
#'   \eqn{c}.
#' }
#' The effect of this is that the dynamics of the rescaled system are identical
#' to those of the unscaled system, in the sense that it does not matter whether
#' one first calls [rescaleSystem()] and then runs a simulation with
#' [project()] or whether one first runs a simulation and then rescales the
#' resulting abundances.
#'
#' Note that if you use non-standard resource dynamics or other components then you
#' may need to rescale additional parameters that appear in those dynamics.
#'
#' @param params A mizer params object
#' @param factor The factor by which the size is rescaled with respect to which
#'   the abundances are given.
#'
#' @return An object of type \linkS4class{MizerParams}
#' @export
rescaleSystem <- function(params, factor) {
    params <- validParams(params)
    assert_that(is.number(factor),
                factor > 0)

    # Resource replenishment rate
    params@cc_pp <- params@cc_pp * factor
    params@resource_params$kappa <- params@resource_params$kappa * factor

    # Rmax
    # r_max is a deprecated spelling of R_max. Get rid of it.
    if ("r_max" %in% names(params@species_params)) {
        params@species_params$R_max <- params@species_params$r_max
        params@species_params$r_max <- NULL
        message("The 'r_max' column has been renamed to 'R_max'.")
    }
    if ("R_max" %in% names(params@species_params)) {
        params@species_params$R_max <- params@species_params$R_max * factor
    }

    # Search volume
    params@search_vol = params@search_vol / factor
    if ("gamma" %in% names(params@species_params)) {
        params@species_params$gamma <- params@species_params$gamma / factor
    }

    # Initial values
    initial_n_other <- params@initial_n_other
    for (res in names(initial_n_other)) {
        initial_n_other[[res]] <- initial_n_other[[res]] * factor
    }
    initialN(params) <- params@initial_n * factor
    initialNResource(params) <- params@initial_n_pp * factor
    initialNOther(params) = initial_n_other

    return(params)
}


#' Update the initial values
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Recalculates the steady-state abundances in a fixed background
#' given by the current abundances, keeping the abundances fixed in the
#' smallest size class for each species. Then readjusts the `erepro`
#' values.
#'
#' @param params A MizerParams object
#'
#' @return The MizerParams object with updated `initial_n` and
#'   `initial_n_pp` slots.
#' @export
updateInitialValues <- function(params) {
    params <- validParams(params)
    # Calculate the rates in the current background
    resource_mort <- getResourceMort(params)
    mumu <- getMort(params)
    gg <- getEGrowth(params)
    # Recompute resource
    params@initial_n_pp <- params@rr_pp * params@cc_pp /
        (params@rr_pp + resource_mort)
    # Recompute all species
    for (sp in 1:length(params@species_params$species)) {
        w_inf_idx <- min(sum(params@w < params@species_params[sp, "w_inf"]) + 1,
                         length(params@w))
        idx <- params@w_min_idx[sp]:(w_inf_idx - 1)
        if (any(gg[sp, idx] == 0)) {
            stop("Can not compute steady state due to zero growth rates")
        }
        n0 <- params@initial_n[sp, params@w_min_idx[sp]]
        params@initial_n[sp, ] <- 0
        params@initial_n[sp, params@w_min_idx[sp]:w_inf_idx] <-
            c(1, cumprod(gg[sp, idx] / ((gg[sp, ] + mumu[sp, ] * params@dw)[idx + 1]))) *
            n0
    }

    # Retune the values of erepro so that we get the correct level of
    # recruitment
    mumu <- getMort(params)
    gg <- getEGrowth(params)
    rdd <- getRDD(params)
    # TODO: vectorise this
    for (i in (1:length(params@species_params$species))) {
        gg0 <- gg[i, params@w_min_idx[i]]
        mumu0 <- mumu[i, params@w_min_idx[i]]
        DW <- params@dw[params@w_min_idx[i]]
        params@species_params$erepro[i] <- params@species_params$erepro[i] *
            params@initial_n[i, params@w_min_idx[i]] *
            (gg0 + DW * mumu0) / rdd[i]
    }
    return(params)
}
