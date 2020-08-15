#' Designate species as background species
#'
#' Marks the specified set of species as background species. Background species
#' are handled differently in some plots and their abundance is automatically
#' adjusted in [retuneBackground()] to keep the community close to the
#' Sheldon spectrum.
#'
#' @param object An object of class \linkS4class{MizerParams} or
#'   \linkS4class{MizerSim}.
#' @param species Name or vector of names of the species to be designated as
#'   background species. By default this is set to all species.
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
markBackground <- function(object, species) {
    if (is(object, "MizerSim")) {
        if (missing(species)) {
            species <- dimnames(object@params@initial_n)$sp
        }
        object@params@A[dimnames(object@params@initial_n)$sp %in% species] <- NA
    } else if (is(object, "MizerParams")) {
        if (missing(species)) {
            species <- dimnames(object@initial_n)$sp
        }
        object@A[dimnames(object@initial_n)$sp %in% species] <- NA
    } else {
        stop("The `object` argument must be of type MizerParams or MizerSim.")
    }
    return(object)
}


#' Retunes abundance of background species.
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

    return(retune_erepro(params))
}

#' Removes species with abundance below a threshold
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

#' Remove species from an ecosystem
#'
#' This function simply removes all entries from the MizerParams object that
#' refer to the selected species. It does not recalculate the steady state for
#' the remaining species or retune their reproductive efficiency.
#'
#' @param params A mizer params object for the original system.
#' @param species A vector of the names of the species to be deleted or a boolean
#'   vector indicating for each species whether it is to be removed (TRUE) or
#'   not.
#'
#' @return An object of type \linkS4class{MizerParams}
#' @export
#' @examples
#' \dontrun{
#' params <- NS_params
#' species_params(params)$species
#' params <- removeSpecies(params, c("Cod", "Haddock"))
#' species_params(params)$species
#' }
removeSpecies <- function(params, species) {
    params <- validParams(params)
    no_sp <- length(params@w_min_idx)
    if (is.logical(species)) {
        if (length(species) != no_sp) {
            stop("The boolean species argument has the wrong length")
        }
    } else {
        species <- dimnames(params@initial_n)$sp %in% species
        if (length(species) == 0) {
            warning("The species argument matches none of the species in the params object")
            return(params)
        }
    }
    keep <- !species
    p <- params
    p@linecolour <- params@linecolour[!(names(params@linecolour) %in%
                                                 params@species_params$species[species])]
    p@linetype <- params@linetype[!(names(params@linetype) %in%
                                             params@species_params$species[species])]
    p@psi <- params@psi[keep, , drop = FALSE]
    p@maturity <- params@maturity[keep, , drop = FALSE]
    p@initial_n <- params@initial_n[keep, , drop = FALSE]
    p@intake_max <- params@intake_max[keep, , drop = FALSE]
    p@search_vol <- params@search_vol[keep, , drop = FALSE]
    p@metab <- params@metab[keep, , drop = FALSE]
    if (length(dim(params@ft_pred_kernel_e)) == 2) {
        p@ft_pred_kernel_e <- params@ft_pred_kernel_e[keep, , drop = FALSE]
    }
    if (length(dim(params@ft_pred_kernel_p)) == 2) {
        p@ft_pred_kernel_p <- params@ft_pred_kernel_p[keep, , drop = FALSE]
    }
    if (length(dim(params@pred_kernel)) == 3) {
        p@pred_kernel <- params@pred_kernel[keep, , , drop = FALSE]
    }
    p@ft_mask <- params@ft_mask[keep, , drop = FALSE]
    p@mu_b <- params@mu_b[keep, , drop = FALSE]
    p@species_params <- params@species_params[keep, , drop = FALSE]
    p@interaction <- params@interaction[keep, keep, drop = FALSE]
    p@selectivity <- params@selectivity[, keep, , drop = FALSE]
    p@catchability <- params@catchability[, keep, drop = FALSE]
    p@w_min_idx <- params@w_min_idx[keep]
    p@A <- params@A[keep]

    # Preserve comments
    for (slot in (slotNames(p))) {
        comment(slot(p, slot)) <- comment(slot(params, slot))
    }

    validObject(p)
    return(p)
}

#' Rescale Abundance
#'
#' Multiplies the abundances of all or of selected species by given factors and
#' then retunes the reproductive efficiencies accordingly.
#'
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

    return(retune_erepro(params))
}

#' Rescale System
#'
#' The abundances in mizer and some rates depend on the size of the area to
#' which they refer. So they could be given per square meter or per square
#' kilometer or for an entire study area or any other choice of yours. This
#' function allows you to change the size by automatically changing the
#' abundances and rates accordingly.
#'
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
    params <- setSearchVolume(params, search_vol = params@search_vol / factor)
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

#' Rename species
#'
#' Changes the names of species in a MizerParams object
#'
#' @param params A mizer params object
#' @param replace A named character vector, with new names as values, and old
#'   names as names.
#'
#' @return An object of type \linkS4class{MizerParams}
#' @export
#' @examples
#' \dontrun{
#' replace <- c(Cod = "Kabeljau", Haddock = "Schellfisch")
#' params <- renameSpecies(NS_params, replace)
#' species_params(params)$species
#' }
renameSpecies <- function(params, replace) {
    params <- validParams(params)
    replace[] <- as.character(replace)
    to_replace <- names(replace)
    species <- as.character(params@species_params$species)
    wrong <- setdiff(names(replace), species)
    if (length(wrong) > 0) {
        stop(paste(wrong, collapse = ", "),
             " do not exist.")
    }
    names(species) <- species
    species[to_replace] <- replace
    names(species) <- NULL
    rownames(params@species_params) <- species
    params@species_params$species <- species
    for (i in seq_len(nrow(params@gear_params))) {
        if (params@gear_params$species[[i]] %in% names(replace)) {
            params@gear_params$species[[i]] <-
                replace[[params@gear_params$species[[i]]]]
        }
    }
    linenames <- names(params@linecolour)
    names(linenames) <- linenames
    linenames[to_replace] <- replace
    names(linenames) <- NULL
    names(params@linecolour) <- linenames
    names(params@linetype) <- linenames
    names(params@w_min_idx) <- species
    dimnames(params@maturity)$sp <- species
    dimnames(params@psi)$sp <- species
    dimnames(params@initial_n)$sp <- species
    dimnames(params@intake_max)$sp <- species
    dimnames(params@search_vol)$sp <- species
    dimnames(params@metab)$sp <- species
    if (length(dim(params@ft_pred_kernel_e)) == 2) {
        dimnames(params@ft_pred_kernel_e)$sp <- species
        dimnames(params@ft_pred_kernel_p)$sp <- species
    } else {
        dimnames(params@pred_kernel)$sp <- species
    }
    dimnames(params@mu_b)$sp <- species
    dimnames(params@interaction)$predator <- species
    dimnames(params@interaction)$prey <- species
    dimnames(params@selectivity)$sp <- species
    dimnames(params@catchability)$sp <- species

    validObject(params)
    return(params)
}


#' Add new species
#'
#' Takes a \linkS4class{MizerParams} object and adds additional species with
#' given parameters to the ecosystem. It sets the initial values for these new
#' species to its steady-state solution in the given initial state of the
#' existing ecosystem. This will be close to the true steady-state if the
#' abundances of the new species are sufficiently low. Hence the abundances of
#' the new species are set so that the maximal biomass density of each new
#' species lies at 1/100 of the community power law. The reproductive
#' efficiencies of the new species are set so as to keep them at that low level.
#'
#' After adding the new species, the background species are not retuned and the
#' system is not run to steady state. You would have to call
#' [retuneBackground()] and [steady()] explicitly.
#'
#' @param params A mizer params object for the original system.
#' @param species_params The species parameters of the new species we
#'   want to add to the system.
#' @param interaction Interaction matrix. A square matrix giving either the
#'   interaction coefficients between all species or only those between the
#'   new species. In the latter case all interaction between an old and a new
#'   species are set to 1. If this argument is missing, all interactions
#'   involving a new species are set to 1.
#'
#' @return An object of type \linkS4class{MizerParams}
#' @seealso [removeSpecies()]
#' @export
#' @examples
#' \dontrun{
#' params <- newTraitParams(max_w_inf = 5000)
#' params <- markBackground(params)
#' a_m <- 0.0085
#' b_m <- 3.11
#' L_inf_m <- 24.3
#' L_mat <- 11.1
#' species_params <- data.frame(
#'     species = "mullet",
#'     w_min = 0.001,
#'     w_inf = a_m*L_inf_m^b_m,
#'     w_mat = a_m*L_mat^b_m,
#'     beta = 283,
#'     sigma = 1.8,
#'     z0 = 0,
#'     alpha = 0.6,
#'     sel_func = "knife_edge",
#'     knife_edge_size = 100,
#'     gear = "knife_edge_gear",
#'     k = 0,
#'     k_vb = 0.6,
#'     a = a_m,
#'     b = b_m
#' )
#' params <- addSpecies(params, species_params)
#' plotSpectra(params)
#' sim <- project(params, t_max=50, progress_bar = FALSE)
#' plotBiomass(sim)
#' }
addSpecies <- function(params, species_params, gear_params = data.frame(),
                       interaction, initial_effort) {
    # check validity of parameters ----
    params <- validParams(params)
    assert_that(is.data.frame(species_params),
                is.data.frame(gear_params))
    species_params <- mizer:::validSpeciesParams(species_params)
    gear_params <- mizer:::validGearParams(gear_params, species_params)
    if (any(species_params$species %in% params@species_params$species)) {
        stop("You can not add species that are already there.")
    }
    no_old_sp <- nrow(params@species_params)
    old_sp <- 1:no_old_sp
    no_new_sp <- nrow(species_params)
    new_sp <- 1:no_new_sp + no_old_sp
    no_sp <- no_old_sp + no_new_sp
    if (missing(interaction)) {
        # keep existing interactions between old species and
        # set interactions involving new species to 1
        inter <- matrix(1, nrow = no_sp, ncol = no_sp)
        inter[old_sp, old_sp] <- params@interaction
    } else if (all(dim(interaction) == c(no_new_sp, no_new_sp))) {
        # keep existing interactions between old species,
        # set interactions involving an old and a new species to 1
        # and use supplied matrix for interaction among new species
        inter <- matrix(1, nrow = no_sp, ncol = no_sp)
        inter[old_sp, old_sp] <- params@interaction
        inter[new_sp, new_sp] <- interaction
    } else if (all(dim(interaction) != c(no_sp, no_sp))) {
        stop("Interaction matrix has invalid dimensions.")
    } else {
        inter <- interaction
    }

    # combine species params ----

    # Move linecolour and linetype into species_params
    params@species_params$linetype <-
        params@linetype[as.character(params@species_params$species)]
    params@species_params$linecolour <-
        params@linecolour[as.character(params@species_params$species)]

    # Make sure that all columns exist in both data frames
    missing <- setdiff(names(params@species_params), names(species_params))
    species_params[missing] <- NA
    missing <- setdiff(names(species_params), names(params@species_params))
    params@species_params[missing] <- NA

    # add the new species (with parameters described by species_params),
    # to make a larger species_params dataframe.
    combi_species_params <- rbind(params@species_params, species_params,
                                  stringsAsFactors = FALSE)

    # combine gear params ----
    # Make sure that all columns exist in both data frames
    if (nrow(gear_params) > 0) {
        missing <- setdiff(names(params@gear_params), names(gear_params))
        gear_params[missing] <- NA
    }
    if (nrow(params@gear_params) > 0) {
        missing <- setdiff(names(gear_params), names(params@gear_params))
        params@gear_params[missing] <- NA
    }
    combi_gear_params <- rbind(params@gear_params, gear_params,
                               stringsAsFactors = FALSE)

    # new params object ----
    # use dataframe and global settings from params to make a new MizerParams
    # object.
    p <- newMultispeciesParams(
        combi_species_params,
        interaction = inter,
        min_w = min(params@w),
        max_w = max(params@w),
        min_w_pp = min(params@w_full),
        no_w = length(params@w),
        gear_params = combi_gear_params,
        n = params@resource_params$n,
        r_pp = params@resource_params$r_pp,
        kappa = params@resource_params$kappa,
        lambda = params@resource_params$lambda,
        w_pp_cutoff = params@resource_params$w_pp_cutoff
    )
    # Copy over old effort
    p@initial_effort[names(params@initial_effort)] <- params@initial_effort
    if (!missing(initial_effort)) {
        if (is.null(names(initial_effort))) {
            stop("The `initial_effort` must be a named list or vector.")
        }
        if (!all(names(initial_effort) %in% dimnames(p@selectivity)$gear)) {
            stop("The names of the `initial_effort` do not match the names of gears.")
        }
        p@initial_effort[names(initial_effort)] <- initial_effort
    }
    # Use the same resource spectrum as params
    p@initial_n_pp <- params@initial_n_pp
    p@cc_pp <- params@cc_pp
    p@rr_pp <- params@rr_pp
    p@resource_dynamics <- params@resource_dynamics
    p@resource_params <- params@resource_params
    # Preserve comments
    comment(p) <- comment(params)
    for (slot in (slotNames(p))) {
        comment(slot(p, slot)) <- comment(slot(params, slot))
    }

    # initial solution ----
    p@initial_n[old_sp, ] <- params@initial_n
    p@A[old_sp] <- params@A
    # Use the same psi and mu_b as before for old species
    p@psi[old_sp, ] <- params@psi
    p@sc <- params@sc
    p@mu_b[old_sp, ] <- params@mu_b
    # we assume same background death for all species
    p@mu_b[new_sp, ] <- rep(params@mu_b[1, ], each = no_new_sp)

    # Turn off self-interaction among the new species, so we can determine the
    # growth rates, and death rates induced upon them by the pre-existing species
    p@interaction[new_sp, new_sp] <- 0
    mumu <- getMort(p)
    gg <- getEGrowth(p)

    # Compute solution for new species
    for (i in new_sp) {
        g <- gg[i, ]
        mu <- mumu[i, ]
        w_inf_idx <- sum(p@w < p@species_params$w_inf[i])
        idx <- p@w_min_idx[i]:(w_inf_idx - 1)
        if (any(g[idx] == 0)) {
            stop("Can not compute steady state due to zero growth rates for ",
                 p@species_params$species[i])
        }
        p@initial_n[i, ] <- 0
        p@initial_n[i, p@w_min_idx[i]:w_inf_idx] <-
            c(1, cumprod(g[idx] / ((g + mu * p@dw)[idx + 1])))

        # set low abundance ----
        # Normalise solution so that at its maximum it lies at 1/100 of the
        # Sheldon spectrum.
        # We look at the maximum of abundance times w^lambda
        # because that is always an increasing function at small size.
        idx <- which.max(p@initial_n[i, ] * p@w^p@resource_params$lambda)
        p@initial_n[i, ] <- p@initial_n[i, ] *
            p@resource_params$kappa * p@w[idx]^(-p@resource_params$lambda) / p@initial_n[i, idx] / 100
        p@A[i] <- sum(p@initial_n[i, ] * p@w * p@dw * p@maturity[i, ])
    }

    if (any(is.infinite(p@initial_n))) {
        stop("Candidate steady state holds infinities.")
    }
    if (any(is.na(p@initial_n) | is.nan(p@initial_n))) {
        stop("Candidate steady state holds non-numeric values.")
    }

    # Turn self interaction back on
    p@interaction[new_sp, new_sp] <- inter[new_sp, new_sp]

    # Retune reproductive efficiencies of new species
    p <- retune_erepro(p, p@species_params$species[new_sp])

    return(p)
}

#' Update the initial values
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
