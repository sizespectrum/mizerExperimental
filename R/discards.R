
#' Plot the size distribution of the catch
#'
#' Plots the normalised number density of the catch for a species as a function
#' of either length or weight. In addition to the catch in the model, the
#' observed catch will be plotted if it is supplied via the `catch` argument.
#' Also superimposes a plot of the number density of all individuals of the
#' species.
#'
#' The observed catch is provided via the `catch` data frame. The catch data can
#' be binned either into length bins or weight bins. In the former case the data
#' frame should have columns \code{length} and \code{dl} holding the start of
#' the size bins in cm and the width of the size bins in cm respectively. In the
#' latter case the data frame should have columns \code{weight} and \code{dw}
#' holding the start of the size bins in grams and the width of the size bins in
#' grams. 
#' 
#' The `catch` data frame also needs to have the columns \code{species} (the
#' name of the species), \code{gear} (the name of the gear) and \code{catch}
#' (the number of individuals of a particular species caught by a particular
#' gear in a size bin).
#' 
#' Optionally, the `catch` data frame can have an additional column called
#' `discarded`, which hold the number of individuals of a
#' particular species that were discarded by a particular gear in a
#' size bin. If such discard data is available, then the function will plot
#' three curves representing the observed data: "Observed catch", "Observed
#' discards" and "Observed landings", where the landings and the discards add
#' up to the observed catch.
#' 
#' @param object An object of class \linkS4class{MizerSim} or
#'   \linkS4class{MizerParams}.
#' @param species The name of the species for which to plot the catches.
#' @param gears Optional. The name of a gear or a list of names of gear. If
#'   supplied, only the catches from these gears will be included. Otherwise
#'   catches from all gears will be included.
#' @param catch Data frame holding binned observed catch data. See Details,
#' @param x_var Determines whether to show the size distribution of the catch as
#'   a function of weight ("Weight") or as a function of length ("Length").
#'   Default is "Weight".
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a list
#' composed of two slots is returned. First slot is a data frame with the four
#' variables 'w' or 'l' (depending on `x_var`), 'Catch density', 'Type', 'Species
#' and the second slot is a data frame with the four variables 'w_mat',
#' 'Species', 'y_coord', 'Type' (to plot vertical lines).
#' @export
#' @family plotting functions
#' @examples
#' \donttest{
#' plotCatchVsSize(NS_params, species = "Cod")
#'
#' # Returning the data frame
#' fr <- plotCatchVsSize(NS_params, species = "Cod", return_data = TRUE)
#' str(fr)
#' }
plotCatchVsSize <- function(object, species = NULL, gears = NULL, catch = NULL,
                            x_var = c("Weight", "Length"),
                            return_data = FALSE) {
    
    # Check arguments ----
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    
    gears <- valid_gears_arg(params, gears, error_on_empty = TRUE)
    
    x_var = match.arg(x_var)
    if (!is.null(catch)) {
        assert_that(is.data.frame(catch),
                    "catch" %in% names(catch),
                    "species" %in% names(catch),
                    "gear" %in% names(catch),
                    all(c("length", "dl") %in% names(catch)) |
                        all(c("weight", "dw") %in% names(catch)))
        catch <- filter(catch, gear %in% gears)
    }
    
    # Prepare model data ----
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    species <- valid_species_arg(params, species)
    if (length(species) != 1) {
        stop("You must select a single species")
    }
    species <- which(params@species_params$species == species)
    
    params <- set_species_param_default(params, "a", 0.006)
    params <- set_species_param_default(params, "b", 3)
    
    retain_prob <- getRetainProbGear(params)
    f_mort <- getFMortGear(params)
    retained <- f_mort * retain_prob
    discarded <- f_mort * (1 - retain_prob)
    # Sum over desired gears and multiply by abundance
    retained <- colSums(retained[gears, , , drop = FALSE]) * params@initial_n
    discarded <- colSums(discarded[gears, , , drop = FALSE]) * params@initial_n
    
    # Loop over species ----
    for (iSpecies in species) {
        s <- params@species_params$species[[iSpecies]]
        a <- params@species_params[iSpecies, "a"]
        b <- params@species_params[iSpecies, "b"]
        
        # Check whether we have enough catch data for this species to plot it
        is_observed <- sum(catch$species == s) > 3
        
        # Choose size range ----
        # To choose the range of sizes over which to plot we look at the range
        # of sizes for which a non-zero catch was observed. If no catch was
        # observed for the species, we use the range from w_mat/100 to w_max.
        if (is_observed) {
            if ("length" %in% names(catch)) {
                l_min = min(catch$length[catch$species == s])
                w_min = a * l_min ^ b
                l_max = max(catch$length[catch$species == s])
                w_max = a * l_max ^ b
            } else {
                w_min = min(catch$weight[catch$species == s])
                w_max = max(catch$weight[catch$species == s])
            }
            w_min_idx <- sum(params@w < w_min)
            w_max_idx <- min(length(params@w), sum(params@w <= w_max) + 1)
        } else {
            w_min_idx <- sum(params@w < (params@species_params$w_mat[[iSpecies]] / 100))
            w_max_idx <- sum(params@w <= params@species_params$w_max[[iSpecies]])
        }
        w_sel <- seq(w_min_idx, w_max_idx, by = 1)
        w <- params@w[w_sel]
        l = (params@w[w_sel] / a) ^ (1 / b)
        
        # Process model catches ----
        retained_w <- retained[iSpecies, w_sel]
        discarded_w <- discarded[iSpecies, w_sel]
        catch_w <- retained_w + discarded_w
        # We just want the distribution: rescale the densities so area is 1
        if (sum(catch_w) > 0) {
            total <- sum(catch_w * params@dw[w_sel])
            retained_w <- retained_w / total
            discarded_w <- discarded_w / total
            catch_w <- catch_w / total
        }
        # The catch density in l gets an extra factor of dw/dl
        dwdl <- b * w / l
        retained_l <- retained_w * dwdl
        discarded_l <- discarded_w * dwdl
        catch_l <- catch_w * dwdl
        # Construct data frame
        plot_dat <- data.frame(w, l, catch_w, catch_l, Type = "Model catch")
        if (any(discarded_w > 0)) {
            # Need to show retained and discarded only if there are discards.
            df_retained <- data.frame(w, l, catch_w = retained_w, 
                                     catch_l = retained_l, 
                                     Type = "Model landings")
            df_discarded <- data.frame(w, l, catch_w = discarded_w,
                                     catch_l = discarded_l,
                                     Type = "Model discards")
            plot_dat <- rbind(plot_dat, df_retained, df_discarded)
        }
        
        # Add abundance density ----
        # We also include the abundance density because that helps to understand
        # the catch density
        abundance_w <- params@initial_n[iSpecies, w_sel]
        # We just want the distribution, so we rescale the density so its area is 1
        abundance_w <- abundance_w / sum(abundance_w * params@dw[w_sel])
        # The catch density in l gets an extra factor of dw/dl
        abundance_l <- abundance_w * b * w / l
        abundance <- data.frame(w, l, catch_w = abundance_w,
                                catch_l = abundance_l,
                                Type = "Model abundance")
        
        # Process observed catches ----
        if (is_observed) {
            has_discards = ("discarded" %in% names(catch))
            sel <- (catch$species == s)
            if ("length" %in% names(catch)) {
                l <- catch$length[sel]
                dl <- catch$dl[sel]
                catch_l <- catch$catch[sel]
                if (has_discards) {
                    discarded_l <- catch$discarded[sel]
                    # normalise to a density in l
                    discarded_l <- discarded_l / sum(catch_l * dl)
                }
                catch_l <- catch_l / sum(catch_l * dl)
                # To get the density in w we need to divide by dw/dl
                w <- a * l ^ b
                catch_w <- catch_l / b * l / w
                if (has_discards) {
                    discarded_w <- discarded_l / b * l / w
                }
            } else {
                w <- catch$weight[sel]
                dw <- catch$dw[sel]
                catch_w <- catch$catch[sel]
                if (has_discards) {
                    discarded_w <- catch$discarded[sel]
                    # normalise to a density in w
                    discarded_w <- discarded_w / sum(catch_w * dw)
                }
                # normalise to a density in w
                catch_w <- catch_w / sum(catch_w * dw)
                # To get the density in l we need to divide by dl/dw
                l <- (w / a)^(1/b)
                catch_l <- catch_w * b / l * w
                if (has_discards) {
                    discarded_l <- discarded_w * b / l * w
                }
            }
            plot_dat <- rbind(plot_dat, 
                              data.frame(w, l, catch_w, catch_l,
                                         Type = "Observed catch"))
            if (has_discards) {
                retained_w <- catch_w - discarded_w
                retained_l <- catch_l - discarded_l
                plot_dat <- rbind(plot_dat, 
                                  data.frame(w, l, catch_w = discarded_w,
                                             catch_l = discarded_l,
                                             Type = "Observed discards"),
                                  data.frame(w, l, catch_w = retained_w,
                                             catch_l = retained_l,
                                             Type = "Observed landings"))
            }
        }
        
        # Prune abundance density ----
        # From the abundance only keep values that are no larger than
        # the maximum of the other shown densities.
        if (x_var == "Weight") {
            abundance <- subset(abundance, catch_w < max(plot_dat$catch_w))
        } else {
            abundance <- subset(abundance, catch_l < max(plot_dat$catch_l))
        }
        # Add the abundance to the data frame last so that it shows up
        # last also in legend
        plot_dat <- rbind(plot_dat, abundance)
        plot_dat$Species <- 
            SpIdx[which(params@species_params$species[iSpecies] == SpIdx)]
    } # End of loop over species
    
    # Add legends ----
    params <- setColours(params, list("Model landings" = "#F8766D",
                                      "Observed landings" = "#00BFCFC4",
                                      "Model discards" = "#F8766D",
                                      "Observed discards" = "#00BFCFC4",
                                      "Model catch" = "#F8766D",
                                      "Observed catch" = "#00BFCFC4",
                                      "Model abundance" = "grey"))
    params <- setLinetypes(params, list("Model landings" = "dashed",
                                        "Observed landings" = "dashed",
                                        "Model discards" = "dotted",
                                        "Observed discards" = "dotted",
                                        "Model catch" = "solid",
                                        "Observed catch" = "solid",
                                        "Model abundance" = "solid"))
    
    # Create weight-based plot ----
    if (x_var == "Weight") {
        ## Add vertical line for maturity size ----
        sizeVline <- data.frame(
            w_mat = params@species_params[species, "w_mat"],
            y_coord = plot_dat %>%
                group_by(Species) %>%
                summarise(Value = max(catch_w)),
            Type = NA) # geom_text wants a group var for some reasons
        colnames(sizeVline)[2:3] <- c("Species", "y_coord")
        
        # remove length-related columns
        plot_dat <- plot_dat[, -c(2, 4)]
        colnames(plot_dat)[2] <- "Catch density"
        
        if (return_data) return(list(plot_dat, sizeVline))
        
        pl <- plotDataFrame(plot_dat, params, wrap_var = "Species",
                            xlab = "Size [g]",
                            ylab = "Normalised number density",
                            wrap_scale = "free")
        
        pl <- pl +
            geom_vline(data = sizeVline,
                       aes(xintercept = w_mat, group = Species),
                       linetype = "dotted") +
            geom_text(data = sizeVline, aes(x = w_mat, y = y_coord * 0.9,
                                            label = "\nMaturity"))
    } else {
        # Create length-based plot ----
        sizeVline <- data.frame(
            w_mat = (params@species_params[species, "w_mat"] / a) ^ (1 / b),
            y_coord = plot_dat %>%
                group_by(Species) %>%
                summarise(Value = max(catch_l)),
            Type = NA) # geom_text wants a group var for some reasons
        
        # remove weight-related columns
        plot_dat <- plot_dat[,-c(1,3)]
        colnames(plot_dat)[2] <- "Catch density"
        colnames(sizeVline)[2:3] <- c("Species", "y_coord")
        
        if (return_data) return(list(plot_dat, sizeVline))
        
        pl <- plotDataFrame(plot_dat, params, wrap_var = "Species",
                            xlab = "Size [cm]",
                            ylab = "Normalised number density",
                            wrap_scale = "free")
        
        pl <- pl +
            geom_vline(data = sizeVline,
                       aes(xintercept = w_mat, group = Species),
                       linetype = "dotted") +
            geom_text(data = sizeVline, aes(x = w_mat, y = y_coord * 0.9,
                                            label = "\nMaturity"))
    }
    if (length(gears) < length(unique(params@gear_params$gear))) {
        pl <- pl + ggtitle(paste("Gears included:", gears, collapse = ", "))
    }
    return(pl)
}

#' @rdname plotCatchVsSize
#' @export
plotlyCatchVsSize <- function(object,
                              species = NULL,
                              gear = NULL,
                              catch = NULL,
                              x_var = c("Weight", "Length"),
                              ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotCatchVsSize", argg),
             tooltip = c("Catch density", "Type", "w", "l"))
}

# Helper functions ####

#' Calculate the proportion of fish retained instead of discarded
#' 
#' This function calculates the proportion of fish retained instead of discarded
#' by a fishing gear. The retention probability is given by a sigmoidal
#' function.
#'
#' The probability that a fish of length \eqn{l} is retained is given by the
#' logistic function \deqn{p(l) = \frac{1}{1 + \exp\left(\log(3)\frac{l50
#' -l}{l50 - l25}\right)}}{p(l) = 1/(1 + exp(log(3)*(l50 -l) / (l50 - l25)))}
#' where \eqn{l_{50}} and \eqn{l_{25}} are the parameters of the sigmoid. The
#' sigmoid is defined such that 50% of the fish are retained at length
#' \eqn{l_{50}} and 25% of the fish are retained at length \eqn{l_{25}}. For
#' each species-gear pair, these parameters are stored in the `gear_params` data
#' frame in the `retain_l50` and `retain_l25` columns. If the `retain_l25` value
#' is missing, it is assumed to be 1.5 times the `retain_l50` value. If the
#' `retain_l50` value is also missing, the retain probability is set to zero.
#'
#' As the mizer model is weight based, and this selectivity function is length
#' based, it uses the length-weight parameters `a` and `b` to convert between
#' length and weight \deqn{l = \left(\frac{w}{a}\right)^{1/b}}{l = (w/a)^(1/b)}
#' 
#' @param params A `MizerParams` object
#' 
#' @return A matrix (gear x species x size) with the retention probability.
#' @export
#' @concept helper
#' @examples
#' retain_prob <- getRetainProb(NS_params)
#' retain_prob["Pelagic", "Herring", 100]
getRetainProbGear <- function(params) {
    assert_that(is(params, "MizerParams"))
    species_params <- params@species_params
    gear_params <- params@gear_params
    sp_names <- as.character(species_params$species)
    no_sp <- length(sp_names)
    w_names <- dimnames(params@selectivity)[[3]]
    w <- params@w
    no_w <- length(w)
    gear_names <- as.character(unique(gear_params$gear))
    no_gears <- length(gear_names)
    
    retain_prob <- 
        array(1, dim = c(no_gears, no_sp, no_w),
              dimnames = list(gear = gear_names, 
                              sp = sp_names,
                              w = w_names
              )
        )
    if (!("retain_l50" %in% names(gear_params))) {
        return(retain_prob)
    }
    retain_l50 <- gear_params$retain_l50
    retain_l50[is.na(retain_l50)] <- Inf
    if (!("retain_l25" %in% names(gear_params))) {
        retain_l25 <- 1.5 * retain_l50
    } else {
        retain_l25 <- gear_params$retain_l25
    }
    retain_l25[is.na(retain_l25)] <- 1.5 * retain_l50[is.na(retain_l25)]
    for (g in seq_len(nrow(gear_params))) {
        species <- as.character(gear_params[g, "species"])
        gear <- as.character(gear_params[g, "gear"])
        retain_prob[gear, species, ] <- 
            retain_sigmoid(w, retain_l25[g], retain_l50[g],
                           species_params = as.list(species_params[species, ]))
    }
    return(retain_prob)
}

# helper function used by calc_retain_prob
# Evaluates a sigmoid function at the given weights
retain_sigmoid <- function(w, l25, l50, species_params) {
    assert_that(is.numeric(l25) && is.numeric(l50))
    if (l50 == Inf) {
        return(rep(1, length(w)))
    }
    if (l50 >= l25) {
        stop("Error in your gear parameters: The value for `retain_l25` must always be larger than that for `retain_l50`")
    }
    if (l50 < 0) {
        stop("Error in your gear parameters: The value for `retain_l50` must be non-negative")
    }
    a <- species_params[["a"]]
    b <- species_params[["b"]]
    if (is.null(a) || is.null(b)) {
        stop("The weight-length parameters `a` and `b` need ",
             "to be provided in the species_params data frame.")
    }
    l <- (w / a)^(1 / b)
    sr <- l50 - l25
    s1 <- l50 * log(3) / sr
    s2 <- s1 / l50
    return(1 / (1 + exp(s1 - s2 * l)))
}

#' Helper function to assure validity of `species`gear`` argument
#'
#' If the `gear` argument contains invalid gears, then these are ignored but a
#' warning is issued.
#'
#' @param object A MizerSim or MizerParams object from which the gears should be
#'   selected.
#' @param gears The gears to be selected. Optional. By default all gears are
#'   selected. A gear name or a vector of gear names.
#' @param error_on_empty Whether to throw an error if there are zero valid
#'   gears. Default FALSE.
#'
#' @return A vector of gear names, in the same order as specified in the 'gear'
#'   argument.
#' @export
#' @concept helper
valid_gears_arg <- function(object, gears = NULL, error_on_empty = FALSE) {
    if (is(object, "MizerSim")) {
        params <- object@params
    } else if (is(object, "MizerParams")) {
        params <- object
    } else {
        stop("The first argument must be a MizerSim or MizerParams object.")
    }
    assert_that(is.flag(error_on_empty))
    all_gears <- unique(params@gear_params$gear)
    # Set gear if missing to list of all gears
    if (is.null(gears)) {
        gears <- all_gears
    }
    invalid <- setdiff(gears, all_gears)
    if (length(invalid) > 0) {
        warning("The following gears do not exist: ", 
                toString(invalid), ".")
    }
    gears <- intersect(gears, all_gears)
    if (length(gears) == 0 && error_on_empty) {
        stop("No gears have been selected.")
    }
    gears
}
