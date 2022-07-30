# Plotting functions ----

# Hackiness to get past the 'no visible binding ... ' warning when running check
utils::globalVariables(c("w_inf","y_coord"))

#' Make a plot from a data frame
#'
#' This is used internally by most plotting functions.
#'
#' @param frame A data frame with at least three variables.
#'   The first three variables are used, in that order, as:
#'   1. Variable to be plotted on x-axis
#'   2. Variable to be plotted on y-axis
#'   3. Grouping variable
#' @param params A MizerParams object, which is used for the line colours and
#'   line types.
#' @param style The style of the plot. Availalble options are "line' for geom_line
#' and "area" for geom_area. Default is "line".
#' @param legend_var The name of the variable that should be used in the legend
#'   and to determine the line style. If NULL then the grouping variable is
#'   used for this purpose.
#' @param wrap_var Optional. The name of the variable that should be used for
#'  creating wrapped facets.
#' @param wrap_scale Optional. Used to pass the scales argument to facet_wrap().
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param xtrans Transformation for the x-axis. Often "log10" may be useful
#'   instead of the default of "identity".
#' @param ytrans Transformation for the y-axis.
#' @param y_ticks The approximate number of ticks desired on the y axis
#' @param highlight Name or vector of names of the species to be highlighted.
#' @keywords internal
#' @export
plotDataFrame <- function(frame, params, style = "line", xlab = waiver(),
                          ylab = waiver(), xtrans = "identity", ytrans = "identity",
                          y_ticks = 6, highlight = NULL, legend_var = NULL,
                          wrap_var = NULL, wrap_scale = NULL) {
    assert_that(is.data.frame(frame),
                is(params, "MizerParams"))
    if (ncol(frame) < 3) {
        stop("The data frame needs to have at least 3 variables.")
    }

    var_names <- names(frame)
    x_var <- var_names[[1]]
    y_var <- var_names[[2]]
    group_var <- var_names[[3]]
    if (is.null(legend_var)) {
        frame$Legend <- frame[[group_var]]
        legend_var <- "Legend"
    } else {
        if (!(legend_var %in% var_names)) {
            stop("The `legend_var` argument must be the name of a variable ",
                 "in the data frame.")
        }
    }
    # Need to keep species in order for legend
    legend_levels <-
        intersect(names(params@linecolour), frame[[legend_var]])
    frame[[legend_var]] <- factor(frame[[legend_var]], levels = legend_levels)

    if (sum(is.na(frame$Legend))) {
        warning("missing legend in params@linecolour, some groups won't be displayed")
    }

    linecolour <- params@linecolour[legend_levels]
    linetype <- params@linetype[legend_levels]
    linesize <- rep_len(0.8, length(legend_levels))
    names(linesize) <- legend_levels
    linesize[highlight] <- 1.6

    xbreaks <- waiver()
    if (xtrans == "log10") xbreaks <- log_breaks()
    ybreaks <- waiver()
    if (ytrans == "log10") ybreaks <- log_breaks(n = y_ticks)

    # The reason why below `group = species` is included in `ggplot()`
    # rather than in `geom_line` is because that puts it first in the
    # plotly tooltips, due to a bug in plotly.
    p <- ggplot(frame, aes(group = .data[[group_var]])) +
        scale_y_continuous(trans = ytrans, breaks = ybreaks,
                           labels = prettyNum, name = ylab) +
        scale_x_continuous(trans = xtrans, name = xlab)

    switch(style,
            "line" = {p <- p +
                geom_line(aes(x = .data[[x_var]], y = .data[[y_var]],
                              colour = .data[[legend_var]],
                              linetype = .data[[legend_var]],
                              size = .data[[legend_var]])) +
                scale_colour_manual(values = linecolour) +
                scale_linetype_manual(values = linetype) +
                scale_size_manual(values = linesize)
            },
            "area" = {p <- p +
                geom_area(aes(x = .data[[x_var]], y = .data[[y_var]],
                              fill = .data[[legend_var]])) +
                scale_fill_manual(values = linecolour)
            },
            {"unknown style selected"}
    )

    if (!is.null(wrap_var)) {
        if (!(wrap_var %in% var_names)) {
            stop("The `wrap_var` argument must be the name of a variable ",
                 "in the data frame.")
        }
        p <- p + facet_wrap(wrap_var, scales = wrap_scale)
    }
    p
}


#' Plot the sources of external, predation and fishing mortality
#' per species and size

#' @param object An object of class \linkS4class{MizerSim} or
#'   \linkS4class{MizerParams}.
#' @param species The name of the predator species for which to plot the
#'   mortality.
#' @param proportion A boolean value that determines whether values should be
#'   displayed as proportions from 0 to 1 or with their actual values. Default
#'   is TRUE.
#' @param return_data A boolean value that determines whether the formatted data
#'   used for the plot is returned instead of the plot itself. Default value is
#'   FALSE
#' @param ... Other arguments (currently unused)
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'w', 'value', 'Cause', 'Species' is returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotDeath(NS_params, species = "Cod")
#'
#' # Returning the data frame
#' fr <- plotDeath(NS_params, species = "Cod", return_data = TRUE)
#' str(fr)
#' }
plotDeath <- function(object, species = NULL, proportion = TRUE,
                      return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    # TODO: Remove this fix once core mizer makes sure a default is set
    if (!"External" %in% names(getColours(params))) {
        params <- setColours(params, c(External = "grey"))
    }
    if (!"Fishing" %in% names(getColours(params))) {
        params <- setColours(params, c(Fishing = "red"))
    }

    species <- valid_species_arg(params, species)

    pred_rate <- getPredRate(params)
    f_mort <- getFMort(params)
    mort <- getMort(params)
    plot_dat <- NULL
    for (iSpecies in species) {
        fish_idx_full <- (params@w_full >= params@species_params[iSpecies, "w_min"]) &
            (params@w_full <= params@species_params[iSpecies, "w_inf"])
        fish_idx <- (params@w >= params@species_params[iSpecies, "w_min"]) &
            (params@w <= params@species_params[iSpecies, "w_inf"])
        predation <- params@interaction[, iSpecies] *
            pred_rate[, fish_idx_full]
        fishing <- f_mort[iSpecies, fish_idx]
        external <- ext_mort(params)[iSpecies, fish_idx]
        total <- mort[iSpecies, fish_idx]
        ylab <- "Death rate [1/year]"
        if (proportion) {
            predation <- predation / rep(total, each = dim(predation)[[1]])
            external <- external / total
            fishing <- fishing / total
            ylab <- "Proportion of all death"
        }
        # Make data.frame for plot
        plot_dat <-
            rbind(plot_dat,
                  data.frame(w = params@w[fish_idx],
                             value = external,
                             Cause = "External",
                             Prey = iSpecies),
                  data.frame(w = params@w[fish_idx],
                             value = fishing,
                             Cause = "Fishing",
                             Prey = iSpecies),
                  data.frame(w = rep(params@w[fish_idx],
                                     each = dim(predation)[[1]]),
                             value = c(predation),
                             Cause = params@species_params$species,
                             Prey = iSpecies)
            )
    }

    if (return_data) return(plot_dat)

    plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                  wrap_var = "Prey", wrap_scale = "free_x",
                  xlab = "Size [g]", ylab = ylab)
}


#' @rdname plotDeath
#' @export
plotlyDeath <- function(object,
                        species = NULL,
                        proportion = TRUE,
                        ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotDeath", argg),
             tooltip = c("value", "Cause", "w"))
}

#' Plot the mortality applied on the resource spectrum(s)
#'
#' @inheritParams plotDeath
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'w', 'value', 'Predator', 'Resource' is
#'   returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotResourcePred(NS_params)
#'
#' # Returning the data frame
#' fr <- plotResourcePred(NS_params, return_data = TRUE)
#' str(fr)
#' }
plotResourcePred <- function(object, proportion = TRUE, return_data = FALSE)
{
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)

    select <- (params@cc_pp > 0)
    pred_rate <- params@species_params$interaction_resource *
        getPredRate(params)[, select]
    total <- colSums(pred_rate)
    ylab <- "Death rate [1/year]"
    if (proportion) {
        pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
        ylab = "Proportion of predation"
    }
    # Make data.frame for plot
    plot_dat <- data.frame(
        w = rep(params@w_full[select], each = dim(pred_rate)[[1]]),
        value = c(pred_rate),
        Predator = SpIdx
    )
    if (return_data) return(plot_dat)

    plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                  xlab = "Resource size [g]", ylab = ylab)
}

#' @rdname plotResourcePred
#' @export
plotlyResourcePred <- function(object,
                               proportion = TRUE,
                               ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotResourcePred", argg),
             tooltip = c("value", "Predator", "w"))
}


#' Plot the proportion of the resource spectrum(s) compared to
#' their carrying capacity
#'
#' @inheritParams plotDeath
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the three variables 'w', 'value', 'Resource' is returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotResourceLevel(NS_params)
#'
#' # Returning the data frame
#' fr <- plotResourceLevel(NS_params, return_data = TRUE)
#' str(fr)
#' }
plotResourceLevel <- function(object, return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }

    select <- (params@cc_pp > 0)
    plot_dat <- data.frame(
        w = params@w_full[select],
        value = params@initial_n_pp[select] / params@cc_pp[select],
        Resource = "Resource" # 3rd var for plotDataFrame()
    )
    if (return_data) return(plot_dat)

    plotDataFrame(plot_dat, params, xtrans = "log10",
                  xlab = "Resource size [g]",
                  ylab = "Proportion of carrying capacity")
}



#' Plot the energy budget of each species through size.
#' This budget is divided between growth, income, metabolic loss and reproduction.
#'
#' @inheritParams plotDeath
#' @param logarithmic A boolean value that determines whether values should be
#' displayed logarithmicly or linearly. Default is TRUE.
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a list composed of
#' two slots is returned. First slot is a data frame with the four variables 'w', 'value',
#' 'Type', 'Species and the second slot is a data frame with the five variables 'w_mat',
#' 'w_inf', 'Species', 'y_coord', 'Type' (to plot vertical lines).
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotEnergyBudget(NS_params, species = "Cod")
#'
#' # Returning the data frame
#' fr <- plotEnergyBudget(NS_params, return_data = TRUE)
#' str(fr)
#' }
plotEnergyBudget <- function(object , species = NULL, logarithmic = TRUE,
                             return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    species <- valid_species_arg(params,species)

    plot_dat <- NULL
    for (iSpecies in species) {
        max_w <- params@species_params[iSpecies, "w_inf"]
        if (logarithmic) {
            min_w <- params@species_params[iSpecies, "w_min"]
        } else {
            min_w = params@species_params[iSpecies, "w_mat"] / 10 # min(1, params@species_params[iSpecies, "w_min"])
        }
        sel <- params@w >= min_w & params@w <= max_w
        len <- sum(sel)
        growth <- getEGrowth(params)[iSpecies, sel]
        growth_and_repro <- getEReproAndGrowth(params)[iSpecies, sel]
        metab <- params@metab[iSpecies, sel]
        income <- growth_and_repro + metab
        repro <- growth_and_repro - growth

        plot_dat <- rbind(plot_dat,
                          data.frame(
                              w = rep(params@w[sel], 4),
                              value = c(growth, income, metab, repro),
                              Type = c(rep("Growth", len),
                                       rep("Income", len),
                                       rep("Metabolic loss", len),
                                       rep("Reproduction", len)),
                              Species = SpIdx[which(iSpecies == SpIdx)])
        )
    }

    if (logarithmic) xtrans = "log10" else xtrans = "identity"

    # adding legends to params object
    params <- setColours(params, list("Growth" = "#F8766D",
                                      "Income" = "#7CAE00",
                                      "Metabolic loss" = "#00BFCFC4",
                                      "Reproduction" = "#C77CFF"))

    sizeVline <- data.frame(
        w_mat = params@species_params[species, "w_mat"],
        w_inf = params@species_params[species, "w_inf"],
        y_coord = plot_dat %>% group_by(Species) %>% summarise(Value = max(value)),
        Type = NA) # geom_text wants a group var for some reason
    colnames(sizeVline)[3:4] <- c("Species", "y_coord")

    if (return_data) return(list(plot_dat, sizeVline))

    pl <- plotDataFrame(plot_dat, params, style = "area", xlab = "Size [g]",
                        ylab = "Rate [g/year]", xtrans = xtrans,
                        wrap_var = "Species", wrap_scale = "free")

    pl <- pl +
        geom_vline(data = sizeVline, aes(xintercept = w_mat, group = Species),
                   linetype = "dotted") +
        geom_vline(data = sizeVline, aes(xintercept = w_inf, group = Species),
                   linetype = "dotted") +
        geom_text(data = sizeVline, aes(x = w_mat, y = y_coord * 0.2,
                                        label = "\nMaturity"), angle = 90) +
        geom_text(data = sizeVline, aes(x = w_inf, y = y_coord * 0.2,
                                        label = "\nMaximum"), angle = 90)

    return(pl)
}

#' @rdname plotEnergyBudget
#' @export
plotlyEnergyBudget <- function(object,
                               species = NULL,
                               logarithmic = TRUE,
                               ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotEnergyBudget", argg),
             tooltip = c("value", "Type", "w"))
}



#' Plot the size distribution of the catch
#'
#' Plots the normalised number density of the catch for a species as a function
#' of either length or weight. In addition to the catch in the model, also the
#' observed catch will be plotted if it is supplied via the `catch` argument.
#' Also superimposes a plot of the number density of all individuals of the
#' species.
#' @inheritParams plotDeath
#' @param catch  Optional. Data frame holding binned observed catch data. The data can
#'   be binned either into length bins or weight bins. In the former case the data
#'   frame should have columns \code{length} and \code{dl} holding the start of
#'   the size bins in cm and the width of the size bins in cm respectively. In
#'   the latter case the data frame should have columns \code{weight} and
#'   \code{dw} holding the start of the size bins in grams and the width of the
#'   size bins in grams. The data frame also needs to have the columns
#'   \code{species} (the name of the species), \code{catch} (the number of
#'   individuals of a particular species caught in a size bin).
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
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotYieldVsSize(NS_params, species = "Cod")
#'
#' # Returning the data frame
#' fr <- plotYieldVsSize(NS_params, species = "Cod", return_data = TRUE)
#' str(fr)
#' }
plotYieldVsSize <- function(object, species = NULL, catch = NULL,
                            x_var = c("Weight", "Length"),
                            return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }

    x_var = match.arg(x_var)

    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    species <- valid_species_arg(params,species)
    species <- which(params@species_params$species %in% species)

    params <- set_species_param_default(params, "a", 0.006)
    params <- set_species_param_default(params, "b", 3)

    plot_dat <- NULL
    for (iSpecies in species) {
        s <- params@species_params$species[[iSpecies]]
        a <- params@species_params[iSpecies, "a"]
        b <- params@species_params[iSpecies, "b"]

        # Check whether we have enough catch data for this species to plot it
        is_observed <- sum(catch$species == s) > 3

        # To choose the range of sizes over which to plot we look at the range
        # of sizes for which a non-zero catch was observed. If no catch was
        # observed for the species, we use the range from w_mat/100 to w_inf.
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
            w_max_idx <- sum(params@w <= w_max)
        } else {
            w_min_idx <- sum(params@w < (params@species_params$w_mat[[iSpecies]] / 100))
            w_max_idx <- sum(params@w <= params@species_params$w_inf[[iSpecies]])
        }
        w_sel <- seq(w_min_idx, w_max_idx, by = 1)
        w <- params@w[w_sel]
        l = (params@w[w_sel] / a) ^ (1 / b)

        catch_w <- getFMort(params)[iSpecies, w_sel] *
            params@initial_n[iSpecies, w_sel]
        # We just want the distribution, so we rescale the density so its area is 1
        if (sum(catch_w) > 0) catch_w <- catch_w / sum(catch_w * params@dw[w_sel])
        # The catch density in l gets an extra factor of dw/dl
        catch_l <- catch_w * b * w / l
        df <- data.frame(w, l, catch_w, catch_l, Type = "Model catch")

        # We also include the abundance density because that helps to understand
        # the catch density
        catch_w <- params@initial_n[iSpecies, w_sel]
        # We just want the distribution, so we rescale the density so its area is 1
        catch_w <- catch_w / sum(catch_w * params@dw[w_sel])
        # The catch density in l gets an extra factor of dw/dl
        catch_l <- catch_w * b * w / l
        abundance <- data.frame(w, l, catch_w, catch_l, Type = "Abundance")

        if (is_observed) {
            sel <- (catch$species == s)
            if ("length" %in% names(catch)) {
                l <- catch$length[sel]
                dl <- catch$dl[sel]
                catch_l <- catch$catch[sel]
                # normalise to a density in l
                catch_l <- catch_l / sum(catch_l * dl)
                # To get the density in w we need to divide by dw/dl
                w <- a * l ^ b
                catch_w <- catch_l / b * l / w
            } else {
                w <- catch$weight[sel]
                dw <- catch$dw[sel]
                catch_w <- catch$catch[sel]
                # normalise to a density in w
                catch_w <- catch_w / sum(catch_w * dw)
                # To get the density in l we need to divide by dl/dw
                l <- (w / a)^(1/b)
                catch_l <- catch_w * b / l * w
            }
            df <- rbind(df, data.frame(w, l, catch_w, catch_l,
                                       Type = "Observed catch"))
        }
        # From the abundance only keep values that are no larger than
        # the maximum of the other shown densities.
        if (x_var == "Weight") {
            abundance <- subset(abundance, catch_w < max(df$catch_w))
        } else {
            abundance <- subset(abundance, catch_l < max(df$catch_l))
        }
        # Add the abundance to the data frame last so that it shows up
        # last also in legend
        df <- rbind(df, abundance)
        df$Species <- SpIdx[which(params@species_params$species[iSpecies] == SpIdx)]
        plot_dat <- rbind(plot_dat, df)
    }

    # adding legends to params object
    params <- setColours(params, list("Model catch" = "#F8766D",
                                      "Observed catch" = "#00BFCFC4",
                                      "Abundance" = "grey"))
    params <- setLinetypes(params, list("Model catch" = "solid",
                                        "Observed catch" = "solid",
                                        "Abundance" = "solid"))

    if (x_var == "Weight") {
        sizeVline <- data.frame(
            w_mat = params@species_params[species, "w_mat"],
            y_coord = plot_dat %>%
                group_by(Species) %>%
                summarise(Value = max(catch_w)),
            Type = NA) # geom_text wants a group var for some reasons

        # remove length-related columns
        plot_dat <- plot_dat[, -c(2, 4)]
        colnames(plot_dat)[2] <- "Catch density"
        colnames(sizeVline)[2:3] <- c("Species", "y_coord")

        if (return_data) return(list(plot_dat, sizeVline))

        pl <- plotDataFrame(plot_dat, params, wrap_var = "Species",
                            xlab = "Size [g]",
                            ylab = "Normalised number density [1/g]",
                            wrap_scale = "free")

        pl <- pl +
            geom_vline(data = sizeVline,
                       aes(xintercept = w_mat, group = Species),
                       linetype = "dotted") +
            geom_text(data = sizeVline, aes(x = w_mat, y = y_coord * 0.9,
                                            label = "\nMaturity"))
    } else {
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
                            ylab = "Normalised number density [1/cm]",
                            wrap_scale = "free")

        pl <- pl +
            geom_vline(data = sizeVline,
                       aes(xintercept = w_mat, group = Species),
                       linetype = "dotted") +
            geom_text(data = sizeVline, aes(x = w_mat, y = y_coord * 0.9,
                                            label = "\nMaturity"))
    }
    return(pl)
}

#' @rdname plotYieldVsSize
#' @export
plotlyYieldVsSize <- function(object,
                              species = NULL,
                              catch = NULL,
                              x_var = c("Weight", "Length"),
                              ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotYieldVsSize", argg),
             tooltip = c("Catch density", "Type", "w", "l"))
}
