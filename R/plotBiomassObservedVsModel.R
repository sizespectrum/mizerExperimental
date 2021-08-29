#' Plotting observed vs. model biomass data
#'
#' If biomass observations are available for at least some species via the
#' `biomass_observed` column in the species parameter data frame, this function
#' plots the biomass of each species in the model against the observed
#' biomasses. When called with a MizerSim object, the plot will use the model
#' biomasses predicted for the final time step in the simulation.
#'
#' Before you can use this function you will need to have added a
#' `biomass_observed` column to your model which gives the observed biomass in
#' grams.  For species for which you have no observed biomass, you should set
#' the value in the `biomass_observed` column to 0 or NA.
#'
#' Biomass observations usually only include individuals above a certain size.
#' This size should be specified in a `biomass_cutoff` column of the species
#' parameter data frame. If this is missing, it is assumed that all sizes are
#' included in the observed biomass, i.e., it includes larval biomass.
#'
#' The total relative error is shown in the caption of the plot, calculated by
#'   \deqn{TRE = \sum_i|1-\rm{ratio_i}|}{TRE = sum_i |1-ratio_i|} where
#'   \eqn{\rm{ratio_i}}{ratio_i} is the ratio of model biomass / observed
#'   biomass for species i.
#'
#' @param object An object of class \linkS4class{MizerParams} or
#'   \linkS4class{MizerSim}.
#' @param species The species to be included. Optional. By default all observed
#'   biomasses will be included. A vector of species names, or a numeric vector
#'   with the species indices, or a logical vector indicating for each species
#'   whether it is to be included (TRUE) or not.
#' @param ratio Whether to plot model biomass vs. observed biomass (FALSE) or
#'   the ratio of model : observed biomass (TRUE). Default is FALSE.
#' @param log_scale Whether to plot on the log10 scale (TRUE) or not (FALSE).
#'   For the non-ratio plot this applies for both axes, for the ratio plot only
#'   the x-axis is on the log10 scale. Default is TRUE.
#' @param labels Whether to show text labels for each species (TRUE) or not
#'   (FALSE). Default is TRUE.
#' @param return_data Whether to return the data frame for the plot (TRUE) or
#'   not (FALSE). Default is FALSE
#' @return A ggplot2 object with the plot of model biomass by species compared
#'   to observed biomass. If `return_data = TRUE`, the data frame used to
#'   create the plot is returned instead of the plot.
#' @importFrom stats cor.test
#' @importFrom utils data
#' @export
#' @examples
#' # create an example
#' params <- NS_params
#' species_params(params)$biomass_observed <-
#'     c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
#' species_params(params)$biomass_cutoff <- 10
#' params <- calibrateBiomass(params)
#'
#' # Plot with default options
#' plotBiomassObservedVsModel(params)
#'
#' # Show the ratio instead
#' plotBiomassObservedVsModel(params, ratio = TRUE)
#'
#' # Run a simulation
#' params <- matchBiomasses(params)
#' sim <- project(params, t_max = 10)
#' plotBiomass(sim)
#'
#' # Plot the biomass comparison at the final time
#' plotBiomassObservedVsModel(sim)
#'
#' # The same with no log scaling of axes
#' plotBiomassObservedVsModel(sim, log_scale = FALSE)
plotBiomassObservedVsModel = function(object, species = NULL, ratio = FALSE,
                                      log_scale = TRUE,
                                      return_data = FALSE, labels = TRUE) {

    # preliminary checks
    if (is(object, "MizerSim")) {
        params = object@params # pull out params object
        n <- finalN(object) # we want final numbers
    } else if (is(object, "MizerParams")) {
        params = object # params object is just input
        n <- initialN(params) # we want initial numbers
    } else {
        stop("You have not provided a valid mizerSim or mizerParams object.")
    }
    sp_params <- params@species_params # get species_params data frame

    # Select appropriate species
    species = valid_species_arg(object, species)
    # find rows corresponding to species selected
    row_select = match(species, sp_params$species)
    if (!"biomass_observed" %in% names(sp_params)) {
        stop("You have not provided values for the column 'biomass_observed' ",
             "in the mizerParams/mizerSim object.")
    } else if (!is.numeric(sp_params$biomass_observed)) {
        stop("The column 'biomass_observed' in the mizerParams/mizerSim object",
             " is not numeric, please fix.")
    } else { # accept
        biomass_observed = sp_params$biomass_observed
    }

    # Check if cutoff exists
    cutoff <- sp_params$biomass_cutoff[row_select]
    # When no cutoff known, set it to 0 for all species
    # (so all sizes are included)
    if (is.null(cutoff)) {
        cutoff = rep(0, length(species))
    } else if (!is.numeric(cutoff)) {
        stop('params@species_params$biomass_cutoff is not numeric, ",
                 "please fix.')
    }

    # pull out biomasses from params object
    sim_biomass = rep(0, length(species))
    for (j in 1:length(species)) {
        sim_biomass[j] = sum((n[row_select[j], ] * params@w * params@dw)
                             [params@w >= cutoff[j]])
    }

    # Build dataframe
    dummy = data.frame(species, sim_biomass, biomass_observed[row_select]) %>%
        rename('species' = 1, 'model' = 2, 'observed' = 3) %>%
        filter(!is.na(observed), observed > 0)

    # preserve species order, add ratio of model/observed
    dummy = dummy %>% mutate(species = factor(species, levels = dummy$species),
                             ratio = model/observed)

    # Check that at least one observed biomass exists
    if (!nrow(dummy) > 0) {
        stop('Error: there are no observed biomasses to compare to model ",
             "biomasses, please fix.')
    }

    if (return_data == TRUE) return(dummy)

    # Calculate total sum of differences (abs(1-ratio)) rounded to 3 digits
    tre <- round(sum(abs(1 - dummy$ratio)), digits = 3)

    if (ratio == FALSE) {
        gg = ggplot(data = dummy, aes(x = observed, y = model,
                                      colour = species)) +
            geom_point(size = 3) +
            labs(y = 'model biomass') +
            coord_cartesian(ylim = range(dummy$model, dummy$observed)) +
            geom_abline(aes(intercept = 0, slope = 1), colour = 'purple',
                        linetype = "dashed", size = 1.3) # y = x line
    } else {
        gg = ggplot(data = dummy, aes(x = observed, y = ratio,
                                      colour = species)) +
            geom_point(size = 3) +
            labs(y = 'observed biomass / model biomass') +
            coord_cartesian(ylim = range(dummy$ratio)) +
            geom_hline(aes(yintercept = 1), linetype = "dashed",
                       colour = 'purple', size = 1.3)
    }

    gg = gg + labs(x = 'observed biomass',
                   caption = paste0("Total relative error = ", tre),
                   color = "Legend") +
        scale_colour_manual(values = getColours(params)[dummy$species])

    if (log_scale == TRUE & ratio == FALSE) {
        gg = gg + scale_x_log10() + scale_y_log10()
    }
    if (log_scale == TRUE & ratio == TRUE) {
        gg = gg + scale_x_log10()
    }

    if (labels == TRUE)  {
        gg = gg + ggrepel::geom_label_repel(
            aes(label = species),
            box.padding = 0.35,
            point.padding = 0.5,
            segment.color = 'grey50',
            show.legend = FALSE,
            max.overlaps = Inf)
    }
    gg
}
