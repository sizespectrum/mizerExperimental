#' Plotting observed vs. observed biomass data
#'
#' Given a MizerParams object or MizerSIm object `object` for which biomass
#' observations are available for at least some species via
#' `species_params(params)$biomass_observed`, this function plots the total
#' biomass by species against the provided biomass observation values.
#'
#' For mizerParams objects, the initial biomass of species will be compared to
#' the observed biomasses - unless the initial biomasses have been carefully
#' calibrated we would not expect these to be close. For mizerSim objects, the
#' final biomasses of species will be compared to the observed biomasses.
#'
#' Make sure that, for species which have no observed biomass,
#' `biomass_observed` for these species are set as 0 or NA.
#'
#' Observed biomasses usually only include individuals above a certain size.
#' This size should either be specified in
#' `species_params(params)$biomass_cutoff` in grams, or else all sizes are
#' included.
#'
#' @param object An object of class \linkS4class{MizerParams} or
#'   \linkS4class{MizerSim}.
#' @param species The species to be affected. Optional. By default all observed
#'   biomasses will be matched. A vector of species names, or a numeric vector
#'   with the species indices, or a logical vector indicating for each species
#'   whether it is to be affected (TRUE) or not.
#' @param ratio Whether to plot the Pearson correlation (FALSE) or the
#'   fraction of biomass (TRUE). Default is FALSE.
#' @param log_scale If using the Pearson coefficient plot, whether to plot on
#'   the log10 scale (TRUE) or not (FALSE). Default is TRUE.
#' @param labels Whether to show text labels for each species (TRUE) or not
#'   (FALSE). Default is TRUE.
#' @param return_data Whether to return the data frame for the plot (TRUE) or
#'   not (FALSE). Default is FALSE
#' @return A plot of the model biomass by species compared to observed
#'   biomass. The total absolute error is shown, calculated by
#'   TAE = \sum_i(abs(1-ratio_i))
#' @return The dataframe which creates the plot. Default is FALSE.
#' @importFrom stats cor.test
#' @importFrom utils data
#' @export
#' @examples
#' ns_params <- newMultispeciesParams(NS_species_params_gears, inter) # the species parameters and interaction matrix
#' ns_sim <- project(ns_params, t_max = 100, progress_bar = FALSE) # run forwards in time
#' end_biomass <- getBiomass(ns_sim)[nrow(ns_sim@n), ] # biomass at steady state
#' vary_biomass <- end_biomass*(0.75+0.5*runif(nrow(ns_params@interaction))) # shift biomasses a bit
#' # Check that works for the params object
#' species_params(ns_params)$biomass_observed <- vary_biomass # read into ns_params object
#' plotBiomassObservedVsModel(ns_params)
#' # Check that works for the sim object
#' species_params(ns_sim@params)$biomass_observed <- vary_biomass
#' plotBiomassObservedVsModel(ns_sim)
#' plotBiomassObservedVsModel(ns_sim, log_scale = F)
#' plotBiomassObservedVsModel(ns_sim, fraction = T)
#' test = plotBiomassObservedVsModel(ns_sim, fraction = T, return_data = T)
#' 
plotBiomassObservedVsModel = function(object, species = NULL, ratio = FALSE, log_scale = TRUE, 
                                      return_data = FALSE, labels = TRUE) {
  
  # browser() # for checking function
  
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
  row_select = match(species, sp_params$species) # find rows corresponding to species selected
  if (!"biomass_observed" %in% names(sp_params)) {
    stop("You have not provided values for the column 'biomass_observed' in the mizerParams/mizerSim object.")
  } else if (!is.numeric(sp_params$biomass_observed)) {
    stop("The column 'biomass_observed' in the mizerParams/mizerSim object is not numeric, please fix.")
  } else { # accept
    biomass_observed = sp_params$biomass_observed
  }
  
  # Check if cutoff exists
  cutoff <- sp_params$biomass_cutoff[row_select]
  # When no cutoff known, set it to 0 for all species (so all sizes are included)
  if (is.null(cutoff)) { 
    cutoff = rep(0, length(species))
  } else { # make sure it's numeric
    if (!is.numeric(cutoff)) stop('params@species_params$biomass_cutoff is not numeric, please fix.')
  }
  
  # pull out biomasses from simulation / params object
  sim_biomass = rep(0, length(species))
  for (j in 1:length(species)) sim_biomass[j] = sum((n[row_select[j], ] * params@w * params@dw)
                                                    [params@w >= cutoff[j]]) # biomass calculation
  
  # Build dataframe
  dummy = data.frame(species, sim_biomass, biomass_observed[row_select]) %>% # fraction of sim/data
    rename('species' = 1, 'model' = 2, 'observed' = 3) %>%
    filter(!is.na(observed), observed > 0) # remove NA and 0 observed biomasses
  
  dummy = dummy %>% mutate(species = factor(species, levels = dummy$species[order(dummy$observed, decreasing = T)]), # order by decreasing species biomass in data
                           ratio = model/observed) # add ratio of model/observed
  
  # Check that at least one observed biomass exists
  if (!nrow(dummy) > 0) stop('Error: there are no observed biomasses to compare to model biomasses, please fix.')
  
  # Calculate total sum of differences (abs(1-ratio))
  sad <- round(sum(abs(1 - dummy$ratio)), digits = 3) # sum of absolute differences, rounded down to 3 digits
  
  if (ratio == F) {
    gg = ggplot(data = dummy, aes(x = observed, y = model, colour = species, label = species)) +
      geom_point(size = 3) +
      labs(y = 'model biomass') + 
      coord_cartesian(ylim = range(dummy$model, dummy$observed)) +
      geom_abline(aes(intercept = 0, slope = 1), colour = 'purple', linetype = "dashed", size = 1.3) # y = x line
  } else {
    gg = ggplot(data = dummy, aes(x = observed, y = ratio, colour = species, label = species)) +
      geom_point(size = 3) +
      labs(y = 'observed biomass / model biomass') +
      coord_cartesian(ylim = range(dummy$ratio)) +
      geom_hline(aes(yintercept = 1), linetype = "dashed", colour = 'purple', size = 1.3)
  }
  
  gg = gg + labs(x = 'observed biomass',
                 title = paste0("Sum of absolute differences = ", sad), 
                 color = "Legend") +
    scale_colour_manual(values = getColours(params)[dummy$species])
  
  if (log_scale == T & ratio == F) gg = gg + scale_x_log10() + scale_y_log10()
  if (log_scale == T & ratio == T) gg = gg + scale_x_log10()
  
  if (labels == T)  {
    gg = gg + geom_label_repel(box.padding = 0.35,
                               point.padding = 0.5,
                               segment.color = 'grey50', 
                               show.legend = F,
                               max.overlaps = Inf)
  }   
  
  print(gg) # output
  
  if (return_data == T) return(dummy) 
}


