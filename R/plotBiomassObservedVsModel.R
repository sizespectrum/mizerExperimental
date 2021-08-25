## Functions for plotting simulated vs. observed biomass data

#' @importFrom stats cor.test
#' @importFrom utils data
#' @export
plotBiomassObservedVsModel = function(object, fraction = F, log_scale = T, species = NULL) {
  
  # preliminary checks
  if (is(object, "MizerSim")) {
    sp_params <- object@params@species_params
  } else if (is(object, "MizerParams")) {
    sp_params <- object@species_params
  } else {
    stop("You have not provided a valid mizerSim or mizerParams object.")
  }
  species = valid_species_arg(object, species)
  row_select = match(species, sp_params$species) # find rows corresponding to species selected
  if (!"biomass_observed" %in% names(sp_params)) {
    stop("You have not provided values for the column 'biomass_observed' in the mizerParams/mizerSim object.")
  } else if (!is.numeric(sp_params$biomass_observed)) {
    stop("The column 'biomass_observed' in the mizerParams/mizerSim object is not numeric, please fix.")
  } else { # accept
    biomass_observed = sp_params$biomass_observed
  }
  
  # Build dataframe
  if (is(object, "MizerSim")) { # for sim object
    sim_biomass = getBiomass(object)[nrow(object@n), row_select]
  } else { # for params object
    sim_biomass <- rowSums(object@initial_n[row_select, ] * object@w * object@dw) # for Gustav to check
  }
  
  dummy = data.frame(species, sim_biomass, biomass_observed[row_select]) # fraction of sim/data
  names(dummy) = c('species', 'simulation', 'data')
  dummy$species = factor(dummy$species, levels = dummy$species[order(dummy$data, decreasing = T)]) # order by decreasing species biomass in data
  xlab = 'predicted biomass' 
  ylab = 'observed biomass'
  
  if (fraction == F & log_scale == T) { # For Pearson only, allow log10 biomasses
    dummy = dummy %>% mutate(simulation = log10(simulation), data = log10(data))
    xlab = 'log10(predicted biomass)' # update axis labels
    ylab = 'log10(observed biomass)'
  } 
  
  # pull out NA biomasses
  dummy = dummy %>% filter(!is.na(data))
  
  if (fraction == F) { # for Pearson
    res <- cor.test(dummy$simulation, dummy$data, method = "pearson") # Pearson's correlation coefficient
    pc = round(res$estimate, digits = 3) # rounded down to 3 digits
    title = paste("Pearsons coefficient =", pc)
    ylim = range(dummy %>% select(simulation, data)) # set y-limit
  } else { # for fraction
    res <- sum(abs(1 - dummy$simulation/dummy$data)) # sum of absolute differences
    pc = round(res, digits = 3) # rounded down to 3 digits
    dummy$simulation = dummy$simulation/dummy$data # replace simulation biomass with fraction
    title = paste("Total difference =", pc)
    ylim = range(dummy$simulation)
  }
  
  gg = ggplot(data = dummy, aes(x = data, y = simulation, colour = species, label = species)) +
    geom_point(size = 3) +
    ggrepel::geom_label_repel(box.padding   = 0.35,
                              point.padding = 0.5,
                              segment.color = 'grey50', 
                              show.legend = F,
                              max.overlaps = Inf) +
    coord_cartesian(ylim = ylim) +
    labs(x = xlab, y = ylab, title = title, color = "Legend")
  
  if (fraction == F) {
    gg = gg + geom_abline(aes(intercept = 0, slope = 1), colour = 'purple', linetype = "dashed", size = 1.3) # y = x line
  } else {
    gg = gg + geom_hline(aes(yintercept = 1), linetype = "dashed", colour = 'purple', size = 1.3)
  }
  
  gg # output
}


