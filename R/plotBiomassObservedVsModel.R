## Functions for plotting simulated vs. observed biomass data

#' @export
plotBiomassObservedVsModel = function(sim, fraction = F, log_scale = T, species = NULL) {
  
  # preliminary checks
  assert_that(is(sim, "MizerSim"))
  species = valid_species_arg(sim, species)
  row_select = match(species, sim@params@species_params$species) # find rows corresponding to species selected
  if (!"biomass_observed" %in% names(sim@params@species_params)) {
    stop(paste0("You have not provided values for the column 'biomass_observed' in ", deparse(substitute(sim))), '@params@species_params.')
  } else if (!is.numeric(sim@params@species_params$biomass_observed)) {
    stop(paste0("The column 'biomass_observed' in ", deparse(substitute(sim))), '@params@species_params is not numeric, please fix.')
  } else { # accept
    biomass_observed = sim@params@species_params$biomass_observed
  }
  
  # Build dataframe
  dummy = data.frame(species, getBiomass(sim)[nrow(sim@n), row_select], biomass_observed[row_select]) # fraction of sim/data
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
    geom_label_repel(box.padding   = 0.35,
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

