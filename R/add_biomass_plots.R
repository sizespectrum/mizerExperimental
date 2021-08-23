## Functions for plotting simulated vs. observed biomass data

plot_pearson = function(sim, log_scale = T) {
  
  if (log_scale == T) {
    dummy = data.frame(sim@params@species_params$species, log10(getBiomass(sim)[nrow(sim@n), ]), log10(sim@params@species_params$biomass_observed))
    xlab = 'log10(predicted biomass)' 
    ylab = 'log10(observed biomass)'
  } else {
    dummy = data.frame(sim@params@species_params$species, getBiomass(sim)[nrow(sim@n), ], sim@params@species_params$biomass_observed)
    xlab = 'predicted biomass' 
    ylab = 'observed biomass'
  }
  names(dummy) = c('species', 'simulation', 'data')
  res <- cor.test(dummy$simulation, dummy$data, method = "pearson")
  pc = round(res$estimate, digits = 4)
  dummy$species = factor(dummy$species, levels = dummy$species[order(dummy$data, decreasing = T)]) # order by decreasing species biomass in data
  
  gg = ggplot(data = dummy, aes(x = data, y = simulation, colour = species, label = species))
  gg = gg + geom_point(size = 3) +
    geom_label_repel(box.padding   = 0.35,
                     point.padding = 0.5,
                     segment.color = 'grey50', 
                     show.legend = F,
                     max.overlaps = Inf) +
    geom_abline(aes(intercept = 0, slope = 1), colour = 'purple', linetype = "dashed", size = 1.3) +
    coord_cartesian(ylim = (range(dummy %>% select(simulation, data)))) +
    labs(x = xlab, y = ylab, title = paste("Pearsons coefficient = ", pc), 
         color = "Legend")
  gg # output
}

plot_fraction = function(sim) {
  dummy = data.frame(sim@params@species_params$species, getBiomass(sim)[nrow(sim@n), ], sim@params@species_params$biomass_observed) # fraction of sim/data
  names(dummy) = c('species', 'simulation', 'data')
  res <- sum(abs(1 - dummy$simulation/dummy$data))
  pc = round(res, digits = 3)
  dummy$fraction = dummy$simulation/dummy$data
  dummy$species = factor(dummy$species, levels = dummy$species[order(dummy$data, decreasing = T)]) # order by decreasing species biomass in data
  
  gg = ggplot(data = dummy, aes(x = data, y = fraction, colour = species, label = species))
  gg = gg + geom_point(size = 3) +
    geom_label_repel(box.padding   = 0.35,
                     point.padding = 0.5,
                     segment.color = 'grey50', 
                     show.legend = F,
                     max.overlaps = Inf) +
    # geom_text(show.legend = F) +
    # geom_text(hjust = 0, vjust = 0) +
    geom_hline(aes(yintercept = 1), linetype = "dashed", colour = 'purple', size = 1.3) +
    labs(x = 'Biomass from data', y = 'Fraction of biomass in simulation', title = paste("Total difference = ", pc), color = "Legend")
  
  gg # output
  
}