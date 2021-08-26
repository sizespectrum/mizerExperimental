## Testing out functions for Gustav

rm(list = ls())
setwd("C:/Users/dattas/Documents/mizerExperimental/R") # set directory if necessary

# libraries
library(mizer)
library(mizerExperimental)
library(tidyverse)
library(ggrepel)
library(assertthat)
library(plotly)

source('plotBiomassObservedVsModel.R') # file with plotting function

ns_params <- newMultispeciesParams(NS_species_params_gears, inter) # the species parameters and interaction matrix
ns_sim = project(ns_params, t_max = 100)

# Test error without including biomass
plotBiomassObservedVsModel(ns_sim) # check that not having biomass_observed throws correct error

# Now generate biomasses - vary from exact values
end_biomass = getBiomass(ns_sim)[nrow(ns_sim@n), ] # biomass at steady state
vary_biomass = end_biomass*(0.75+0.5*runif(nrow(ns_params@interaction))) # shift biomasses a bit

# Check that works for the params object
ns_params@species_params$biomass_observed = vary_biomass # read into ns_params object
plotBiomassObservedVsModel(ns_params)

# Check that works for the sim object
ns_sim@params@species_params$biomass_observed = vary_biomass
plotBiomassObservedVsModel(ns_sim)
plotBiomassObservedVsModel(ns_sim, log_scale = F)
plotBiomassObservedVsModel(ns_sim, fraction = T)
test = plotBiomassObservedVsModel(ns_sim, fraction = T, return_data = T)

# test labels and plotly version
plotBiomassObservedVsModel(ns_sim, labels = F)
ggplotly(plotBiomassObservedVsModel(ns_sim, labels = F))

# Test it works for specific species - either naming, numeric vector or a T/F vector
plotBiomassObservedVsModel(ns_sim, species = c('Herring', 'Cod', 'Saithe', 'N.pout'))
plotBiomassObservedVsModel(ns_sim, species = c(1, 3, 5, 7, 9, 11))
plotBiomassObservedVsModel(ns_sim, species = c(T, T, T, T, T, F, F, F, T, T, F, F))


# Test if some observed biomasses are NA or 0
ns_sim2 = ns_sim # copy over sim object
ns_sim2@params@species_params$biomass_observed[c(2, 7, 10)] = NA # wipe out some biomasses
ns_sim2@params@species_params$biomass_observed[c(1, 5)] = 0 # wipe out some biomasses
plotBiomassObservedVsModel(ns_sim2)

# Check that correct error flags for no biomass_observed values
ns_sim2@params@species_params$biomass_observed = 0 # set all to zero
plotBiomassObservedVsModel(ns_sim2)

# Check a sim object works with a cutoff
ns_sim2 = ns_sim # copy over sim object
ns_sim2@params@species_params$biomass_cutoff = rep(10, nrow(ns_sim3@params@species_params))
plotBiomassObservedVsModel(ns_sim2)

# Do the same for a mizerParams object
ns_params2 = ns_params # copy over sim object
ns_params2@species_params$biomass_cutoff = rep(20, nrow(ns_params2@species_params))
plotBiomassObservedVsModel(ns_params2)


