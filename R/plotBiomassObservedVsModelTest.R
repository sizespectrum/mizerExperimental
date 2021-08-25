## Testing out functions for Gustav

rm(list = ls())
# setwd("C:/...") # set directory if necessary

# libraries
library(mizer)
library(mizerExperimental)
library(tidyverse)
library(ggrepel)
library(assertthat)

# source('add_biomass_plots.R') # file with test code, commented out

ns_params <- newMultispeciesParams(NS_species_params_gears, inter) # the species parameters and interaction matrix
ns_sim = project(ns_params, t_max = 100)

# Test error without including biomass
plotBiomassObservedVsModel(ns_sim) # check that not having biomass_observed throws correct error

# Now check it works properly
end_biomass = getBiomass(ns_sim)[nrow(ns_sim@n), ] # biomass at steady state
vary_biomass = end_biomass*(0.75+0.5*runif(nrow(ns_params@interaction))) # shift biomasses a bit
ns_params@species_params$biomass_observed = vary_biomass # read into ns_params object
ns_sim2 = project(ns_params, t_max = 100)

# Should work fine now
plotBiomassObservedVsModel(ns_sim2)
plotBiomassObservedVsModel(ns_sim2, log_scale = F)
plotBiomassObservedVsModel(ns_sim2, fraction = T)

# Test it works for specific species
plotBiomassObservedVsModel(ns_sim2, species = c('Herring', 'Cod', 'Saithe', 'N.pout'))

# Test if some observed biomasses are NA
ns_sim3 = ns_sim2 # copy over sim object
ns_sim3@params@species_params$biomass_observed[c(2, 7, 10)] = NA # wipe out some biomasses
plotBiomassObservedVsModel(ns_sim3)

# Check that works for the params object now
plotBiomassObservedVsModel(ns_params)




