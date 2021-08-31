## Testing out functions for Gustav

rm(list = ls())
setwd("C:/Users/dattas/Documents/mizerExperimental/R") # set directory if necessary

# libraries
library(mizer)
library(mizerExperimental)
library(tidyverse)
library(ggrepel)
library(plotly)
library(testthat)

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
plotBiomassObservedVsModel(ns_sim, ratio = T)
plotBiomassObservedVsModel(ns_sim, ratio = T, log_scale = F)
plotBiomassObservedVsModel(ns_sim, ratio = T, labels = F)
test = plotBiomassObservedVsModel(ns_sim, ratio = T, return_data = T)

# test plotly version
plotlyBiomassObservedVsModel(ns_sim)

# Test it works for specific species - either naming, numeric vector or a T/F vector
plotBiomassObservedVsModel(ns_sim, species = c('Herring', 'Cod', 'Saithe', 'N.pout'))
plotBiomassObservedVsModel(ns_sim, species = c(1, 3, 5, 7, 9, 11))
plotBiomassObservedVsModel(ns_sim, species = c(T, T, T, T, T, F, F, F, T, T, F, F))
plotBiomassObservedVsModel(ns_sim, species = rep(F, 12)) # error expected for no species
plotBiomassObservedVsModel(ns_sim, species = c('Herring', 'Cod', 'Monkey')) # error expected for incorrect species


# Test if some observed biomasses are NA or 0
ns_sim2 = ns_sim # copy over sim object
ns_sim2@params@species_params$biomass_observed[c(2, 7, 10)] = NA # wipe out some biomasses
ns_sim2@params@species_params$biomass_observed[c(1, 5)] = 0 # wipe out some biomasses
plotBiomassObservedVsModel(ns_sim2)
plotlyBiomassObservedVsModel(ns_sim2, ratio = T)

# Check that plot still works for no biomass_observed values
ns_sim2@params@species_params$biomass_observed = 0 # set all to zero
plotBiomassObservedVsModel(ns_sim2)

# Check a sim object works with a cutoff
ns_sim2 = ns_sim # copy over sim object
ns_sim2@params@species_params$biomass_cutoff = rep(10, nrow(ns_sim2@params@species_params))
plotBiomassObservedVsModel(ns_sim2)

# Do the same for a mizerParams object
ns_params2 = ns_params # copy over sim object
ns_params2@species_params$biomass_cutoff = rep(20, nrow(ns_params2@species_params))
plotBiomassObservedVsModel(ns_params2)


# Set up test

test_that("plotBiomassObservedVsModel works", {
  
  # Set up parameters
  params <- NS_params
  
  # check you get error without biomass_observed column
  expect_error(plotBiomassObservedVsModel(params))
  
  # pull out data frame for biomass comparison
  species_params(params)$biomass_observed <-
    c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
  species_params(params)$biomass_cutoff <- 10
  params <- calibrateBiomass(params)
  dummy = plotBiomassObservedVsModel(params, return_data = T) # get data frame out
  
  # Check biomasses equal those put in
  expect_equal(dummy$observed, species_params(params)$biomass_observed) # check observed_biomasses are equal
  
  # check that you get error with no species
  expect_error(plotBiomassObservedVsModel(params, species = rep(F, 12)))
  
  # Try removing observed biomasses, check it still checks out
  params2 = params # copy over
  species_params(params2)$biomass_observed[c(1, 7, 10)] = NA
  dummy = plotBiomassObservedVsModel(params2, return_data = T) # get data frame out
  expect_equal(dummy$is_observed, !is.na(species_params(params2)$biomass_observed)) # check that dummy has correct observations
  expect_equal(dummy$observed[dummy$is_observed], species_params(params2)$biomass_observed[!is.na(species_params(params2)$biomass_observed)]) # check observed_biomasses are equal
  
  # # Try removing species, check it still checks out
  sp_select = c(1, 4, 7, 10, 11, 12) # choose some species
  dummy = plotBiomassObservedVsModel(params, species = sp_select, return_data = T) # get reduced data frame out
  expect_equal(nrow(dummy), length(sp_select)) # check that dummy is right size
  expect_equal(dummy$observed, species_params(params)$biomass_observed[sp_select]) # check observed_biomasses are equal
  
  # Finally, look at plots
  dummy = plotBiomassObservedVsModel(params, return_data = T) # get data frame out
  p <- plotBiomassObservedVsModel(params) # get plot
  expect_true(is.ggplot(p))
  expect_identical(p$labels$x, "observed biomass")
  expect_identical(p$labels$y, "model biomass")
  expect_identical(p$data, dummy)
  
  # Ratio plot
  p <- plotBiomassObservedVsModel(params, ratio = T) # get plot
  expect_true(is.ggplot(p))
  expect_identical(p$labels$x, "observed biomass")
  expect_identical(p$labels$y, "model biomass / observed biomass")
  
  
})





