test_that("plotBiomassObservedVsModel works", {
  
  # Set up parameters
  params <- NS_params
  species_params(params)$biomass_observed <-
    c(0.8, 61, 12, 35, 1.6, 20, 10, 7.6, 135, 60, 30, 78)
  species_params(params)$biomass_cutoff <- 10
  params <- calibrateBiomass(params)
  dummy = plotBiomassObservedVsModel(params, return_data = T) # get data frame out
  
  # Check biomasses equal those put in
  expect_equal(dummy$observed, species_params(params)$biomass_observed) # check observed_biomasses are equal
  
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
  
  # Finally, look at plot
  
})
