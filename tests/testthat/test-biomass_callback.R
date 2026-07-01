test_that("biomass_callback works during simulation", {
    params <- NS_params
    pdf(NULL)
    on.exit(dev.off())
    
    # Test biomass_callback execution
    sim_bm <- project(params, t_max = 2, callback = biomass_callback, progress_bar = FALSE)
    expect_s4_class(sim_bm, "MizerSim")
    
    # Test biomass_callback with species filtering
    sim_bm2 <- project(params, t_max = 2, callback = biomass_callback, species = c("Cod", "Herring"), progress_bar = FALSE)
    expect_s4_class(sim_bm2, "MizerSim")
    
    # Test biomass_callback warning on invalid species
    expect_warning(
        project(params, t_max = 2, callback = biomass_callback, species = c("Cod", "InvalidSpecies"), progress_bar = FALSE),
        "The following species specified in biomass_callback were not found"
    )
})
