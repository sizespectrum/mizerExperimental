test_that("biomass_callback works during simulation", {
    params <- NS_params
    pdf(NULL)
    on.exit(dev.off())
    
    # Test biomass_callback execution
    sim_bm <- project(params, t_max = 2, callback = biomass_callback, progress_bar = FALSE)
    expect_s3_class(sim_bm, "MizerSim")
    
    # Test biomass_callback with species filtering
    sim_bm2 <- project(params, t_max = 2, callback = biomass_callback, species = c("Cod", "Herring"), progress_bar = FALSE)
    expect_s3_class(sim_bm2, "MizerSim")
    
    # Test biomass_callback warning on invalid species
    expect_warning(
        project(params, t_max = 2, callback = biomass_callback, species = c("Cod", "InvalidSpecies"), progress_bar = FALSE),
        "The following species specified in biomass_callback were not found"
    )
})


test_that("biomass_callback runs with second-order size quadrature", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    pdf(NULL)
    on.exit(dev.off())
    sim <- project(params, t_max = 0.02, dt = 0.01, t_save = 0.01,
                   callback = biomass_callback, progress_bar = FALSE)
    expect_s3_class(sim, "MizerSim")
    expect_true(all(is.finite(getBiomass(sim))))
})
