test_that("constantEggRDI() keeps egg density constant", {
    # choose an example params object
    params <- NS_params
    # We set the reproduction rate functions
    params <- setRateFunction(params, "RDI", "constantEggRDI")
    params <- setRateFunction(params, "RDD", "noRDD")
    # Now the egg density stays fixed no matter how we fish
    sim <- project(params, t_max = 1, effort = 1)
    # Check that indeed the egg densities have not changed
    no_sp <- nrow(params@species_params) # number of species
    # Hacky shortcut to access the correct element of a 2D array using 1D notation
    idx <- (params@w_min_idx - 1) * no_sp + (1:no_sp)
    expect_equal(finalN(sim)[idx], initialN(params)[idx])
})
