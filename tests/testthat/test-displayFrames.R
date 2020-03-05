context("displayFrames")
# Initialisation ----------------
params <- mizer::newMultispeciesParams(
    mizer::NS_species_params_gears, mizer::inter, no_w = 30,
    n = 2/3, p = 0.7, lambda = 2.8 - 2/3)
sim <- mizer::project(params, effort = 1, t_max = 3, dt = 1, t_save = 1)
sim0 <- mizer::project(params, effort = 0, t_max = 3, dt = 1, t_save = 1)
species <- c("Cod", "Haddock")

# plots have not changed ----
test_that("plots have not changed", {
    p <- displayFrames(getBiomassFrame(sim0,
                                       species = species,
                                       start_time = 1,
                                       total = TRUE),
                       getBiomassFrame(sim,
                                       end_time = 3,
                                       ylim = c(1e12)),
                       params)
    vdiffr::expect_doppelganger("Display Frames", p)

    expect_known_value(getSSBFrame(sim, species = "Cod", total = TRUE),
                       "values/getSSBFrame")
})
