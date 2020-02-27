test_that("animateSpectra does not throw error", {
    sim <- mizer::project(mizer::NS_params, t_max = 2, t_save = 1, effort = 1)
    expect_error(animateSpectra(sim), NA)
})
