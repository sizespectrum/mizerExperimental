# Initialisation ----------------
species_params <- NS_species_params_gears
# Make species names numeric because that created problems in the past
species_params$species <- 1:(nrow(species_params))
species_params$pred_kernel_type <- "truncated_lognormal"
params <- newMultispeciesParams(species_params, inter, no_w = 30,
                                n = 2/3, p = 0.7, lambda = 2.8 - 2/3)
sim <- project(params, effort = 1, t_max = 3, dt = 1, t_save = 1)
sim0 <- project(params, effort = 0, t_max = 3, dt = 1, t_save = 1)
species <- c(11, 10)
# Mark some species as background
params_bkgrd <- params
params_bkgrd@A[1:3] <- NA
# params object with single species
sp_single <- data.frame(species = 1, w_max = 1000, h = 30)
params_single <- newMultispeciesParams(sp_single, no_w = 30)
# Make some data frame for plotDataFrame
sampleDf <- plotBiomass(sim, return_data = TRUE)

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

# plots have not changed ----
test_that("plots have not changed", {
    p <- plotDataFrame(sampleDf, params)
    expect_doppelganger("Plot Data Frame", p)

    # the next line only needed until NS_params is upgraded
    params <- setColours(NS_params, c(Fishing = "red"))
    p <- plotDeath(params, species = "Haddock")
    expect_doppelganger("Plot Death", p)

    p <- plotResourcePred(NS_params)
    expect_doppelganger("Plot Resource Pred", p)

    p <- plotResourceLevel(NS_params)
    expect_doppelganger("Plot Resource", p)

    p <- plotEnergyBudget(NS_params, species = "Haddock")
    expect_doppelganger("Plot Energy Budget", p)

    p <- plotYieldVsSize(NS_params, species = "Haddock")
    expect_doppelganger("Plot Yield vs Size", p)
})

# plotly functions do not throw error
test_that("plotly functions do not throw error", {
    expect_error(plotlyDeath(params, species = species), NA)
    expect_error(plotlyResourcePred(params), NA)
    expect_error(plotlyEnergyBudget(params, species = species), NA)
    expect_error(plotlyYieldVsSize(params, species = species), NA)
})


# testing the plot outputs
test_that("return_data is identical",{
    expect_equal(dim(plotDeath(sim, species = species, return_data = TRUE)), c(784,4))

    expect_equal(dim(plotResourcePred(sim, return_data = TRUE)), c(612,3))

    expect_equal(dim(plotResourceLevel(sim, return_data = TRUE)), c(51,3))

    expect_equal(dim(plotEnergyBudget(sim, species = species, return_data = TRUE)[[1]]), c(224,4))

    expect_equal(dim(plotYieldVsSize(sim, species = species, return_data = TRUE)[[1]]), c(43,4))
}
)
