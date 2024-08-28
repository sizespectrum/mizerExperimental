test_that("matchYield works", {
    params <- NS_params
    initial_effort(params) <- 1
    yields <- getYieldGear(params)
    gp <- params@gear_params
    # Add yield_observed column but make it twice too large
    for (i in seq_len(nrow(gp))) {
        species <- as.character(gp$species[i])
        gear <- as.character(gp$gear[i])
        gp$yield_observed[i] <- yields[gear, species] * 2
    }
    gear_params(params) <- gp
    p <- matchYield(params)
    expect_equal(getYieldGear(params) * 2, getYieldGear(p))
})
