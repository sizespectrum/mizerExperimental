test_that("matchYield works", {
    params <- NS_params
    initial_effort(params) <- 1
    yields <- getYieldGear(params)
    gp <- params@gear_params
    # Add yield_observed column but make it twice too small
    for (i in seq_len(nrow(gp))) {
        species <- as.character(gp$species[i])
        gear <- as.character(gp$gear[i])
        gp$yield_observed[i] <- yields[gear, species] / 2
    }
    gear_params(params) <- gp
    
    # Change only some catchabilities
    p <- matchYield(params, species = c("Dab", "Sole"), gears = "Beam",
                    keep = "biomass")
    expect_equal(p@gear_params["Dab, Beam", "catchability"],
                 params@gear_params["Dab, Beam", "catchability"] / 2)
    expect_equal(p@gear_params["Sole, Beam", "catchability"],
                 params@gear_params["Sole, Beam", "catchability"] / 2)
    expect_equal(p@gear_params["Plaice, Beam", "catchability"],
                 params@gear_params["Plaice, Beam", "catchability"])
    expect_equal(getBiomass(p), getBiomass(params))
    
    # Change all catchabilities
    p <- with_info_level(matchYield(params, keep = "biomass"), info_level = 0)
    expect_equal(p@gear_params$catchability,
                 params@gear_params$catchability / 2)
    expect_equal(getBiomass(p), getBiomass(params))
})


test_that("yield calibration retains the active Ricker law", {
    p <- setReproduction(validParams(NS_params), RDD = "RickerRDD")
    initial_effort(p) <- 1
    yields <- getYieldGear(p)
    gp <- p$gear_params
    gp$yield_observed <- vapply(seq_len(nrow(gp)), function(i) {
        yields[gp$gear[i], gp$species[i]] / 2
    }, numeric(1))
    gear_params(p) <- gp
    q <- matchYield(p, species = c("Dab", "Sole"), gears = "Beam",
                    keep = "biomass")
    expect_identical(q$rates_funcs$RDD, "RickerRDD")
    expect_equal(getRates(q)$rdd[c("Dab", "Sole")],
                 getRequiredRDD(q)[c("Dab", "Sole")])
    expect_equal(getBiomass(q), getBiomass(p))
})
