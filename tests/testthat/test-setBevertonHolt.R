test_that("setBevertonHolt sets erepro correctly", {
    params <- setBevertonHolt(NS_params,
                              erepro = 10 * NS_params@species_params$erepro)
    expect_identical(params@species_params$erepro,
                     10 * NS_params@species_params$erepro)
    expect_equal(getRequiredRDD(params), getRDD(params))

    # Get warning if requested erepro is too small
    expect_warning(params <-
                       setBevertonHolt(NS_params,
                                       erepro = NS_params@species_params$erepro / 10),
                   "For the following species the requested `erepro` was too small and has been increased to the smallest possible value: Gurnard, Plaice")
    expect_identical(params@species_params$R_max[params@species_params$species == "Gurnard"],
                     Inf)
    expect_equal(getRequiredRDD(params), getRDD(params))
})

test_that("setBevertonHolt sets R_max correctly", {
    params <- setBevertonHolt(NS_params,
                              R_max = 10 * NS_params@species_params$R_max)
    expect_identical(params@species_params$R_max,
                     10 * NS_params@species_params$R_max)
    expect_equal(getRequiredRDD(params), getRDD(params))
})

test_that("setBevertonHolt sets reproduction_level correctly", {
    expect_warning(params <- setBevertonHolt(NS_params, reproduction_level = 0.4),
                   "The following species require an unrealistic reproductive efficiency greater than 1: Plaice")
    expect_equal(getRDD(params), params@species_params$R_max * 0.4,
                 check.attributes = FALSE)
    expect_equal(getRequiredRDD(params), getRDD(params))
})

test_that("setBevertonHolt sets R_factor correctly", {
    expect_warning(params <- setBevertonHolt(NS_params, R_factor = 4),
                   "The following species require an unrealistic reproductive efficiency greater than 1: Plaice")
    expect_equal(getRDD(params), params@species_params$R_max / 4,
                 check.attributes = FALSE)
    expect_equal(getRequiredRDD(params), getRDD(params))
})
