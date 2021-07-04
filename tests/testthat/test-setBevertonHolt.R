test_that("setBevertonHolt works", {
    params <- setBevertonHolt(NS_params,
                              erepro = 10 * NS_params@species_params$erepro)
    expect_identical(params@species_params$erepro,
                     10 * NS_params@species_params$erepro)
    expect_equal(getRequiredRDD(params), getRDD(params))

    params <- setBevertonHolt(NS_params,
                              R_max = 10 * NS_params@species_params$R_max)
    expect_identical(params@species_params$R_max,
                     10 * NS_params@species_params$R_max)
    expect_equal(getRequiredRDD(params), getRDD(params))

    expect_warning(params <- setBevertonHolt(NS_params, reproduction_level = 0.4),
                   "The following species require an unrealistic reproductive efficiency greater than 1: Plaice")
    expect_equal(getRDD(params), params@species_params$R_max * 0.4,
                 check.attributes = FALSE)
    expect_equal(getRequiredRDD(params), getRDD(params))

    expect_warning(params <- setBevertonHolt(NS_params, R_factor = 4),
                   "The following species require an unrealistic reproductive efficiency greater than 1: Plaice")
    expect_equal(getRDD(params), params@species_params$R_max / 4,
                 check.attributes = FALSE)
    expect_equal(getRequiredRDD(params), getRDD(params))

})
