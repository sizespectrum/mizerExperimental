local_edition(3)
test_that("alignResource keeps resource encounter unchanged", {
    params <- NS_params
    # switch off interaction with non-resource
    params@interaction[] <- 0
    params2 <- alignResource(params)
    expect_equal(getEncounter(params)[, 1],
                 getEncounter(params2)[ ,1])
})
