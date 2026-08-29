local_edition(3)
test_that("alignResource keeps resource encounter unchanged", {
    params <- NS_params
    # switch off interaction with non-resource
    params@interaction[] <- 0
    params2 <- alignResource(params)
    expect_equal(getEncounter(params)[, 1],
                 getEncounter(params2)[ ,1])
})

test_that("alignResource records the rescaled gamma as given", {
    params <- NS_params
    aligned <- alignResource(params)
    factor <- given_species_params(params)$gamma /
        given_species_params(aligned)$gamma
    # The rescaled `gamma` is recorded as user input, ...
    expect_equal(species_params(aligned)$gamma,
                 given_species_params(aligned)$gamma)
    expect_true(all(factor > 0))
    # ... so a later recalculation does not undo the rescaling.
    recalculated <- aligned
    species_params(recalculated) <- species_params(aligned)
    expect_equal(species_params(recalculated)$gamma,
                 species_params(aligned)$gamma)
    expect_equal(recalculated@search_vol, aligned@search_vol,
                 ignore_attr = TRUE)
})
