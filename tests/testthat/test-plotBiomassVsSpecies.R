test_that("plotBiomassVsSpecies() uses the model's quadrature", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    species_params(params)$biomass_observed <- getBiomass(params)

    df <- plotBiomassVsSpecies(params)$data
    model <- df$Biomass[df$Type == "Model"]
    expect_equal(model, unname(getBiomass(params, use_cutoff = TRUE)))
    # The hand-rolled first-order sum that this replaced is off by several
    # percent under bin averaging, so this is a real check.
    hand_rolled <- rowSums(sweep(params@initial_n, 2,
                                 params@w * params@dw, "*"))
    expect_false(isTRUE(all.equal(model, unname(hand_rolled))))
})

test_that("plotBiomassVsSpecies() honours biomass_cutoff", {
    params <- NS_params
    species_params(params)$biomass_observed <- getBiomass(params)
    species_params(params)$biomass_cutoff <- 10

    df <- plotBiomassVsSpecies(params)$data
    model <- df$Biomass[df$Type == "Model"]
    expect_equal(model, unname(getBiomass(params, use_cutoff = TRUE)))
    expect_true(all(model < getBiomass(params)))
})
