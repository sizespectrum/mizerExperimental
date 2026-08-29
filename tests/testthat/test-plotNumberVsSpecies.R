test_that("plotNumberVsSpecies() uses the model's quadrature", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    species_params(params)$number_observed <- 1e10

    df <- plotNumberVsSpecies(params)$data
    model <- df$Number[df$Type == "Model"]
    expect_equal(model, unname(sizeIntegral(params)))
})

test_that("plotNumberVsSpecies() honours number_cutoff", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    species_params(params)$number_observed <- 1e10
    cutoff <- 10
    species_params(params)$number_cutoff <- cutoff

    df <- plotNumberVsSpecies(params)$data
    model <- df$Number[df$Type == "Model"]
    expect_equal(model, unname(sizeIntegral(params, min_w = cutoff)))
    # `sizeIntegral()` lets the bin straddling the cutoff contribute only the
    # part of it that lies above the cutoff, where the hand-rolled sum that
    # this replaced counted the whole bin.
    hand_rolled <- rowSums(sweep(params@initial_n, 2, params@dw, "*")
                           [, params@w >= cutoff, drop = FALSE])
    expect_false(isTRUE(all.equal(model, unname(hand_rolled))))
})
