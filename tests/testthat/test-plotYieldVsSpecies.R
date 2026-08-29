test_that("plotYieldVsSpecies() agrees with getYield()", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    gear_params(params)$yield_observed <- 1e10

    df <- plotYieldVsSpecies(params)$data
    model <- df$Yield[df$Type == "Model"]
    yield <- getYield(params)
    expect_equal(model, unname(yield[yield > 0]))
    # The hand-rolled first-order sum that this replaced differs under bin
    # averaging, so this is a real check.
    hand_rolled <- rowSums(sweep(params@initial_n, 2,
                                 params@w * params@dw, "*") * getFMort(params))
    expect_false(isTRUE(all.equal(model, unname(hand_rolled[yield > 0]))))
})

test_that("plotYieldVsSpecies() agrees with getYieldGear() for one gear", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    gear_params(params)$yield_observed <- 1e10
    gear <- as.character(gear_params(params)$gear[[1]])

    df <- plotYieldVsSpecies(params, gear = gear)$data
    model <- df$Yield[df$Type == "Model"]
    yield <- getYieldGear(params)[gear, ]
    expect_equal(model, unname(yield[yield > 0]))
})
