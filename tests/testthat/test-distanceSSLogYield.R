test_that("distanceSSLogYield() agrees with getYield()", {
    params <- NS_params
    second_order_w(params) <- c(bin_average = TRUE)
    state <- function(p) list(n = p@initial_n, n_pp = p@initial_n_pp,
                              n_other = p@initial_n_other)
    previous <- params
    # Tilt the spectra so that the two states differ in shape, not just by a
    # factor: a factor cancels out of the distance whatever the quadrature.
    current <- params
    initialN(current) <- sweep(params@initial_n, 2, params@w^0.1, "*")

    d <- distanceSSLogYield(params, current = state(current),
                            previous = state(previous))

    y_current <- getYield(current)
    y_previous <- getYield(previous)
    sel <- y_current > 0 & y_previous > 0
    expect_equal(d, sum((log(y_current[sel]) - log(y_previous[sel]))^2))
})

test_that("distanceSSLogYield() is zero between identical states", {
    params <- NS_params
    s <- list(n = params@initial_n, n_pp = params@initial_n_pp,
              n_other = params@initial_n_other)
    expect_equal(distanceSSLogYield(params, current = s, previous = s), 0)
})
