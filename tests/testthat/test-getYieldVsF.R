# A params object where two gears catch the same species
params <- NS_params
gp <- gear_params(params)
gp_extra <- gp[gp$species == "Cod", ]
gp_extra$gear <- "Extra"
gp_extra$catchability <- 0.5
gear_params(params) <- rbind(gp, gp_extra)
initial_effort(params)["Extra"] <- 1

test_that("getYieldVsF() checks the `gear` argument", {
    expect_error(getYieldVsF(params, "Cod", gear = "Nonexistent"),
                 "The gear Nonexistent does not catch Cod.")
    expect_error(getYieldVsF(params, "Cod", gear = c("Otter", "Extra")),
                 "You can only select a single gear.")
    # Gears that do not catch the species are rejected
    expect_error(getYieldVsF(params, "Cod", gear = "Industrial"),
                 "The gear Industrial does not catch Cod.")
    expect_message(getYieldVsF(params, "Cod", F_range = numeric(0)),
                   "Several gears catch Cod")
})

test_that("getYieldVsF() only changes F from the selected gear", {
    # Only vary the fishing mortality from the "Extra" gear, keeping the
    # current fishing mortality from that gear, so that nothing should change.
    curve <- getYieldVsF(params, "Cod", gear = "Extra", F_range = 0.5)
    expect_equal(curve$F, 0.5)
    # The yield is still positive even at zero effort of the selected gear
    # because the "Otter" gear keeps catching Cod.
    curve0 <- getYieldVsF(params, "Cod", gear = "Extra", F_range = 0)
    expect_gt(curve0$yield, 0)
    # Whereas without a gear selection all fishing on Cod is switched off
    curve_all <- getYieldVsF(params, "Cod", F_range = 0)
    expect_equal(curve_all$yield, 0)
})

test_that("getYieldVsF() reports the kind of attractor it found", {
    curve <- getYieldVsF(params, "Cod", gear = "Extra", F_range = c(0.4, 0.6),
                         progress_bar = FALSE)
    expect_named(curve, c("F", "yield", "yield_min", "yield_max",
                          "type", "period", "residual"))
    expect_equal(curve$F, c(0.4, 0.6))
    # This model settles on a fixed point, so the yield does not vary and no
    # cycle period is reported.
    expect_true(all(curve$type == "below_tolerance"))
    expect_equal(curve$yield, curve$yield_min)
    expect_equal(curve$yield, curve$yield_max)
    expect_true(all(is.na(curve$period)))
    # The residual says how far the states still are from a fixed point
    expect_true(all(curve$residual < 0.01))
})

test_that("getYieldVsF() averages the yield over one period of a limit cycle", {
    # At high effort the North Sea model settles onto a limit cycle
    p_cycle <- suppressMessages(
        projectToSteady(NS_params, effort = 2, t_max = 200, tol = 1e-8,
                        info_level = 0))
    expect_identical(attr(p_cycle, "convergence")$type, "cycle")

    curve <- suppressMessages(
        getYieldVsF(p_cycle, "Herring", F_range = c(0.5, 1),
                    progress_bar = FALSE))
    expect_identical(curve$type, c("cycle", "cycle"))
    expect_true(all(curve$period > 0))
    # The yield oscillates, so the average lies strictly inside the range
    expect_true(all(curve$yield_min < curve$yield))
    expect_true(all(curve$yield < curve$yield_max))

    # The average over one period agrees with an average over many periods
    settled <- suppressMessages(
        projectToSteady(NS_params, effort = 2, t_max = 200, tol = 1e-8,
                        info_level = 0))
    period <- attr(settled, "convergence")$period
    long <- project(settled, t_max = round(20 * period / 0.1) * 0.1,
                    dt = 0.1, t_save = 0.1, progress_bar = FALSE)
    y <- as.vector(getYield(long)[, "Herring"])
    reference <- mean(y[-length(y)])
    one <- suppressMessages(
        getYieldVsF(settled, "Herring",
                    F_range = sum(initial_effort(settled)[["Otter"]] *
                                      gear_params(settled)$catchability[
                                          gear_params(settled)$species == "Herring"]),
                    progress_bar = FALSE))
    expect_equal(one$yield, reference, tolerance = 0.05)
})

test_that("getYieldVsF() messages about F values that did not settle", {
    expect_message(
        getYieldVsF(params, "Cod", gear = "Extra", F_range = 0.5, t_max = 3,
                    progress_bar = FALSE),
        "did not settle onto an attractor within 3 years")
})
