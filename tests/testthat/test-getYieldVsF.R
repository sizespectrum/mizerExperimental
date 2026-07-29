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
