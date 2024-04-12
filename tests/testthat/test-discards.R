local_edition(3)

test_that("valid_gears_arg works", {
    expect_equal(valid_gears_arg(NS_params, "Industrial"), "Industrial")
    expect_equal(valid_gears_arg(NS_params, c("Industrial", "Otter")),
                 c("Industrial", "Otter"))
    expect_equal(valid_gears_arg(NS_params, gears = NULL),
                 unique(as.character(NS_params@gear_params$gear)))
    expect_equal(valid_gears_arg(NS_params, "nonexistent"),
                 character(0)) |>
        expect_warning("The following gears do not exist: nonexistent.")
    expect_error(valid_gears_arg(NS_params, "nonexistent", error_on_empty = TRUE),
                 "No gears have been selected.") |>
        expect_warning("The following gears do not exist: nonexistent.")
    
    # Also works with MizerSim object
    expect_equal(valid_gears_arg(NS_sim, "Sprat"), "Sprat")
})

test_that("plotCatchVsSize throws the correct errors", {
    expect_error(plotCatchVsSize(NS_params),
                 "You must select a single species")
    expect_error(plotCatchVsSize(NS_params, species = c("Sprat", "Sandeel")),
                 "You must select a single species")
    expect_warning(plotCatchVsSize(NS_params, species = "Sprat", gears = "nonexistent"),
                  "The following gears do not exist: nonexistent.") |>
        expect_error("No gears have been selected.")
})

test_that("plotCatchVsSize works", {
    params <- NS_params
    data <- plotCatchVsSize(params, species = "Cod", return_data = TRUE)
    expect_equal(unique(data[[1]]$Type), c("Model catch", "Model abundance"))
    gear_params(params)$retain_l50 <- NA
    gear_params(params)["Cod, Otter", "retain_l50"] <- 1000
    
    # If there is any discard then discards and landings should be included
    data <- plotCatchVsSize(params, species = "Cod", return_data = TRUE)
    expect_equal(unique(data[[1]]$Type), 
                 c("Model catch", "Model landings", "Model discards",
                   "Model abundance"))
    # But if for the selected gears and species there is no discard, then
    # they should not be plotted
    data <- plotCatchVsSize(params, species = "Haddock", return_data = TRUE)
    expect_equal(unique(data[[1]]$Type), 
                 c("Model catch", "Model abundance"))
    
    # TODO: add more tests when we have a good example catch data frame
})