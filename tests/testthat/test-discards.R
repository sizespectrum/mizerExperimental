local_edition(3)

## helper functions ----
test_that("getRetainProbGear works", {
    params <- NS_params
    # Choose example size
    w50_idx <- 50
    w50 <- params@w[w50_idx]
    l50 <- w2l(w50, params)
    l25_default <- 1.5 * l50
    w25_default <- l2w(l25_default, params)
    w25_idx <- 60
    w25 <- params@w[w25_idx]
    l25 <- w2l(w25, params)
    
    # If retain_l50 is missing, the return value should be all zero
    # with same dimensions and names as the selectivity matrix
    rpones <- params@selectivity
    rpones[] <- 1
    expect_identical(getRetainProbGear(params), rpones)
    gear_params(params)$retain_l50 <- NA
    expect_identical(getRetainProbGear(params), rpones)
    
    # Set retain_l50 for Sprat, Industrial trawl
    sp_idx <- which(params@species_params$species == "Sprat")
    gear_params(params)["Sprat, Industrial", "retain_l50"] <- l50[sp_idx]
    # Error if a and b not provided
    expect_error(getRetainProbGear(params),
                 "The weight-length parameters `a` and `b` need to be provided")
    params@species_params$a <- 0.01
    params@species_params$b <- 3
    retain_prob <- getRetainProbGear(params)
    # Expect 0.5 at l50
    expect_equal(retain_prob["Industrial", "Sprat", w50_idx], 0.5)
    # Expect 0.25 at l25
    # Check that it is above 0.25 in the previous size class and below in the next
    w25_default_idx <- sum(params@w <= w25_default[sp_idx])
    expect_gt(retain_prob["Industrial", "Sprat", w25_default_idx - 1], 0.25)
    expect_lt(retain_prob["Industrial", "Sprat", w25_default_idx + 1], 0.25)
    # Expect entries for other gear-species pairs to be one
    retain_prob["Industrial", "Sprat", ] <- 1
    expect_identical(retain_prob, rpones)
    
    # set also retain_l25 for Sprat, Industrial trawl
    gear_params(params)["Sprat, Industrial", "retain_l25"] <- l25[sp_idx]
    retain_prob <- getRetainProbGear(params)
    # Expect 0.5 at l50
    expect_equal(retain_prob["Industrial", "Sprat", w50_idx], 0.5)
    # Expect 0.25 at l25
    # Check that it is above 0.25 in the previous size class and below in the next
    w25_idx <- sum(params@w <= w25[sp_idx])
    expect_gt(retain_prob["Industrial", "Sprat", w25_idx - 1], 0.25)
    expect_lt(retain_prob["Industrial", "Sprat", w25_idx + 1], 0.25)
    
    # Expect errors for invalid parameters
    # l50 must be smaller than l25
    gear_params(params)["Sprat, Industrial", "retain_l25"] <- l50[sp_idx]
    expect_error(getRetainProbGear(params),
                 "The value for `retain_l25` must always be larger than")
    # l50 can not be negative
    gear_params(params)["Sprat, Industrial", "retain_l50"] <- -1
    expect_error(getRetainProbGear(params),
                 "The value for `retain_l50` must be non-negative")
})

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