# Helper function to create a basic test params object
create_test_params <- function() {
    species_params <- data.frame(
        species = c("Species1", "Species2"),
        w_max = c(1000, 2000),
        a = c(0.006, 0.006),
        b = c(3, 3)
    )
    
    gear_params <- data.frame(
        species = "Species1",
        gear = "Trawl",
        catchability = 0.5,
        sel_func = "sigmoid_length",
        l25 = 10,
        l50 = 15
    )
    
    newMultispeciesParams(species_params, gear_params = gear_params)
}

test_that("prepare_params adds 'no gear' gear correctly", {
    # Create a minimal MizerParams object for testing
    p <- create_test_params()
    
    # Run the function
    p_prepared <- prepare_params(p)
    
    # Tests
    gp <- gear_params(p_prepared)
    
    # Check that both species now have gear params
    expect_equal(nrow(gp), 2)
    
    # Check that Species2 was assigned "no gear"
    species2_gear <- gp[gp$species == "Species2", ]
    expect_equal(species2_gear$gear, "no gear")
    expect_equal(species2_gear$catchability, 0)
})

test_that("prepare_params sets default parameters", {
    # Create a minimal MizerParams object for testing
    species_params <- data.frame(
        species = c("Species1", "Species2"),
        w_max = c(1000, 2000)
    )
    
    p <- newMultispeciesParams(species_params)
    
    # Run the function
    p_prepared <- prepare_params(p)
    
    # Check that default parameters were set
    sp <- species_params(p_prepared)
    expect_equal(sp$a, c(0.006, 0.006))
    expect_equal(sp$b, c(3, 3))
    expect_equal(sp$t0, c(0, 0))
    
    # Check that w_mat25 was calculated correctly
    expected_w_mat25 <- sp$w_mat / (3^(1 / 10))
    expect_equal(sp$w_mat25, expected_w_mat25)
})

test_that("prepare_params handles case with all species having gears", {
    # Create a minimal MizerParams object where all species have gears
    species_params <- data.frame(
        species = c("Species1", "Species2"),
        w_max = c(1000, 2000),
        a = c(0.006, 0.006),
        b = c(3, 3)
    )
    
    gear_params <- data.frame(
        species = c("Species1", "Species2"),
        gear = c("Trawl1", "Trawl2"),
        catchability = c(0.5, 0.5),
        sel_func = c("sigmoid_length", "sigmoid_length"),
        l25 = c(10, 10),
        l50 = c(15, 15)
    )
    
    p <- newMultispeciesParams(species_params, gear_params = gear_params)
    
    # Run the function
    p_prepared <- prepare_params(p)
    
    # Test that no "no gear" was added
    gp <- gear_params(p_prepared)
    expect_equal(nrow(gp), 2)
    expect_false(any(gp$gear == "no gear"))
})

test_that("finalise_params removes temporary attributes and gears", {
    p <- create_test_params()
    p <- prepare_params(p)  # This adds the "no gear" gear
    
    # Add some test attributes that should be removed
    p@species_params$tuneParams_old_repro_level <- 0.5
    p@species_params$tuneParams_old_R_max <- 10000000
    p@species_params$tuneParams_old_erepro <- 0.1
    attr(p, "changes") <- 5
    
    # Run finalisation
    p_final <- finalise_params(p)
    
    # Test cleanup
    expect_null(attr(p_final, "changes"))
    gp <- gear_params(p_final)
    expect_false(any(gp$gear == "no gear"))
    
    # Test removal of temporary reproduction parameters
    sp <- species_params(p_final)
    expect_false("tuneParams_old_repro_level" %in% names(sp))
    expect_false("tuneParams_old_R_max" %in% names(sp))
    expect_false("tuneParams_old_erepro" %in% names(sp))
})

test_that("tab_name and tab_title work correctly", {
    expect_equal(tab_name("TestTab"), "testTab")
    expect_equal(tab_name("testTab"), "testTab")
    
    # Test tab_title with default behavior
    expect_equal(tab_title("TestTab"), "TestTab")
})
