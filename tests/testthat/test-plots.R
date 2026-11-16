# Initialisation ----------------
species_params <- NS_species_params_gears
# Make species names numeric because that created problems in the past
species_params$species <- 1:(nrow(species_params))
species_params$pred_kernel_type <- "truncated_lognormal"
params <- newMultispeciesParams(species_params, inter, no_w = 30,
                                n = 2/3, p = 0.7, lambda = 2.8 - 2/3)
sim <- project(params, effort = 1, t_max = 3, dt = 1, t_save = 1)
sim0 <- project(params, effort = 0, t_max = 3, dt = 1, t_save = 1)
species <- c(11, 10)
# Mark some species as background
params_bkgrd <- markBackground(params, species = params@species_params$species[1:3])
# params object with single species
sp_single <- data.frame(species = 1, w_inf = 1000, h = 30)
params_single <- newMultispeciesParams(sp_single, no_w = 30)
# Make some data frame for plotDataFrame
sampleDf <- plotBiomass(sim, return_data = TRUE)

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

# plots have not changed ----
test_that("plots have not changed", {
    p <- plotDataFrame(sampleDf, params)
    expect_doppelganger("Plot Data Frame", p)

    # the next line only needed until NS_params is upgraded
    params <- setColours(NS_params, c(Fishing = "red"))
    p <- plotDeath(params, species = "Haddock")
    expect_doppelganger("Plot Death", p)

    p <- plotResourcePred(NS_params)
    expect_doppelganger("Plot Resource Pred", p)

    p <- plotResourceLevel(NS_params)
    expect_doppelganger("Plot Resource", p)

    p <- plotEnergyBudget(NS_params, species = "Haddock")
    expect_doppelganger("Plot Energy Budget", p)

    p <- plotYieldVsSize(NS_params, species = "Haddock")
    expect_doppelganger("Plot Yield vs Size", p)
})

# plotly functions do not throw error
test_that("plotly functions do not throw error", {
    expect_error(plotlyDeath(params, species = species), NA)
    expect_error(plotlyResourcePred(params), NA)
    expect_error(plotlyEnergyBudget(params, species = species), NA)
    expect_error(plotlyYieldVsSize(params, species = species), NA)
})


# testing the plot outputs
test_that("return_data is identical",{
    expect_equal(dim(plotDeath(sim, species = species, return_data = TRUE)), c(784,4))

    expect_equal(dim(plotResourcePred(sim, return_data = TRUE)), c(612,3))

    expect_equal(dim(plotResourceLevel(sim, return_data = TRUE)), c(51,3))

    expect_equal(dim(plotEnergyBudget(sim, species = species, return_data = TRUE)[[1]]), c(224,4))

    expect_equal(dim(plotYieldVsSize(sim, species = species, return_data = TRUE)[[1]]), c(42,4))
}
)

# testing single-species and single-gear
test_that("plotYieldVsSize works with single species and single gear", {
    params_single <- newSingleSpeciesParams()
    initial_effort(params_single) <- 1
    expect_error(plotYieldVsSize(params_single, species = "Target species",
                                 gear = "knife_edge_gear"),
                 NA)
})

death_test_params <- function() {
    newMultispeciesParams(NS_species_params[1:2, ], no_w = 20)
}

test_that("plotDeath uses checked causes and authoritative proportions", {
    p <- death_test_params()
    initial_effort(p) <- 1
    data <- plotDeath(p, proportion = FALSE, return_data = TRUE)
    report <- attr(data, "mortality_breakdown")
    expect_true(report$complete)
    expect_named(data, c("w", "value", "Cause", "Prey"))
    expect_true(all(p$species_params$species %in% data$Cause))
    summed <- aggregate(value ~ w + Prey, data, sum)
    for (i in seq_len(nrow(summed))) {
        size <- match(summed$w[i], p$w)
        expect_equal(summed$value[i],
                     as.numeric(report$total[summed$Prey[i], size]))
    }
    fractions <- plotDeath(p, return_data = TRUE)
    summed <- aggregate(value ~ w + Prey, fractions, sum)
    expect_equal(summed$value, rep(1, nrow(summed)))
    expect_true(all(c("id", "label", "colour") %in%
                        names(attr(data, "series"))))
})

test_that("plotDeath includes extension causes without a second rate pipeline", {
    count <- 0L
    rlang::local_bindings(
        death_counted_rates = function(params, ...) {
            count <<- count + 1L
            mizerRates(params, ...)
        },
        death_declared_total = function(params, total, ...) {
            list(causes = list(author = total), complete = TRUE,
                 metadata = data.frame(id = "author", label = "Authored",
                                       colour = "red"))
        },
        death_additive_mort = function(params, n, ...) n * 0 + 0.2,
        .env = .GlobalEnv
    )
    p <- death_test_params()
    other_mort(p)$extra <- "death_additive_mort"
    data <- plotDeath(p, proportion = FALSE, return_data = TRUE)
    report <- attr(data, "mortality_breakdown")
    expect_true(report$complete)
    expect_true("contribution:extra" %in% names(report$causes))
    expect_equal(as.numeric(report$causes[["contribution:extra"]]),
                 rep(0.2, length(p$initial_n)))
    p <- setRateFunction(p, "Rates", "death_counted_rates")
    p <- setProcess(p, "consumers", mortality_fun = "death_declared_total")
    count <- 0L
    data <- plotDeath(p, return_data = TRUE)
    expect_identical(count, 1L)
    expect_identical(unique(data$Cause), "Authored")
    expect_equal(data$value, rep(1, nrow(data)))
})

test_that("plotDeath labels partial coverage and does not invent attribution", {
    rlang::local_bindings(
        death_partial = function(params, total, ...) {
            list(causes = list(known = total / 2), complete = FALSE)
        },
        death_wrong_complete = function(params, total, ...) {
            list(causes = list(known = total / 2), complete = TRUE)
        },
        death_unregistered_rates = function(params, ...) mizerRates(params, ...),
        .env = .GlobalEnv
    )
    p <- death_test_params()
    p <- setProcess(p, "consumers", mortality_fun = "death_partial")
    data <- plotDeath(p, return_data = TRUE)
    expect_equal(data$value, rep(0.5, nrow(data)))
    expect_false(attr(data, "mortality_breakdown")$complete)
    expect_identical(unique(data$Cause), "known")
    expect_match(plotDeath(p)$labels$caption, "Incomplete mortality breakdown")
    p <- setProcess(p, "consumers", mortality_fun = "death_wrong_complete")
    expect_error(plotDeath(p), "Complete mortality decomposition disagrees")
    p <- setRateFunction(p, "Rates", "death_unregistered_rates")
    expect_error(plotDeath(p), "Mortality breakdown is unavailable")
})

test_that("plotDeath preserves distinct IDs when labels coincide", {
    rlang::local_bindings(
        death_duplicate_labels = function(params, total, ...) {
            list(causes = list(first = total / 2, second = total / 2),
                 complete = TRUE,
                 metadata = data.frame(id = c("first", "second"),
                                       label = c("Same", "Same")))
        }, .env = .GlobalEnv
    )
    p <- setProcess(death_test_params(), "consumers",
                    mortality_fun = "death_duplicate_labels")
    data <- plotDeath(p, return_data = TRUE)
    expect_setequal(data$Cause, c("first", "second"))
    expect_identical(attr(data, "series")$label, c("Same", "Same"))
    expect_error(ggplot2::ggplot_build(plotDeath(p)), NA)
})

test_that("plotDeath retains aggregate modified predation when needed", {
    rlang::local_bindings(
        death_modified_predation = function(params, ...) {
            2 * mizerPredMort(params, ...)
        }, .env = .GlobalEnv
    )
    p <- setRateFunction(death_test_params(), "PredMort",
                         "death_modified_predation")
    data <- plotDeath(p, return_data = TRUE)
    expect_true("Predation" %in% data$Cause)
    expect_false(any(p$species_params$species %in% data$Cause))
    expect_true(attr(data, "mortality_breakdown")$complete)
})

test_that("plotDeath retains legacy controls and simulation state selection", {
    p <- death_test_params()
    simulation <- project(p, effort = 1, t_max = 0.2, dt = 0.1, t_save = 0.1)
    species <- as.character(p$species_params$species[1])
    data <- plotDeath(simulation, species, FALSE, TRUE, "identity")
    expected <- initialParams(simulation)
    initial_effort(expected) <- simulation$effort[nrow(simulation$effort), ]
    reference <- plotDeath(expected, species, FALSE, TRUE, "identity")
    expect_equal(lapply(data, identity), lapply(reference, identity))
    expect_identical(unique(data$Prey), species)
    plot <- plotDeath(expected, species, FALSE, FALSE, "identity")
    expect_identical(plot$scales$get_scales("x")$trans$name, "identity")
    expect_identical(plot$scales$get_scales("y")$name, "Death rate [1/year]")
})

test_that("Zero mortality is drawn without undefined proportions", {
    p <- death_test_params()
    p$initial_n[] <- 0
    initial_effort(p) <- 0
    ext_mort(p) <- p$mu_b * 0
    data <- plotDeath(p, return_data = TRUE)
    expect_true(all(is.finite(data$value)))
    expect_true(all(data$value == 0))
})
