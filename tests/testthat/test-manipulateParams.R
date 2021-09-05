library(mizer)

# markBackground() ----
test_that("markBackground() works", {
    params <- markBackground(NS_params, species = "Cod")
    expect_identical(params@A[[11]], NA_real_)
    params <- markBackground(NS_params, species = c("Cod", "Dab"))
    expect_identical(params@A[[5]], NA_real_)
    sim <- markBackground(project(NS_params, t_max = 0.1))
    expect_true(all(is.na(sim@params@A)))
    expect_error(markBackground(1),
                 "The `object` argument must be of type MizerParams or MizerSim.")
})



# pruneSpecies() removes low-abundance species ----
test_that("pruneSpecies() removes low-abundance species", {
    params <- newTraitParams()
    p <- params
    # We multiply one of the species by a factor of 10^-4 and expect
    # pruneSpecies() to remove it.
    p@initial_n[5, ] <- p@initial_n[5, ] * 10^-4
    p <- pruneSpecies(p, 10^-2)
    expect_is(p, "MizerParams")
    expect_equal(nrow(params@species_params) - 1, nrow(p@species_params))
    expect_equal(p@initial_n[5, ], params@initial_n[6, ])
})

# adjustBackgroundSpecies() ----
test_that("adjustBackgroundSpecies", {
    expect_message(adjustBackgroundSpecies(NS_params),
                   "There are no background species left.")
})

test_that("adjustBackgroundSpecies() removes Cod", {
    params <- markBackground(NS_params, species = "Cod")
    expect_warning(
        expect_message(params <- adjustBackgroundSpecies(params),
                   "There are no background species left.")
    )
})

test_that("adjustBackgroundSpecies() reproduces scaling model", {
    # This numeric test failed on Solaris and without long doubles. So for now
    # skipping it on CRAN
    skip_on_cran()
    p <- newTraitParams(n = 2/3, lambda = 2 + 3/4 - 2/3) # q = 3/4
    initial_n <- p@initial_n
    # We multiply one of the species by a factor of 5 and expect
    # adjustBackgroundSpecies() to tune it back down to the original value.
    p@initial_n[5, ] <- 5 * p@initial_n[5, ]
    pr <- p %>%
        markBackground() %>%
        adjustBackgroundSpecies()
    expect_lt(max(abs(initial_n - pr@initial_n)), 2e-11)
})


# scaleAbundance ----
test_that("scaleAbundance works", {
    expect_warning(p <- setBevertonHolt(NS_params, reproduction_level = 1/4))
    factor <- c(Cod = 2, Haddock = 3)
    expect_warning(p2 <- scaleAbundance(NS_params, factor))
    expect_identical(p@initial_n["Cod"] * 2, p2@initial_n["Cod"])
    expect_unchanged(p, expect_warning(scaleAbundance(p2, 1/factor)))
})
test_that("scaleAbundance throws correct error",{
    expect_error(scaleAbundance(NS_params, c(2, 3)))
    expect_error(scaleAbundance(NS_params, "a"))
})
test_that("scaleAbundance warns on wrong names", {
    expect_error(scaleAbundance(NS_params, c(Kod = 2, Hadok = 3)),
                 "Kod, Hadok do not exist")
})
