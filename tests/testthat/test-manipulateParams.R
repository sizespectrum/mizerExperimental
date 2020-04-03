library(mizer)

# removeSpecies ----
test_that("removeSpecies works", {
    remove <- NS_species_params$species[2:11]
    reduced <- NS_species_params[!(NS_species_params$species %in% remove), ]
    params <- MizerParams(NS_species_params, no_w = 20,
                          max_w = 39900, min_w_pp = 9e-14)
    p1 <- removeSpecies(params, species = remove)
    expect_equal(nrow(p1@species_params), nrow(params@species_params) - 10)
    p2 <- MizerParams(reduced, no_w = 20,
                      max_w = 39900, min_w_pp = 9e-14)
    expect_equivalent(p1, p2)
    sim1 <- project(p1, t_max = 0.4, t_save = 0.4)
    sim2 <- project(p2, t_max = 0.4, t_save = 0.4)
    expect_identical(sim1@n[2, 2, ], sim2@n[2, 2, ])
})

# retuneBackground() reproduces scaling model ----
test_that("retuneBackground() reproduces scaling model", {
    # This numeric test failed on Solaris and without long doubles. So for now
    # skipping it on CRAN
    skip_on_cran()
    p <- newTraitParams(n = 2/3, lambda = 2 + 3/4 - 2/3) # q = 3/4
    initial_n <- p@initial_n
    # We multiply one of the species by a factor of 5 and expect
    # retuneBackground() to tune it back down to the original value.
    p@initial_n[5, ] <- 5 * p@initial_n[5, ]
    pr <- p %>%
        markBackground() %>%
        retuneBackground()
    expect_lt(max(abs(initial_n - pr@initial_n)), 2e-11)
})

# pruneSpecies() removes low-abundance species ----
test_that("pruneSpecies() removes low-abundance species", {
    params <- newTraitParams()
    p <- params
    # We multiply one of the species by a factor of 10^-3 and expect
    # pruneSpecies() to remove it.
    p@initial_n[5, ] <- p@initial_n[5, ] * 10^-4
    p <- pruneSpecies(p, 10^-2)
    expect_is(p, "MizerParams")
    expect_equal(nrow(params@species_params) - 1, nrow(p@species_params))
    expect_equal(p@initial_n[5, ], params@initial_n[6, ])
})

# addSpecies ----
test_that("addSpecies works when adding a second identical species", {
    p <- newTraitParams()
    no_sp <- length(p@A)
    p <- markBackground(p)
    species_params <- p@species_params[5,]
    species_params$species = "new"
    # Adding species 5 again should lead two copies of the species with the
    # same combined abundance
    pa <- addSpecies(p, species_params)
    # TODO: think about what to check now
})
test_that("addSpecies does not allow duplicate species", {
    p <- NS_params
    species_params <- p@species_params[5, ]
    expect_error(addSpecies(p, species_params),
                 "You can not add species that are already there.")
})

# retuneReproductiveEfficiency ----
test_that("retuneReproductiveEfficiency works", {
    p <- newTraitParams(rfac = Inf)
    no_sp <- nrow(p@species_params)
    erepro <- p@species_params$erepro
    p@species_params$erepro[5] <- 15
    ps <- retune_erepro(p)
    expect_equal(ps@species_params$erepro, erepro)
    # can also select species in various ways
    ps <- retune_erepro(p, species = p@species_params$species[5])
    expect_equal(ps@species_params$erepro, erepro)
    p@species_params$erepro[3] <- 15
    species <- (1:no_sp) %in% c(3,5)
    ps <- retune_erepro(p, species = species)
    expect_equal(ps@species_params$erepro, erepro)
})

# renameSpecies ----
test_that("renameSpecies works", {
    sp <- NS_species_params
    p <- newMultispeciesParams(sp)
    sp$species <- tolower(sp$species)
    replace <- NS_species_params$species
    names(replace) <- sp$species
    p2 <- newMultispeciesParams(sp)
    p2 <- renameSpecies(p2, replace)
    expect_identical(p, p2)
})
test_that("renameSpecies warns on wrong names", {
    expect_error(renameSpecies(NS_params, c(Kod = "cod", Hadok = "haddock")),
                 "Kod, Hadok do not exist")
})

# rescaleAbundance ----
test_that("rescaleAbundance works", {
    p <- retune_erepro(NS_params)
    factor <- c(Cod = 2, Haddock = 3)
    p2 <- rescaleAbundance(NS_params, factor)
    expect_identical(p@initial_n["Cod"] * 2, p2@initial_n["Cod"])
    expect_equal(p, rescaleAbundance(p2, 1/factor))
})
test_that("rescaleAbundance throws correct error",{
    expect_error(rescaleAbundance(NS_params, c(2, 3)))
    expect_error(rescaleAbundance(NS_params, "a"))
})
test_that("rescaleAbundance warns on wrong names", {
    expect_error(rescaleAbundance(NS_params, c(Kod = 2, Hadok = 3)),
                 "Kod, Hadok do not exist")
})

# rescaleSystem ----
test_that("rescaleSystem does not change dynamics.", {
    factor <- 10
    sim <- project(NS_params, t_max = 1)
    params2 <- rescaleSystem(NS_params, factor)
    sim2 <- project(params2, t_max = 1)
    expect_equal(sim2@n[1, , ], sim@n[1, , ] * factor)
    expect_equal(sim2@n[2, , ], sim@n[2, , ] * factor)
})
