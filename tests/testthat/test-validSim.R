test_that("validSim works", {
  sim <- project(NS_params, t_max = 0.2, t_save = 0.1)
  sim@n[3, 1, 1] <- Inf
  expect_warning(simt <- validSim(sim),
                 "The simulation failed to work beyond time = 0.1")
  expect_equal(dim(simt@n), c(2, 12, 100))
  expect_equal(dim(simt@n_pp), c(2, 226))
  expect_equal(dim(simt@effort), c(2, 4))
  sim@n[2, 2, 2] <- NaN
  expect_warning(simt <- validSim(sim),
                 "The simulation failed to work beyond time = 0")
  expect_equal(dim(simt@n), c(1, 12, 100))
})
