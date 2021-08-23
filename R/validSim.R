#' Returns a valid simulation by removing non-finite results
#'
#' Checks whether any abundances are non-finite and if any are found, a warning
#' is issued and the simulation is truncated at the last time step where all
#' results are finite.
#'
#' @param sim A MizerSim object
#' @return A MizerSim object with possibly fewer time steps
#' @export
validSim <- function(sim) {
    assert_that(is(sim, "MizerSim"))
    if (!all(is.finite(sim@n))) {
        inf_idx <- which(!is.finite(sim@n), arr.ind = TRUE)
        max_t_idx <- min(inf_idx[, 1]) - 1
        max_t <- as.numeric(dimnames(sim@n)$time[max_t_idx])
        warning("The simulation failed to work beyond time = ", max_t)
        # we can't use drop = FALSE because we do want to drop time dimension.
        n <- sim@n[max_t_idx, , ]
        dim(n) <- dim(sim@n)[2:3]
        rates <- getRates(sim@params,
                          n = n,
                          n_pp = sim@n_pp[max_t_idx, ],
                          n_other = sim@n_pp[max_t_idx, ],
                          effort = sim@effort[max_t_idx, ],
                          t = max_t)
        inf_rates <- sapply(rates, function(x) any(!is.finite(x)))
        if (any(inf_rates)) {
            warning("The following rates failed to be finite: ",
                    paste(names(rates)[inf_rates], collapse = ", "), ".")
        }
        sim <- truncateSim(sim, end_time = max_t)
    }
    sim
}

truncateSim <- function(sim, end_time) {
    assert_that(is(sim, "MizerSim"))
    times <- dimnames(sim@n)$time
    if (!(end_time %in% times)) {
        stop("end_time = ", end_time, " is not a valid time contained in the simulation.")
    }
    t_idx <- match(end_time, times)
    sim@n <- sim@n[1:t_idx, , , drop = FALSE]
    sim@n_pp <- sim@n_pp[1:t_idx, , drop = FALSE]
    sim@n_other <- sim@n_other[1:t_idx, , drop = FALSE]
    sim@effort <- sim@effort[1:t_idx, , drop = FALSE]
    sim
}
