#' Animation of the abundance spectra
#'
#' @param sim A MizerSim object
#' @inheritParams plotSpectra
#' @export
#' @family plotting functions
#' @examples
#' sim <- project(NS_params, t_max = 15, effort = 1)
#' animateSpectra(sim)
animateSpectra <- function(sim,
                           species,
                           wlim = c(NA, NA),
                           ylim = c(NA, NA),
                           power = 1,
                           total = FALSE,
                           plankton = TRUE) {

    # Select species ----
    if (missing(species)) {
        # Set species to list of all non-background species
        species <- dimnames(sim@params@initial_n)$sp[!is.na(sim@params@A)]
    }
    species <- as.character(species)
    invalid_species <-
        !(species %in% as.character(dimnames(sim@n)$sp))
    if (any(invalid_species)) {
        warning(paste("The following species do not exist in the model and are ignored:",
                      species[invalid_species]))
    }
    nf <- melt(sim@n[, as.character(dimnames(sim@n)$sp) %in% species, , drop = FALSE])

    # Add plankton ----
    if (plankton) {
        nf_pp <- melt(sim@n_pp)
        nf_pp$sp <- "Plankton"
        nf <- rbind(nf, nf_pp)
    }
    # Add total ----
    if (total) {
        # Calculate total community abundance
        fish_idx <- (length(sim@params@w_full) -
                         length(sim@params@w) + 1):length(sim@params@w_full)
        total_n <- sim@n_pp
        total_n[, fish_idx] <- total_n[, fish_idx] +
            rowSums(aperm(sim@n, c(1, 3, 2)), dims = 2)
        nf_total <- melt(total_n)
        nf_total$sp <- "Total"
        nf <- rbind(nf, nf_total)
    }

    # Impose limits ----
    if (is.na(wlim[1])) wlim[1] <- min(sim@params@w) / 100
    if (is.na(wlim[2])) wlim[2] <- max(sim@params@w_full)
    if (is.na(ylim[1])) ylim[1] <- 10^-20
    if (is.na(ylim[2])) ylim[2] <- 10^20
    nf <- nf %>%
        filter(value >= ylim[1],
               value <= ylim[2],
               w >= wlim[1],
               w <= wlim[2])

    # Deal with power argument ----
    if (power %in% c(0, 1, 2)) {
        y_label <- c("Number density [1/g]", "Biomass density",
                     "Biomass density [g]")[power + 1]
    } else {
        y_label <- paste0("Number density * w^", power)
    }
    nf <- mutate(nf, value = value * w^power)

    nf %>%
        plot_ly() %>%
        add_lines(x = ~w, y = ~value,
                  color = ~sp, colors = sim@params@linecolour,
                  frame = ~time,
                  line = list(simplify = FALSE)) %>%
        layout(xaxis = list(type = "log", exponentformat = "power",
                            title = "Size [g]"),
               yaxis = list(type = "log", exponentformat = "power",
                            title = y_label))
}
