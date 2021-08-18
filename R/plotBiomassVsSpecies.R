#' Plot the biomass against species
#' @export
plotBiomassVsSpecies <- function(params) {
    no_sp <- length(params@species_params$species)
    cutoff <- params@species_params$cutoff_size
    # When no cutoff known, set it to maturity weight / 20
    if (is.null(cutoff)) cutoff <- params@species_params$w_mat / 20
    cutoff[is.na(cutoff)] <- params@species_params$w_mat[is.na(cutoff)] / 20
    observed <- params@species_params$biomass_observed
    if (is.null(observed)) observed <- 0
    
    # selector for foreground species
    foreground <- !is.na(params@A)
    foreground_indices <- (1:no_sp)[foreground]
    biomass_model <- foreground_indices  # create vector of right length
    for (i in seq_along(foreground_indices)) {
        sp <- foreground_indices[i]
        biomass_model[i] <- sum((params@initial_n[sp, ] * params@w * params@dw)
                                [params@w >= cutoff[[sp]]])
    }
    species <- factor(params@species_params$species[foreground],
                      levels = params@species_params$species[foreground])
    df <- rbind(
        data.frame(Species = species,
                   Type = "Observation",
                   Biomass = observed[foreground]),
        data.frame(Species = species,
                   Type = "Model",
                   Biomass = biomass_model)
    )
    # Get rid of "Observed" entries for species without 
    # observations (where we have set observed = 0)
    df <- df[df$Biomass > 0, ] 
    
    ggplot(df) +
        geom_point(aes(x = Species, y = Biomass, colour = Type),
                   size = 8, alpha = 0.5) +
        scale_y_continuous(name = "Biomass [g]", trans = "log10",
                           breaks = log_breaks()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
