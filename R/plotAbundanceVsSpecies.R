# The following is a copy of the code for `plotBiomassVsSpecies()` just with
# the text replacements "Biomasses" -> "Abundances", "Biomass" -> "Abundance"
# and the corresponding lower-case versions, in that order.

#' Plot the abundance against species
#' 
#' @param params A MizerParams object
#' @export
plotAbundanceVsSpecies <- function(params) {
    no_sp <- length(params@species_params$species)
    cutoff <- params@species_params$abundance_cutoff
    # When no cutoff known, set it to 0
    if (is.null(cutoff)) cutoff <- rep(0, no_sp)
    cutoff[is.na(cutoff)] <- 0
    observed <- params@species_params$abundance_observed
    if (is.null(observed)) observed <- rep(NA, no_sp)
    
    # selector for foreground species
    foreground <- !is.na(params@A)
    foreground_indices <- (1:no_sp)[foreground]
    abundance_model <- foreground_indices  # create vector of right length
    for (i in seq_along(foreground_indices)) {
        sp <- foreground_indices[i]
        abundance_model[i] <- sum((params@initial_n[sp, ] * params@w * params@dw)
                                [params@w >= cutoff[[sp]]])
    }
    species <- factor(params@species_params$species[foreground],
                      levels = params@species_params$species[foreground])
    df <- rbind(
        data.frame(Species = species,
                   Type = "Observation",
                   Abundance = observed[foreground],
                   other = abundance_model),
        data.frame(Species = species,
                   Type = "Model",
                   Abundance = abundance_model,
                   other = observed[foreground])
    )
    # Get rid of unobserved entries
    df <- df[df$Abundance > 0 & !is.na(df$Abundance), ] 
    
    ggplot(df, aes(x = Species, y = Abundance)) +
        geom_point(aes(shape = Type), size = 4) +
        geom_linerange(aes(ymin = Abundance, ymax = other, colour = Species)) +
        scale_y_continuous(name = "Abundance [g]", trans = "log10",
                           breaks = log_breaks()) + 
        scale_colour_manual(values = getColours(params)) +
        scale_shape_manual(values = c(Model = 1, Observation = 15)) +
        guides(colour = "none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
