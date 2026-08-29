#' Plot the biomass against species
#'
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @param params A MizerParams object
#' @param ... Not used.
#' @examples
#' plotBiomassVsSpecies(NS_params)
#'

#' @export
plotBiomassVsSpecies <- function(params, ...) UseMethod("plotBiomassVsSpecies")

#' @export
plotBiomassVsSpecies.MizerParams <- function(params, ...) {
    no_sp <- length(params@species_params$species)
    observed <- params@species_params$biomass_observed
    if (is.null(observed)) observed <- rep(NA, no_sp)

    # selector for foreground species
    foreground <- !params@species_params$is_background
    # `getBiomass()` observes the `biomass_cutoff` column and integrates over
    # the size grid with the model's own quadrature scheme, so the model
    # biomass agrees with the one the calibration functions match.
    biomass_model <- unname(getBiomass(params, use_cutoff = TRUE)[foreground])
    species <- factor(params@species_params$species[foreground],
                      levels = params@species_params$species[foreground])
    df <- rbind(
        data.frame(Species = species,
                   Type = "Observation",
                   Biomass = observed[foreground],
                   other = biomass_model),
        data.frame(Species = species,
                   Type = "Model",
                   Biomass = biomass_model,
                   other = observed[foreground])
    )
    # Get rid of unobserved entries
    df <- df[df$Biomass > 0 & !is.na(df$Biomass), ] 
    
    ggplot(df, aes(x = Species, y = Biomass)) +
        geom_point(aes(shape = Type), size = 4) +
        geom_linerange(aes(ymin = Biomass, ymax = other, colour = Species)) +
        scale_y_continuous(name = "Biomass [g]", trans = "log10",
                           breaks = log_breaks()) + 
        scale_colour_manual(values = getColours(params)) +
        scale_shape_manual(values = c(Model = 1, Observation = 15)) +
        guides(colour = "none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
