# The following is a copy of the code for `plotBiomassVsSpecies()` just with
# the text replacements "Biomass" -> "Number" and "biomass" to "number" and
# the removal of the `params@w` factor in the calculations.

#' Plot the number against species
#'
#' This is a generic function with a method for objects of class
#' [MizerParams][mizer::MizerParams].
#'
#' @param params A MizerParams object
#' @param ... Not used.
#' @examples
#' plotNumberVsSpecies(NS_params)
#'

#' @export
plotNumberVsSpecies <- function(params, ...) UseMethod("plotNumberVsSpecies")

#' @export
plotNumberVsSpecies.MizerParams <- function(params, ...) {
    no_sp <- length(params@species_params$species)
    cutoff <- params@species_params$number_cutoff
    # A species without a cutoff is counted over its whole size range
    if (is.null(cutoff)) cutoff <- rep(NA_real_, no_sp)
    cutoff[is.na(cutoff)] <- min(params@w)
    observed <- params@species_params$number_observed
    if (is.null(observed)) observed <- rep(NA, no_sp)

    # selector for foreground species
    foreground <- !params@species_params$is_background
    # `sizeIntegral()` integrates over the size grid with the model's own
    # quadrature scheme, and lets the bin straddling the cutoff contribute only
    # the part of it that lies above the cutoff.
    number_model <- unname(sizeIntegral(params, min_w = cutoff)[foreground])
    species <- factor(params@species_params$species[foreground],
                      levels = params@species_params$species[foreground])
    df <- rbind(
        data.frame(Species = species,
                   Type = "Observation",
                   Number = observed[foreground],
                   other = number_model),
        data.frame(Species = species,
                   Type = "Model",
                   Number = number_model,
                   other = observed[foreground])
    )
    # Get rid of unobserved entries
    df <- df[df$Number > 0 & !is.na(df$Number), ] 
    
    ggplot(df, aes(x = Species, y = Number)) +
        geom_point(aes(shape = Type), size = 4) +
        geom_linerange(aes(ymin = Number, ymax = other, colour = Species)) +
        scale_y_continuous(name = "Number [g]", trans = "log10",
                           breaks = log_breaks()) + 
        scale_colour_manual(values = getColours(params)) +
        scale_shape_manual(values = c(Model = 1, Observation = 15)) +
        guides(colour = "none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
