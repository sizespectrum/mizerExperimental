#' Plot the yield against species
#' 
#' @param params A MizerParams object
#' @export
plotYieldVsSpecies <- function(params) {
    no_sp <- length(params@species_params$species)
    observed <- params@species_params$yield_observed
    if (is.null(observed)) observed <- rep(NA, no_sp)
    
    biomass <- sweep(params@initial_n, 2, params@w * params@dw, "*")
    yield_model <- rowSums(biomass * getFMort(params))
    
    # selector for foreground species
    foreground <- !is.na(params@A)
    foreground_indices <- (1:no_sp)[foreground]
    yield_model <- yield_model[foreground_indices]
    observed <- observed[foreground_indices]
    species <- factor(params@species_params$species[foreground],
                      levels = params@species_params$species[foreground])
    df <- rbind(
        data.frame(Species = species,
                   Type = "Observation",
                   Yield = observed,
                   other = yield_model),
        data.frame(Species = species,
                   Type = "Model",
                   Yield = yield_model,
                   other = observed)
    )
    # Get rid of unobserved entries
    df <- df[df$Yield > 0 & !is.na(df$Yield), ] 
    
    ggplot(df, aes(x = Species, y = Yield)) +
        geom_point(aes(shape = Type), size = 4) +
        geom_linerange(aes(ymin = Yield, ymax = other, colour = Species)) +
        scale_y_continuous(name = "Yield [g/year]", trans = "log10",
                           breaks = log_breaks()) + 
        scale_colour_manual(values = getColours(params)) +
        scale_shape_manual(values = c(Model = 1, Observation = 15)) +
        guides(colour = "none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}