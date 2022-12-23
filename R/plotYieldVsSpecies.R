#' Plot the yield against species
#' 
#' @param params A MizerParams object
#' @param gear Optional. The name of a gear. If supplied, only the yield from
#'   this gear will be displayed.
#' @export
plotYieldVsSpecies <- function(params, gear = NULL) {
    params <- validParams(params)
    gp <- params@gear_params %>%
        set_species_param_default("yield_observed", NA)
    if (!is.null(gear)) {
        assert_that(is.character(gear),
                    length(gear) == 1)
        if (!(gear %in% params@gear_params$gear)) {
            stop("The gear ", gear, " does not exist.")
        }
        gp <- gp[gp$gear == gear, c("species", "yield_observed")]
    } else {
        gp <- gp %>%
            group_by(species) %>%
            summarise(yield_observed = sum(yield_observed))
    }
    no_sp <- length(params@species_params$species)
    observed <- vector(mode = "double", length = no_sp)
    names(observed) <- params@species_params$species
    observed[gp$species] <- gp$yield_observed
    
    biomass <- sweep(params@initial_n, 2, params@w * params@dw, "*")
    if (is.null(gear)) {
        f_mort <- getFMort(params)
    } else {
        f_mort <- getFMortGear(params)[gear, , ]
    }
    yield_model <- rowSums(biomass * f_mort)
    
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
    
    pl <- ggplot(df, aes(x = Species, y = Yield)) +
        geom_point(aes(shape = Type), size = 4) +
        geom_linerange(aes(ymin = Yield, ymax = other, colour = Species)) +
        scale_y_continuous(name = "Yield [g/year]", trans = "log10",
                           breaks = log_breaks()) + 
        scale_colour_manual(values = getColours(params)) +
        scale_shape_manual(values = c(Model = 1, Observation = 15)) +
        guides(colour = "none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    
    if (!is.null(gear)) {
        pl <- pl + ggtitle(paste("Gear:", gear))
    }
    return(pl)
}