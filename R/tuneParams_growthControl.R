growthControlUI <- function(p, sp) {
    sp_sel <- p@species_params$species == sp$species
    w_min_idx <- p@w_min_idx[sp_sel]
    w_min <- p@w[w_min_idx]
    e0 <- getEncounter(p)[sp_sel, w_min_idx]
    h0 <- getMaxIntakeRate(p)[sp_sel, w_min_idx]
    f0 <- e0 / (e0 + h0)
    kappa <- resource_params(p)$kappa
    tagList(
        tags$h3(tags$a(id = "predation"), "Predation"),
        sliderInput("gamma", "Predation rate coefficient 'gamma'",
                    value = sp$gamma,
                    min = signif(sp$gamma / 2, 3),
                    max = signif(sp$gamma * 1.5, 3),
                    ticks = FALSE),
        tags$h3(tags$a(id = "resource"), "Resource"),
        popify(sliderInput("kappa",
                           "Power-law coefficient 'kappa'",
                           value = kappa, 
                           min = signif(kappa / 2, 3), 
                           max = signif(kappa * 1.5, 3),
                           ticks = FALSE),
               title = "Adjusting resource abundance",
               content = "Adjusting the resource abundance allows you to line up your community spectrum with the resource spectrum, for example to achieve a more constant feeding level."
        )
    )
}

growthControl <- function(input, output, session, params, flags,
                                    ...) {

    ## Adjust predation ####
    observeEvent(input$gamma, {
        p <- params()
        sp <- input$sp
        sp_sel <- p@species_params$species == sp
        if (!identical(sp, flags$sp_old_pred)) {
            flags$sp_old_pred <- sp
            return()
        }
        factor <- input$gamma / p@species_params[sp, "gamma"]
        
        # adjust gamma
        updateSliderInput(session, "gamma",
                          min = signif(input$gamma / 2, 3),
                          max = signif(input$gamma * 1.5, 3))
        p@species_params[sp, "gamma"] <- input$gamma
        p <- setSearchVolume(p)
        
        # adjust h
        p@species_params[sp, "h"] <- p@species_params[sp, "h"] * factor
        p <- setMaxIntakeRate(p)
        
        tuneParams_update_species(sp, p, params)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
    )
    
    ## Scale background ####
    observeEvent(input$kappa, {
        p <- params()
        sp <- input$sp
        if (!identical(sp, flags$sp_old_kappa)) {
            flags$sp_old_kappa <- sp
            return()
        }
        factor <- resource_params(p)$kappa / input$kappa
        p <- scaleDownBackground(p, factor)
        gamma <- species_params(p)[sp, "gamma"]
        updateSliderInput(session, "kappa",
                          min = signif(input$kappa / 2, 3), 
                          max = signif(input$kappa * 1.5, 3)
        )
        # avoid triggering input$gamma
        flags$sp_old_pred <- NULL
        updateSliderInput(session, "gamma",
                          value = gamma,
                          min = signif(gamma / 2, 3), 
                          max = signif(gamma * 1.5, 3)
        )
        params(p)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
}