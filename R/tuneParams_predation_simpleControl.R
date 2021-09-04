predation_simpleControlUI <- function(p, sp) {
    sp_sel <- p@species_params$species == sp$species
    w_min_idx <- p@w_min_idx[sp_sel]
    w_min <- p@w[w_min_idx]
    e0 <- getEncounter(p)[sp_sel, w_min_idx]
    h0 <- getMaxIntakeRate(p)[sp_sel, w_min_idx]
    f0 <- e0 / (e0 + h0)
    tagList(
        tags$h3(tags$a(id = "predation"), "Predation"),
        sliderInput("gamma", "Predation rate coefficient 'gamma'",
                    value = sp$gamma,
                    min = signif(sp$gamma / 2, 3),
                    max = signif(sp$gamma * 1.5, 3),
                    ticks = FALSE),
        sliderInput("f0", "Feeding level at smallest size",
                    value = f0,
                    min = 0,
                    max = 1)
    )
}

predation_simpleControl <- function(input, output, session, params, flags) {

    ## Adjust predation ####
    observeEvent(
        list(input$gamma, input$f0),
        {
            p <- params()
            sp <- input$sp
            sp_sel <- p@species_params$species == sp
            if (!identical(sp, flags$sp_old_pred)) {
                flags$sp_old_pred <- sp
                return()
            }
            w_min_idx <- p@w_min_idx[sp_sel]
            
            # adjust gamma
            updateSliderInput(session, "gamma",
                              min = signif(input$gamma / 2, 3),
                              max = signif(input$gamma * 1.5, 3))
            p@species_params[sp, "gamma"] <- input$gamma
            p <- setSearchVolume(p)
            
            # adjust h
            w_min <- p@w[w_min_idx]
            e <- getEncounter(p)[sp_sel, w_min_idx]
            h <- e * w_min ^ -p@species_params[sp, "n"] *
                (1 - input$f0) / input$f0
            
            # e0 <- getEncounter(p)[sp_sel, w_min_idx]
            # h0 <- getMaxIntakeRate(p)[sp_sel, w_min_idx]
            # f0 <- e0 / (e0 + h0)
            # h <- p@species_params[sp, "h"] * f0 / input$f0
            p@species_params[sp, "h"]     <- h
            p <- setMaxIntakeRate(p)
            
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)

}