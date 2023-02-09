#' Controlling the fishing parameters in the tuning gadget
#' @inheritParams abundanceControl
fishingControl <- function(input, output, session, params, params_old, 
                           flags, ...) {
    observe({
        gear <- isolate(input$gear)
        p <- isolate(params())
        sp <- isolate(input$sp)
        req(input$catchability,
            input$effort)
        
        if (!identical(sp, flags$sp_old_fishing)) {
            flags$sp_old_fishing <- sp
            return()
        }
        gp_idx <- which(p@gear_params$species == sp &
                            p@gear_params$gear == gear)
        
        # Update slider min/max so that they are a fixed proportion of the
        # parameter value
        p@gear_params[gp_idx, "catchability"]  <- input$catchability
        updateSliderInput(session, "catchability",
                          min = signif(max(input$catchability / 2 - 1, 0), 2),
                          max = signif(max(input$catchability * 2, 2), 2))
        
        initial_effort(p)[gear] <- input$effort
        updateSliderInput(session, "effort",
                          max = signif((input$effort + 1) * 1.5, 2))
        
        if (p@gear_params[gp_idx, "sel_func"] == "knife_edge") {
            updateSliderInput(session, "knife_edge_size",
                              max = signif(input$knife_edge_size * 2, 2))
            p@gear_params[gp_idx, "knife_edge_size"]   <- input$knife_edge_size
        }
        if (p@gear_params[gp_idx, "sel_func"] == "sigmoid_length" ||
            p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
            updateSliderInput(session, "l50",
                              max = signif(input$l50 * 2, 2))
            updateSliderInput(session, "ldiff",
                              max = signif(input$ldiff * 2, 2))
            p@gear_params[gp_idx, "l50"]   <- input$l50
            p@gear_params[gp_idx, "l25"]   <- input$l50 - input$ldiff
        }
        if (p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
            p@gear_params[gp_idx, "l50_right"]   <- input$l50_right
            p@gear_params[gp_idx, "l25_right"]   <- input$l50_right + input$ldiff_right
            updateSliderInput(session, "l50_right",
                              max = signif(input$l50_right * 2, 2))
            updateSliderInput(session, "ldiff_right",
                              max = signif(input$ldiff_right * 2, 2))
        }
        
        p <- setFishing(p)
        tuneParams_update_species(sp, p, params, params_old)
    })
    
    observeEvent(input$gear, {
        gear <- input$gear
        p <- params()
        sp <- input$sp
        
        gp_idx <- which(p@gear_params$species == sp &
                            p@gear_params$gear == gear)
        
        catchability <- p@gear_params[gp_idx, "catchability"]
        updateSliderInput(session, "catchability",
                          value = catchability,
                          min = signif(max(catchability / 2 - 1, 0), 2),
                          max = signif(max(catchability * 2, 2), 2))
        
        effort <- initial_effort(p)[[gear]]
        updateSliderInput(session, "effort",
                          value = effort,
                          max = signif((effort + 1) * 1.5, 2))
        
        if (p@gear_params[gp_idx, "sel_func"] == "knife_edge") {
            knife_edge_size <- p@gear_params[gp_idx, "knife_edge_size"]
            updateSliderInput(session, "knife_edge_size",
                              value = knife_edge_size,
                              max = signif(knife_edge_size * 2, 2))
        }
        if (p@gear_params[gp_idx, "sel_func"] == "sigmoid_length" ||
            p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
            l50 <- p@gear_params[gp_idx, "l50"]
            ldiff <- p@gear_params[gp_idx, "l50"] - p@gear_params[gp_idx, "l25"]
            updateSliderInput(session, "l50",
                              value = l50,
                              max = signif(l50 * 2, 2))
            updateSliderInput(session, "ldiff",
                              value = ldiff,
                              max = signif(ldiff * 2, 2))
        }
        if (p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
            l50_right <- p@gear_params[gp_idx, "l50_right"]
            ldiff_right <- p@gear_params[gp_idx, "l50_right"] - 
                p@gear_params[gp_idx, "l25_right"]
            updateSliderInput(session, "l50_right",
                              value = l50_right,
                              max = signif(l50_right * 2, 2))
            updateSliderInput(session, "ldiff_right",
                              value = ldiff_right,
                              max = signif(ldiff_right * 2, 2))
        }
    },
    ignoreInit = TRUE)
}

#' @rdname fishingControl
#' @inheritParams abundanceControlUI
fishingControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    gp <- p@gear_params[p@gear_params$species == sp$species, ]
    if (nrow(gp) == 0) { # Species not selected by any gears
        return(tagList())
    }
    gears <- as.character(gp$gear)
    if (is.null(input$gear) || !(input$gear %in% gears)) {
        gear <- gears[[1]]
    } else {
        gear <- input$gear
    }
    
    effort <- p@initial_effort[[gear]]
    gp <- gp[gp$gear == gear, ]
    l1 <- list(tags$h3(tags$a(id = "fishing"), "Fishing"),
               selectInput("gear", "Gear to tune:", gears, 
                           selected = gear),
               sliderInput("catchability", "Catchability",
                           value = gp$catchability,
                           min = signif(max(0, gp$catchability / 2 - 1), 2),
                           max = signif(max(gp$catchability * 2, 2), 2),
                           step = 0.01),
               sliderInput("effort", "Effort",
                           value = effort,
                           min = 0,
                           max = signif((effort + 1) * 1.5, 2),
                           step = 0.01))
    
    if (gp$sel_func == "knife_edge") {
        l1 <- c(l1, list(
            sliderInput("knife_edge_size", "knife_edge_size",
                        value = gp$knife_edge_size,
                        min = 1,
                        max = signif(gp$knife_edge_size * 2, 2),
                        step = 0.1)))
    } else if (gp$sel_func == "sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput("l50", "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput("ldiff", "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif((gp$l50 - gp$l25) * 2, 2),
                        step = 0.1)))
    } else if (gp$sel_func == "double_sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput("l50", "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput("ldiff", "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif((gp$l50 - gp$l25) * 2, 2),
                        step = 0.1),
            sliderInput("l50_right", "L50 right",
                        value = gp$l50_right,
                        min = 1,
                        max = signif(sp$l50_right * 2, 2),
                        step = 0.1),
            sliderInput("ldiff_right", "L50-L25 right",
                        value = gp$l25_right - gp$l50_right,
                        min = 0.1,
                        max = signif((gp$l50_right - gp$l25_right) * 2, 2),
                        step = 0.1)
        ))
    }
    l1
}
