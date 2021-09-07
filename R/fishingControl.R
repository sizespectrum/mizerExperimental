#' Controlling the fishing parameters in the tuning gadget
#' @inheritParams abundanceControl
fishingControl <- function(input, output, session, params, params_old, flags, ...) {
    observeEvent(
        list(input$catchability,
             input$effort,
             input$knife_edge_size,
             input$l50,
             input$ldiff),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_fishing)) {
                flags$sp_old_fishing <- sp
                return()
            }
            gp_idx <- which(p@gear_params$species == sp)
            if (length(gp_idx) != 1) {
                showModal(modalDialog(
                    title = "Invalid gear specification",
                    HTML(paste0("Currently you can only use models where each ",
                                "species is caught by only one gear")),
                    easyClose = TRUE
                ))
            }
            
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            p@gear_params[gp_idx, "catchability"]  <- input$catchability
            updateSliderInput(session, "catchability",
                              min = signif(max(input$catchability / 2 - 1, 0), 2),
                              max = signif(max(input$catchability * 2, 2), 2))
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
                                  max = signif(input$l50 / 10, 2))
                p@gear_params[gp_idx, "l50"]   <- input$l50
                p@gear_params[gp_idx, "l25"]   <- input$l50 - input$ldiff
            }
            if (p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
                p@gear_params[gp_idx, "l50_right"]   <- input$l50_right
                p@gear_params[gp_idx, "l25_right"]   <- input$l50_right + input$ldiff_right
                updateSliderInput(session, "l50_right",
                                  max = signif(input$l50_right * 2, 2))
                updateSliderInput(session, "ldiff_right",
                                  max = signif(input$l50_right / 10, 2))
            }
            
            p <- setFishing(p, initial_effort = input$effort)
            tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
}

#' @rdname fishingControl
#' @inheritParams abundanceControlUI
fishingControlUI <- function(p, sp) {
    # If there are several gears, we only use the effort for the first.
    # If this is changed by the user, all efforts will be set the same.
    effort <- p@initial_effort[[1]]
    gp <- p@gear_params[p@gear_params$species == sp$species, ]
    if (nrow(gp) != 1) {
        showModal(modalDialog(
            title = "Invalid gear specification",
            HTML(paste0("Currently you can only use models where each ",
                        "species is caught by only one gear. In this model ",
                        sp$species, " is caught by ", nrow(gp), " gears.")),
            easyClose = TRUE
        ))
    }
    l1 <- list(tags$h3(tags$a(id = "fishing"), "Fishing"),
               sliderInput("catchability", "Catchability",
                           value = gp$catchability,
                           min = signif(max(0, gp$catchability / 2 - 1), 2),
                           max = signif(max(gp$catchability * 2, 2), 2),
                           step = 0.01),
               sliderInput("effort", "Effort",
                           value = effort,
                           min = 0,
                           max = signif((effort + 1) * 1.5, 2)))
    
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
                        max = signif(max(gp$l50 / 4, (gp$l50 - gp$l25)*1.1), 2),
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
                        max = signif(max(gp$l50 / 4, (gp$l50 - gp$l25)*1.1), 2),
                        step = 0.1),
            sliderInput("l50_right", "L50 right",
                        value = gp$l50_right,
                        min = 1,
                        max = signif(sp$l50_right * 2, 2),
                        step = 0.1),
            sliderInput("ldiff_right", "L50-L25 right",
                        value = gp$l25_right - gp$l50_right,
                        min = 0.1,
                        max = signif(max(gp$l50_right / 4,
                                         (gp$l25_right - gp$l50_right)*1.1), 2),
                        step = 0.1)
        ))
    }
    l1
}
