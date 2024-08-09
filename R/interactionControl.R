#' Controlling the interaction matrix in the tuning gadget
#' @inheritParams abundanceControl
interactionControl <- function(input, output, session, params,
                               params_old, flags, ...) {
    observe({ # change in prey or predator slider
        req(input$interaction_resource,
            input$prey_inter,
            input$pred_inter)
        p <- isolate(params())
        sp <- isolate(input$sp)
        
        if (!identical(sp, flags$sp_old_inter)) {
            flags$sp_old_inter <- sp
            return()
        }
        p@species_params[sp, "interaction_resource"] <-
            input$interaction_resource
        updateSliderInput(session, "interaction_resource",
                          max = 2 * (input$interaction_resource + 0.5))
        
        
        p@interaction[sp, isolate(input$prey_sp)] <- input$prey_inter
        updateSliderInput(session, "prey_inter",
                          max = 2 * (input$prey_inter + 0.5))
        
        p@interaction[isolate(input$pred_sp), sp] <- input$pred_inter
        updateSliderInput(session, "pred_inter",
                          max = 2 * (input$pred_inter + 0.5))
        
        tuneParams_update_species(sp, p, params, params_old)
    })
    
    observe({ # Change in prey species selector
        req(input$prey_sp)
        p <- isolate(params())
        sp <- isolate(input$sp)
        updateSliderInput(session, "prey_inter",
                          value = p@interaction[sp, input$prey_sp],
                          max = 2 * (p@interaction[sp, input$prey_sp] + 0.5))
    })
    observe({ # Change in predator species selector
        req(input$pred_sp)
        p <- isolate(params())
        sp <- isolate(input$sp)
        updateSliderInput(session, "pred_inter",
                          value = p@interaction[input$pred_sp, sp],
                          max = 2 * (p@interaction[input$pred_sp, sp] + 0.5))
    })
}

#' @rdname interactionControl
#' @inheritParams abundanceControlUI
interactionControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    l1 <- list(
        tags$h3(tags$a(id = "interaction"), "Interaction matrix"),
        sliderInput("interaction_resource", "Resource",
                    value = sp$interaction_resource,
                    min = 0,
                    max = 2 * (sp$interaction_resource + 0.5),
                    step = 0.1),
        selectInput("prey_sp", "Prey species",
                    choices = p@species_params$species),
        sliderInput("prey_inter", "Interaction strength",
                    value = p@interaction[sp$species, 1],
                    min = 0,
                    max = 2 * (p@interaction[sp$species, 1] + 0.5),
                    step = 0.1),
        selectInput("pred_sp", "Predator species",
                    choices = p@species_params$species),
        sliderInput("pred_inter", "Interaction strength",
                    value = p@interaction[1, sp$species],
                    min = 0,
                    max = 2 * (p@interaction[sp$species, 1] + 0.5),
                    step = 0.1)
    )
    l1
}
