#' Controlling the interaction matrix in the tuning gadget
#' @inheritParams abundanceControl
interactionControl <- function(input, output, session, params, flags, ...) {
    observe({
        req(input$interaction_resource)
        p <- isolate(params())
        sp <- isolate(input$sp)
        # The following req()a are required, otherwise changes in the sliders
        # do not trigger. I am not sure why.
        for (i in p@species_params$species[!is.na(p@A)]) {
            inter_var <- paste0("inter_", i)
            req(input[[inter_var]])
        }
        if (!identical(sp, flags$sp_old_inter)) {
            flags$sp_old_inter <- sp
            return()
        }
        p@species_params[sp, "interaction_resource"] <-
            input$interaction_resource
        for (i in p@species_params$species[!is.na(p@A)]) {
            inter_var <- paste0("inter_", i)
            p@interaction[sp, i] <- input[[inter_var]]
        }
        tuneParams_update_species(sp, p, params)
    })
}

#' @rdname interactionControl
#' @inheritParams abundanceControlUI
interactionControlUI <- function(p, sp) {
    l1 <- list(
        tags$h3(tags$a(id = "interaction"), "Prey interactions"),
        sliderInput("interaction_resource", "Resource",
                    value = sp$interaction_resource,
                    min = 0,
                    max = 1,
                    step = 0.001))
    for (i in p@species_params$species[!is.na(p@A)]) {
        inter_var <- paste0("inter_", i)
        l1 <- c(l1, list(
            sliderInput(inter_var, i,
                        value = p@interaction[sp$species, i],
                        min = 0,
                        max = 1,
                        step = 0.001)
        ))
    }
    l1
}
