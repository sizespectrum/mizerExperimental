#' Controlling the reproduction parameters in the tuning gadget
#' @inheritParams abundanceControl
reproductionControl <- function(input, output, session, params, params_old,
                                flags, ...) {
    observeEvent(
        list(input$w_mat, input$wfrac, input$w_inf, input$m),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_repro)) {
                flags$sp_old_repro <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "w_mat",
                              min = signif(input$w_mat / 2, 2),
                              max = signif(input$w_mat * 1.5, 2))
            updateSliderInput(session, "w_inf",
                              min = signif(input$w_inf / 2, 2),
                              max = signif(input$w_inf * 1.5, 2))

            p@species_params[sp, "w_mat25"]   <- input$w_mat * input$wfrac
            p@species_params[sp, "w_mat"]   <- input$w_mat
            p@species_params[sp, "w_inf"]   <- input$w_inf
            p@species_params[sp, "m"]     <- input$m

            p <- setReproduction(p)
            tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
}

#' @rdname reproductionControl
#' @inheritParams abundanceControlUI
reproductionControlUI <- function(p, sp, re) {
    tagList(
        tags$h3(tags$a(id = "reproduction"), "Reproduction"),
        sliderInput("w_mat", "w_mat", value = sp$w_mat,
                    min = signif(sp$w_mat / 2, 2),
                    max = signif(sp$w_mat * 1.5, 2)),
        sliderInput("wfrac", "w_mat25/w_mat", value = sp$w_mat25/sp$w_mat,
                    min = 0.5,
                    max = 1,
                    step = 0.01),
        sliderInput("w_inf", "w_inf", value = sp$w_inf,
                    min = signif(sp$w_inf / 2, 2),
                    max = signif(sp$w_inf * 1.5, 2)),
        sliderInput("m", "m", value = sp$m,
                    min = sp$n,
                    max = sp$n * 2,
                    step = 0.01)
    )
}

