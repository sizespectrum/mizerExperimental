#' Controlling the species parameters not included in other controls
#' @inheritParams abundanceControl
#' @export
otherControl <- function(input, output, session, params, params_old,
                         flags, ...) {
    observe({
        req(input$alpha, input$ks, input$k, input$z0)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_other)) {
            flags$sp_old_other <- sp
            return()
        }
        # Update slider min/max so that they are a fixed proportion of the
        # parameter value
        updateSliderInput(session, "ks",
                          min = signif(input$ks / 2, 2),
                          max = signif((input$ks + 0.1) * 1.5, 2))
        updateSliderInput(session, "k",
                          min = signif(input$k / 2, 2),
                          max = signif((input$k + 0.1) * 1.5, 2))
        updateSliderInput(session, "z0",
                          min = signif(input$z0 / 2, 2),
                          max = signif((input$z0 + 0.1) * 1.5, 2))

        p@species_params[sp, "alpha"] <- input$alpha
        p@species_params[sp, "ks"]    <- input$ks
        p@species_params[sp, "k"]     <- input$k
        p@species_params[sp, "z0"]    <- input$z0
        p <- setMetabolicRate(p)
        p <- setExtMort(p)
        tuneParams_update_species(sp, p, params, params_old)
    })

    observeEvent(
        input$p,
        {
            p <- params()
            sp <- input$sp

            # change ks so that metabolic rate at maturity stays the same
            p@species_params[[sp, "ks"]] <- p@species_params[[sp, "ks"]] *
                p@species_params[[sp, "w_mat"]] ^
                (p@species_params[[sp, "p"]] - input$p)
            p@species_params[[sp, "p"]] <- input$p
            ks <- p@species_params[[sp, "ks"]]
            updateSliderInput(session, "ks",
                              value = ks,
                              min = signif(ks / 2, 2),
                              max = signif((ks + 0.1) * 1.5, 2))
        },
        ignoreInit = TRUE)
}

#' @rdname otherControl
#' @inheritParams abundanceControlUI
#' @export
otherControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    tagList(
        tags$h3(tags$a(id = "other"), "Other"),
        sliderInput("ks", "Coefficient of standard metabolism 'ks'",
                    value = sp$ks,
                    min = signif(sp$ks / 2, 2),
                    max = signif((sp$ks + 0.1) * 1.5, 2),
                    step = 0.05),
        numericInput("p", "Exponent of metabolism 'p'",
                     value = sp$p,
                     min = 0.6, max = 0.8, step = 0.005),
        sliderInput("k", "Coefficient of activity 'k'",
                    value = sp$k,
                    min = signif(sp$k / 2, 2),
                    max = signif((sp$k + 0.1) * 1.5, 2),
                    step = 0.01),
        sliderInput("z0", "External mortality 'z0'",
                    value = sp$z0,
                    min = signif(sp$z0 / 2, 2),
                    max = signif((sp$z0 + 0.1) * 1.5, 2),
                    step = 0.05),
        sliderInput("alpha", "Assimilation efficiency 'alpha'",
                    value = sp$alpha,
                    min = 0,
                    max = 1)
    )
}
