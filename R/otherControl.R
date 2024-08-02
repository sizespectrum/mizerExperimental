#' Controlling the species parameters not included in other controls
#'
#'   Adds controls for the parameters that did not fit into other categories.
#'
#'   The parameters included are:
#' *  conversion efficiency `alpha`
#' *  metabolic rate coefficient `ks` and exponent `p`
#' *  activity rate coefficient `k`
#' *  external mortality at maturity size
#'
#'   If the external mortality at maturity size is changed, then the
#'   external mortality at all other sizes is scaled by the same factor.
#'
#'   If the metabolism exponent `p` is changed, this also changes `ks` so
#'   that metabolic rate at maturity stays the same.
#'   
#' @inheritParams abundanceControl
otherControl <- function(input, output, session, params, params_old,
                         flags, ...) {
    observe({
        req(input$alpha, input$ks, input$k, input$mu_mat)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_other)) {
            flags$sp_old_other <- sp
            return()
        }
        # determine external mortality at maturity
        mat_idx <- sum(p@w < p@species_params[sp, "w_mat"])
        mu_mat <- ext_mort(p)[input$sp, mat_idx]
        
        # Update slider min/max so that they are a fixed proportion of the
        # parameter value
        updateSliderInput(session, "ks",
                          min = signif(input$ks / 2, 2),
                          max = signif((input$ks + 0.1) * 1.5, 2))
        updateSliderInput(session, "k",
                          min = signif(input$k / 2, 2),
                          max = signif((input$k + 0.1) * 1.5, 2))
        
        if (mu_mat != input$mu_mat) {
            updateSliderInput(session, "mu_mat",
                              min = signif(input$mu_mat / 2, 2),
                              max = signif((input$mu_mat + 0.1) * 1.5, 2))
            # re-calculate ext_mort.
            ext_mort(p)[sp, ] <- ext_mort(p)[sp, ] * (input$mu_mat / mu_mat)
        }

        p@species_params[sp, "alpha"] <- input$alpha
        p@species_params[sp, "ks"]    <- input$ks
        p@species_params[sp, "k"]     <- input$k
        p <- setMetabolicRate(p)
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
                              value = ks, # this will trigger the other observer
                              min = signif(ks / 2, 2),
                              max = signif((ks + 0.1) * 1.5, 2))
            updateSliderInput(session, "p",
                              min = signif(input$p - 0.1, 2),
                              max = signif(input$p + 0.1, 2))
        },
        ignoreInit = TRUE)
}

#' @rdname otherControl
#' @inheritParams abundanceControlUI
otherControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    # determine external mortality at maturity
    mat_idx <- sum(p@w < sp$w_mat)
    mu_mat <- ext_mort(p)[input$sp, mat_idx]
    tagList(
        tags$h3(tags$a(id = "other"), "Other"),
        sliderInput("ks", "Coefficient of standard metabolism 'ks'",
                    value = sp$ks,
                    min = signif(sp$ks / 2, 2),
                    max = signif((sp$ks + 0.1) * 1.5, 2),
                    step = 0.05),
        sliderInput("p", "Exponent of metabolism 'p'",
                     value = sp[["p"]],
                     min = sp[["p"]] - 0.1, max = sp[["p"]] + 0.1, 
                     step = 0.005),
        sliderInput("k", "Coefficient of activity 'k'",
                    value = sp$k,
                    min = signif(sp$k / 2, 2),
                    max = signif((sp$k + 0.1) * 1.5, 2),
                    step = 0.01),
        tags$h3(tags$a(id = "ext_mort"), "Mort"),
        sliderInput("mu_mat", "External mortality at maturity size",
                    value = mu_mat,
                    min = signif(mu_mat / 2, 2),
                    max = signif((mu_mat + 0.1) * 1.5, 2),
                    step = 0.05),
        sliderInput("alpha", "Assimilation efficiency 'alpha'",
                    value = sp$alpha,
                    min = 0,
                    max = 1)
    )
}
