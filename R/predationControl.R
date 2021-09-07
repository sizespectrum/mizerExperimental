#' Controlling the predation parameters in the tuning gadget
#' @inheritParams abundanceControl
predationControl <- function(input, output, session, params, params_old,
                             flags, ...) {
    ## Adjust predation kernel ####
    observeEvent(
        list(input$beta, input$sigma),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_kernel)) {
                flags$sp_old_kernel <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "beta",
                              min = signif(input$beta / 2, 2),
                              max = signif(input$beta * 1.5, 2))
            updateSliderInput(session, "sigma",
                              min = signif(input$sigma / 2, 2),
                              max = signif(input$sigma * 1.5, 2))
            p@species_params[sp, "beta"]  <- input$beta
            p@species_params[sp, "sigma"] <- input$sigma
            p <- setPredKernel(p)
            tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
    
    ## Adjust predation ####
    observeEvent(
        list(input$gamma, input$h, input$q),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_pred)) {
                flags$sp_old_pred <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "gamma",
                              min = signif(input$gamma / 2, 3),
                              max = signif(input$gamma * 1.5, 3))
            updateSliderInput(session, "h",
                              min = signif(input$h / 2, 2),
                              max = signif(input$h * 1.5, 2))
            p@species_params[sp, "gamma"] <- input$gamma
            p@species_params[sp, "h"]     <- input$h
            p@species_params[sp, "q"]     <- input$q
            p <- setSearchVolume(p)
            p <- setMaxIntakeRate(p)
            tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
    
    ## Adjust growth exponent ####
    observeEvent(
        input$n,
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_n)) {
                flags$sp_old_n <- sp
                return()
            }
            # change h so that max intake rate at maturity stays the same
            p@species_params[[sp, "h"]] <- p@species_params[[sp, "h"]] *
                p@species_params[[sp, "w_mat"]] ^
                (p@species_params[[sp, "n"]] - input$n)
            p@species_params[[sp, "n"]] <- input$n
            h <- p@species_params[[sp, "h"]]
            updateSliderInput(session, "h",
                              value = h,
                              min = signif(h / 2, 2),
                              max = signif(h * 1.5, 2))
        },
        ignoreInit = TRUE)
}

#' @rdname predationControl
#' @inheritParams abundanceControlUI
predationControlUI <- function(p, sp) {
    tagList(
        tags$h3(tags$a(id = "predation"), "Predation"),
        sliderInput("gamma", "Predation rate coefficient 'gamma'",
                    value = sp$gamma,
                    min = signif(sp$gamma / 2, 3),
                    max = signif(sp$gamma * 1.5, 3),
                    step = sp$gamma / 50, ticks = FALSE),
        sliderInput("h", "max feeding rate 'h'",
                    value = sp$h,
                    min = signif(sp$h / 2, 2),
                    max = signif(sp$h * 1.5, 2)),
        numericInput("q", "Exponent of search volume 'q'",
                     value = sp$q,
                     min = 0.6, max = 0.8, step = 0.005),
        numericInput("n", "Exponent of max feeding rate 'n'",
                     value = sp$n,
                     min = 0.6, max = 0.8, step = 0.005),
        sliderInput("beta", "Preferred predator-prey mass ratio 'beta'",
                    value = sp$beta,
                    min = signif(sp$beta / 2, 2),
                    max = signif(sp$beta * 1.5, 2)),
        sliderInput("sigma", "Width of size selection function 'sigma'",
                    value = sp$sigma,
                    min = signif(sp$sigma / 2, 2),
                    max = signif(sp$sigma * 1.5, 2),
                    step = 0.05)
    )
}
