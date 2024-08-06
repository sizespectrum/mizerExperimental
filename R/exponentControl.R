#' Controlling the allometric exponents
#' @inheritParams abundanceControl
exponentControl <- function(input, output, session, params, params_old,
                             flags, ...) {
    ## Adjust consumption exponent ####
    observeEvent(
        input$n,
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_n)) {
                flags$sp_old_n <- sp
                return()
            }
            # Change encounter but keep consumption the same
            Q <- getConsumption(p)[sp]
            ext_encounter(p)[sp, ] <- p@w ^ input$n
            Qn <- getConsumption(p)[sp]
            ext_encounter(p)[sp, ] <- ext_encounter(p)[sp, ] * (Q / Qn)
            
            # Set intake_max to keep feeding level of 0.6
            E0 <- p@ext_encounter[sp, 1] / p@w[1]
            h <- 2 / 3 * E0
            
            # change species parameters
            p@species_params[[sp, "h"]] <- h
            p@species_params[[sp, "n"]] <- input$n
            delta_n = input$n - p@species_params[[sp, "n"]]
            p@species_params[[sp, "q"]] <- p@species_params[[sp, "q"]] + delta_n
            
            updateSliderInput(session, "n",
                              min = signif(input$n - 0.1, 2),
                              max = signif(input$n + 0.1, 2))
            p <- setMaxIntakeRate(p)
            tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
}

#' @rdname exponentControl
#' @inheritParams abundanceControlUI
exponentControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    tagList(
        tags$h3(tags$a(id = "exponent"), "Allometric exponents"),
        sliderInput("n", "Exponent of max feeding rate 'n'",
                    value = sp[["n"]],
                    min = sp[["n"]] - 0.1, 
                    max = sp[["n"]] + 0.1, 
                    step = 0.01)
    )
}
