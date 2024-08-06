#' Controlling the allometric exponents
#' @inheritParams abundanceControl
exponentControl <- function(input, output, session, params, params_old,
                             flags, ...) {
    ## Adjust consumption exponent ####
    observeEvent(
        list(input$n, input$p, input$d),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_n)) {
                flags$sp_old_n <- sp
                return()
            }
            
            delta_n = input$n - p@species_params[[sp, "n"]]
            if (delta_n != 0) {
                # Change encounter but keep consumption the same
                Q <- getConsumption(p)[sp]
                ext_encounter(p)[sp, ] <- p@w ^ input$n
                Qn <- getConsumption(p)[sp]
                ext_encounter(p)[sp, ] <- ext_encounter(p)[sp, ] * (Q / Qn)
                
                # change species parameters
                p@species_params[[sp, "n"]] <- input$n
                p@species_params[[sp, "q"]] <- p@species_params[[sp, "q"]] + delta_n
                
                updateSliderInput(session, "n",
                                  min = signif(input$n - 0.02, 3),
                                  max = signif(input$n + 0.02, 3))
            }
            
            delta_p = input$p - p@species_params[[sp, "p"]]
            if (delta_p != 0) {
                # Change metabolism but keep respiration the same
                R <- getRespiration(p)[sp]
                metab(p)[sp, ] <- p@w ^ input$p
                Rn <- getRespiration(p)[sp]
                metab(p)[sp, ] <- metab(p)[sp, ] * (R / Rn)
                
                # change species parameters
                p@species_params[[sp, "p"]] <- input$p
                
                updateSliderInput(session, "p",
                                  min = signif(input$p - 0.02, 3),
                                  max = signif(input$p + 0.02, 3))
            }
            
            delta_d = input$d - p@species_params[[sp, "d"]]
            if (delta_d != 0) {
                # Change metabolism but keep respiration the same
                M <- getM0B(p)[sp]
                ext_mort(p)[sp, ] <- p@w ^ input$d
                Mn <- getM0B(p)[sp]
                ext_mort(p)[sp, ] <- ext_mort(p)[sp, ] * (M / Mn)
                
                # change species parameters
                p@species_params[[sp, "d"]] <- input$d
                
                updateSliderInput(session, "d",
                                  min = signif(input$d - 0.02, 3),
                                  max = signif(input$d + 0.02, 3))
            }
            
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
        sliderInput("n", "Exponent of consumption",
                    value = sp[["n"]],
                    min = sp[["n"]] - 0.02, 
                    max = sp[["n"]] + 0.02, 
                    step = 0.005),
        sliderInput("p", "Exponent of respiration",
                    value = sp[["p"]],
                    min = sp[["p"]] - 0.02, 
                    max = sp[["p"]] + 0.02, 
                    step = 0.005),
        sliderInput("d", "Exponent of mortality",
                    value = sp[["d"]],
                    min = sp[["d"]] - 0.02, 
                    max = sp[["d"]] + 0.02, 
                    step = 0.005)
    )
}
