#' Abundance tab for tuning gadget
#' 
#' The Abundance tab shows:
#' * A plot of total abundance for each species, compared to
#' observed abundances when available, using [plotAbundanceObservedVsModel()].
#' * Buttons "Calibrate" and "Match" that trigger a call to 
#' [calibrateAbundance()] or [matchAbundances()] respectively.
#' 
#' Clicking on a species in the abundance plot makes that species the selected
#' species. Double-clicking on a species selects that species __and__
#' changes its abundance.
#' @inheritParams spectraTab
abundanceTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    # Select clicked species ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$abundance_click, {
        if (is.null(input$abundance_click$x)) return()
        lvls <- input$abundance_click$domain$discrete_limits$x
        sp <- lvls[round(input$abundance_click$x)]
        if (sp != input$sp) {
            updateSelectInput(session, "sp",
                              selected = sp)
        }
    })
    
    # Plot total abundance ----
    output$plotTotalAbundance <- renderPlot({
        plotAbundanceVsSpecies(params()) +
            theme(text = element_text(size = 18))
    })
    
    
    # Abundance inputs ----
    output$abundance_sel <- renderUI({
        sp <- input$sp
        p <- isolate(params())
        species_params <- p@species_params[sp, ]
        if (is.null(species_params$abundance_observed) ||
            is.na(species_params$abundance_observed)) {
            species_params$abundance_observed <- 0
        }
        if (is.null(species_params$abundance_cutoff) ||
            is.na(species_params$abundance_cutoff)) {
            species_params$abundance_cutoff <- 0
        }
        list(
            div(style = "display:inline-block",
                numericInput("abundance_observed",
                             paste0("Observed abundance in grams for ", sp),
                             value = species_params$abundance_observed)),
            div(style = "display:inline-block",
                numericInput("abundance_cutoff", "Lower cutoff size in grams",
                             value = species_params$abundance_cutoff))
        )
    })
    
    # Process abundance inputs ----
    observe({
        p <- isolate(params())
        abundance_observed <- req(input$abundance_observed)
        if (abundance_observed == 0) abundance_observed <- NA
        p@species_params[isolate(input$sp), "abundance_observed"] <-
            abundance_observed
        p@species_params[isolate(input$sp), "abundance_cutoff"] <-
            req(input$abundance_cutoff)
        params(p)
    })
    
    # Calibrate all abundances ----
    observeEvent(input$calibrate_abundance, {
        # Rescale so that the model matches the total observed abundance
        p <- calibrateAbundance(params())
        params(p)
        tuneParams_add_to_logs(logs, p)
        # Trigger an update of sliders
        trigger_update(runif(1))
    })
    
    # Match abundance of double-clicked species ----
    observeEvent(input$match_species_abundance, {
        if (is.null(input$match_species_abundance$x)) return()
        lvls <- input$match_species_abundance$domain$discrete_limits$x
        sp <- lvls[round(input$match_species_abundance$x)]
        p <- params()
        sp_idx <- which(p@species_params$species == sp)
        
        # Temporarily set observed abundance to the clicked abundance, then
        # match that abundance, then restore observed abundance
        obs <- p@species_params$abundance_observed[[sp_idx]]
        p@species_params$abundance_observed[[sp_idx]] <- 
            input$match_species_abundance$y
        p <- matchAbundances(p, species = sp)
        p@species_params$abundance_observed[[sp_idx]] <- obs
        
        params(p)
        if (sp == input$sp) {
            n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
            updateSliderInput(session, "n0",
                              value = n0,
                              min = signif(n0 / 10, 3),
                              max = signif(n0 * 10, 3))
        } else {
            updateSelectInput(session, "sp", selected = sp)
        }
    })
    
    # Match all abundances ----
    observeEvent(input$match_abundances, {
        p <- matchAbundances(params())
        sp_idx <- which(p@species_params$species == input$sp)
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))
        params(p)
    })
}

#' @rdname abundanceTab
abundanceTabUI <- function(params, ...) {
    p <- isolate(params())
    
    tl <- tagList()
    # plot Abundance ----
    tl <- tagList(tl,
                  popify(plotOutput("plotTotalAbundance",
                             click = "abundance_click",
                             dblclick = "match_species_abundance"),
                         placement = "left",
                         title = "Comparison between model and observed abundances",
                         content = "For each species this plots the observed abundance (square) and the model abundance (circle). You will want to get these into alignment. You can click in the column for a species to select that species. If you double-click in a column the abundance of that species will be scaled to give the observed abundance."),
                  uiOutput("abundance_sel"))
    
    # calibration buttons ----
    tl <- tagList(tl,
                  popify(actionButton("calibrate_abundance", "Calibrate"),
                         title = "Calibrate model",
                         content = "Rescales the entire model so that the total of all observed abundances agrees with the total of the model abundances for the same species."),
                  popify(actionButton("match_abundances", "Match"),
                         title = "Match abundances",
                         content = "Moves the entire size spectrum for each species up or down to give the observed abundance value. It does that by multiplying the egg density by the ratio of observed abundance to model abundance. After that adjustment you should run to steady state by hitting the Steady button, after which the abundance will be a bit off again. You can repeat this process if you like to get ever closer to the observed abundance.")
    )
    tl
}
