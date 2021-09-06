#' Biomass tab for tuning gadget
#' 
#' The Biomass tab shows:
#' * A plot of total biomass for each species, compared to
#' observed biomasses when available, using [plotBiomossObservedVsModel()].
#' * Buttons "Calibrate" and "Match" that trigger a call to 
#' [calibrateBiomass()] or [matchBiomasses()] respectively.
#' * A plot of the biomass density spectra as a function of log size,
#' using [plotSpectra()].
#' @param input Reactive holding the inputs
#' @param output Reactive holding the outputs
#' @param session Shiny session
#' @param params Reactive value holding a MizerParams object.
#' @param logs Environment holding the log of steady states.
#' @param trigger_update Reactive value used for triggering update of
#'   species parameter sliders.
#' @param ... Unused
biomassTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    
    ## Plot spectra ####
    output$plotSpectra <- renderPlotly({
        # if (input$binning == "Logarithmic") {
        power <- 2
        # } else {
        #     power <- 1
        # }
        plot <- plotSpectra(params(), power = power, highlight = input$sp, 
                            total = TRUE) +
            theme(text = element_text(size = 12))
        ggplotly(plot, tooltip = c("Species", "w", "value"))
    })
    
    ## Retune background ####
    observeEvent(input$retune_background, {
        p <- adjustBackgroundSpecies(params())
        # For now we won't disable the button because of a bug in shinyBS
        # whereby the tooltip stays forever on disabled buttons.
        # if (!anyNA(p@A)) {
        #     shinyjs::disable("retune_background")
        #     removeTooltip(session, "retune_background")
        # }
        params(p)
    })
    
    ## Remove background ####
    observeEvent(input$remove_background, {
        p <- removeBackgroundSpecies(params())
        # For now we won't disable the button because of a bug in shinyBS
        # whereby the tooltip stays forever on disabled buttons.
        # if (!anyNA(p@A)) {
        #   removeTooltip(session, "remove_background")
        #   shinyjs::disable("remove_background")
        #   shinyjs::disable("retune_background")
        # }
        params(p)
    })
    
    # Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$biomass_click, {
        if (is.null(input$biomass_click$x)) return()
        lvls <- input$biomass_click$domain$discrete_limits$x
        sp <- lvls[round(input$biomass_click$x)]
        if (sp != input$sp) {
            updateSelectInput(session, "sp",
                              selected = sp)
        }
    })
    
    # Plot total biomass ----
    output$plotTotalBiomass <- renderPlot({
        plotBiomassVsSpecies(params()) +
            theme(text = element_text(size = 18))
    })
    
    
    # Biomass selector ----
    output$biomass_sel <- renderUI({
        sp <- input$sp
        p <- isolate(params())
        species_params <- p@species_params[sp, ]
        if (is.null(species_params$biomass_observed) ||
            is.na(species_params$biomass_observed)) {
            species_params$biomass_observed <- 0
        }
        if (is.null(species_params$biomass_cutoff) ||
            is.na(species_params$biomass_cutoff)) {
            species_params$biomass_cutoff <- 0
        }
        list(
            div(style = "display:inline-block",
                numericInput("biomass_observed",
                             paste0("Observed biomass for ", sp),
                             value = species_params$biomass_observed)),
            div(style = "display:inline-block",
                numericInput("biomass_cutoff", "Lower cutoff",
                             value = species_params$biomass_cutoff))
        )
    })
    
    # Adjust biomass observed ----
    observe({
        p <- isolate(params())
        p@species_params[isolate(input$sp), "biomass_observed"] <-
            req(input$biomass_observed)
        p@species_params[isolate(input$sp), "biomass_cutoff"] <-
            req(input$biomass_cutoff)
        params(p)
    })
    
    # Rescale model ----
    observeEvent(input$scale_system, {
        # Rescale so that the model matches the total observed biomass
        p <- calibrateBiomass(params())
        params(p)
        tuneParams_add_to_logs(logs, p)
        # Trigger an update of sliders
        trigger_update(runif(1))
    })
    
    # to make biomass of current species agree with observation
    observeEvent(input$rescale, {
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        if ("biomass_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$biomass_observed[[sp]]) &&
            p@species_params$biomass_observed[[sp]] > 0) {
            cutoff <- p@species_params$biomass_cutoff[[sp]]
            if (is.null(cutoff) || is.na(cutoff)) {
                cutoff <- 0
            }
            biomass_observed <- p@species_params$biomass_observed[[sp]]
            biomass_model <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
            scale_by <- biomass_observed / biomass_model
            p <- scaleModel(p, factor = scale_by)
            params(p)
            tuneParams_add_to_logs(logs, p)
            # Trigger an update of sliders
            trigger_update(runif(1))
        }
    })
    
    # Tune egg density ----
    # The "Tune egg density" button calculates the ratio of observed and
    # model biomass and then multiplies the egg density by that ratio.
    observeEvent(input$tune_egg, {
        if (is.null(input$tune_egg$x)) return()
        lvls <- input$tune_egg$domain$discrete_limits$x
        sp <- lvls[round(input$tune_egg$x)]
        p <- params()
        sp_idx <- which.max(p@species_params$species == sp)
        if ("biomass_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$biomass_observed[[sp_idx]]) &&
            p@species_params$biomass_observed[[sp_idx]] > 0) {
            cutoff <- p@species_params$biomass_cutoff[[sp_idx]]
            if (is.null(cutoff) || is.na(cutoff)) {
                cutoff <- 0
            }
            total <- sum((p@initial_n[sp_idx, ] * p@w * p@dw)[p@w >= cutoff])
            factor <- p@species_params$biomass_observed[[sp_idx]] / total
            p@initial_n[sp_idx, ] <- p@initial_n[sp_idx, ] * factor
        }
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
    observeEvent(input$tune_egg_all, {
        # I just copied and pasted the code form above into a loop.
        # I am sure this could be improved.
        p <- params()
        sp_sel <- which.max(p@species_params$species == input$sp)
        for (sp in seq_along(p@species_params$species)) {
            if ("biomass_observed" %in% names(p@species_params) &&
                !is.na(p@species_params$biomass_observed[[sp]]) &&
                p@species_params$biomass_observed[[sp]] > 0) {
                cutoff <- p@species_params$biomass_cutoff[[sp]]
                if (is.null(cutoff) || is.na(cutoff)) {
                    cutoff <- 0
                }
                total <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
                n0_old <- p@initial_n[sp, p@w_min_idx[[sp]]]
                n0 <- n0_old * p@species_params$biomass_observed[[sp]] / total
                # rescale abundance to new egg density
                p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 / n0_old
                
                if (sp == sp_sel) {
                    updateSliderInput(session, "n0",
                                      value = n0,
                                      min = signif(n0 / 10, 3),
                                      max = signif(n0 * 10, 3))
                }
            }
        }
        params(p)
    })
}

#' @rdname biomassTab
biomassTabUI <- function(params, ...) {
    p <- isolate(params())
    tl <- tagList()
    has_bio <- ("biomass_observed" %in% names(species_params(p))) &&
        !all(is.na(species_params(p)$biomass_observed))
    
    # plot Biomass ----
    tl <- tagList(tl,
                  popify(plotOutput("plotTotalBiomass",
                             click = "biomass_click",
                             dblclick = "tune_egg"),
                         placement = "left",
                         title = "Comparison between model and observed biomasses",
                         content = "For each species this plots the observed biomass (square) and the model biomass (circle). You will want to get these into alignment. You can click in the column for a species to select that species. If you double-click in a column the abundance of that species will be scaled to give the observed biomass."),
                  uiOutput("biomass_sel"))
    
    # calibration buttons ----
    if (has_bio) {
        tl <- tagList(tl,
                      popify(actionButton("scale_system", "Calibrate"),
                             title = "Calibrate model",
                             content = "Rescales the entire model so that the total of all observed biomasses agrees with the total of the model biomasses for the same species."),
                      popify(actionButton("tune_egg_all", "Match"),
                             title = "Match biomasses",
                             content = "Moves the entire size spectrum for each species up or down to give the observed biomass value. It does that by multiplying the egg density by the ratio of observed biomass to model biomass. After that adjustment you should run to steady state by hitting the Steady button, after which the biomass will be a bit off again. You can repeat this process if you like to get ever closer to the observed biomass."))
    }
    
    # background buttons ----
    if (anyNA(p@A)) {
        tl <- tagList(tl, 
                      popify(actionButton("retune_background", "Adj bs"),
                             title = "Adjust background species",
                             content = "Adjust the biomasses of the background species in such a way that the total spectrum aligns well with the resource spectrum. Background species that are no longer needed because forground species have taken their place in the community spectrum are automatically removed."),
                      popify(actionButton("remove_background", "Rem bs"),
                             title = "Remove background species",
                             content = "Remove all background species."))
    }
    
    # plot Spectra ----
    tl <- tagList(tl,
                  plotlyOutput("plotSpectra")
    )
    # Explain spectra ----
    tl <- tagList(tl,
                  h2("Size spectra"),
                  p("The lower plot shows the biomass size spectra of the individual fish species and of the resource, as well as the total size spectrum (in black)."),
                  p("This plot, as well as those on other tabs, is interactive in various ways. For example you can remove individual species from the plot by clicking on their name in the legend. Hovering over the lines pops up extra information. You can zoom into a portion of the plot by dragging a rectangle with the mouse while holding the left mouse button down."),
                  p("Remember that after any adjustment you make in this app, you need to hit the 'Steady' button before you will see the full multi-species consequences of the change.")
    )
}
