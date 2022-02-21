#' Biomass tab for tuning gadget
#'
#' The Biomass tab shows:
#' * A plot of total biomass for each species, compared to
#' observed biomasses when available, using [plotBiomassObservedVsModel()].
#' * Buttons "Calibrate" and "Match" that trigger a call to
#' [calibrateBiomass()] or [matchBiomasses()] respectively.
#'
#' Clicking on a species in the biomass plot makes that species the selected
#' species. Double-clicking on a species selects that species __and__
#' changes its biomass.
#' @inheritParams spectraTab
#' @export
biomassTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    # Select clicked species ----
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


    # Biomass inputs ----
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
                             paste0("Observed biomass in grams for ", sp),
                             value = species_params$biomass_observed)),
            div(style = "display:inline-block",
                numericInput("biomass_cutoff", "Lower cutoff size in grams",
                             value = species_params$biomass_cutoff))
        )
    })

    # Process biomass inputs ----
    observe({
        p <- isolate(params())
        biomass_observed <- req(input$biomass_observed)
        if (biomass_observed == 0) biomass_observed <- NA
        p@species_params[isolate(input$sp), "biomass_observed"] <-
            biomass_observed
        p@species_params[isolate(input$sp), "biomass_cutoff"] <-
            req(input$biomass_cutoff)
        params(p)
    })

    # Calibrate all biomasses ----
    observeEvent(input$calibrate_biomass, {
        # Rescale so that the model matches the total observed biomass
        p <- calibrateBiomass(params())
        params(p)
        tuneParams_add_to_logs(logs, p)
        # Trigger an update of sliders
        trigger_update(runif(1))
    })

    # Match biomass of double-clicked species ----
    observeEvent(input$match_species_biomass, {
        if (is.null(input$match_species_biomass$x)) return()
        lvls <- input$match_species_biomass$domain$discrete_limits$x
        sp <- lvls[round(input$match_species_biomass$x)]
        p <- params()
        sp_idx <- which(p@species_params$species == sp)

        # Temporarily set observed biomass to the clicked biomass, then
        # match that biomass, then restore observed biomass
        obs <- p@species_params$biomass_observed[[sp_idx]]
        p@species_params$biomass_observed[[sp_idx]] <-
            input$match_species_biomass$y
        p <- matchBiomasses(p, species = sp)
        p@species_params$biomass_observed[[sp_idx]] <- obs

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

    # Match all biomasses ----
    observeEvent(input$match_biomasses, {
        p <- matchBiomasses(params())
        sp_idx <- which(p@species_params$species == input$sp)
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))
        params(p)
    })
}

#' @rdname biomassTab
#' @export
biomassTabUI <- function(params, ...) {
    p <- isolate(params())

    tl <- tagList()
    # plot Biomass ----
    tl <- tagList(tl,
                  popify(plotOutput("plotTotalBiomass",
                             click = "biomass_click",
                             dblclick = "match_species_biomass"),
                         placement = "left",
                         title = "Comparison between model and observed biomasses",
                         content = "For each species this plots the observed biomass (square) and the model biomass (circle). You will want to get these into alignment. You can click in the column for a species to select that species. If you double-click in a column the abundance of that species will be scaled to give the observed biomass."),
                  uiOutput("biomass_sel"))

    # calibration buttons ----
    tl <- tagList(tl,
                  popify(actionButton("calibrate_biomass", "Calibrate"),
                         title = "Calibrate model",
                         content = "Rescales the entire model so that the total of all observed biomasses agrees with the total of the model biomasses for the same species."),
                  popify(actionButton("match_biomasses", "Match"),
                         title = "Match biomasses",
                         content = "Moves the entire size spectrum for each species up or down to give the observed biomass value. It does that by multiplying the egg density by the ratio of observed biomass to model biomass. After that adjustment you should run to steady state by hitting the Steady button, after which the biomass will be a bit off again. You can repeat this process if you like to get ever closer to the observed biomass.")
    )
    tl
}
