#' Number tab for tuning gadget
#'
#' The Number tab shows:
#' * A plot of total number for each species, compared to
#' observed numbers when available, using [plotNumberVsSpecies()].
#' * Buttons "Calibrate" and "Match" that trigger a call to
#' [calibrateNumber()] or [matchNumbers()] respectively.
#'
#' Clicking on a species in the number plot makes that species the selected
#' species. Double-clicking on a species selects that species __and__
#' changes its number.
#' @inheritParams spectraTab
#' @param params_old Reactive value holding non-updated MizerParams object
numberTab <- function(input, output, session,
                       params, params_old, logs, trigger_update, ...) {
    # Select clicked species ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$number_click, {
        if (is.null(input$number_click$x)) return()
        lvls <- input$number_click$domain$discrete_limits$x
        sp <- lvls[round(input$number_click$x)]
        if (sp != input$sp) {
            updateSelectInput(session, "sp",
                              selected = sp)
        }
    })

    # Plot total number ----
    output$plotTotalNumber <- renderPlot({
        plotNumberVsSpecies(params()) +
            theme(text = element_text(size = 18))
    })


    # Number inputs ----
    output$number_sel <- renderUI({
        sp <- input$sp
        p <- isolate(params())
        species_params <- p@species_params[sp, ]
        if (is.null(species_params$number_observed) ||
            is.na(species_params$number_observed)) {
            species_params$number_observed <- 0
        }
        if (is.null(species_params$number_cutoff) ||
            is.na(species_params$number_cutoff)) {
            species_params$number_cutoff <- 0
        }
        list(
            div(style = "display:inline-block",
                numericInput("number_observed",
                             paste0("Observed number in grams for ", sp),
                             value = species_params$number_observed)),
            div(style = "display:inline-block",
                numericInput("number_cutoff", "Lower cutoff size in grams",
                             value = species_params$number_cutoff))
        )
    })

    # Process number inputs ----
    observe({
        p <- isolate(params())
        number_observed <- req(input$number_observed)
        if (number_observed == 0) number_observed <- NA
        p@species_params[isolate(input$sp), "number_observed"] <-
            number_observed
        p@species_params[isolate(input$sp), "number_cutoff"] <-
            req(input$number_cutoff)
        tuneParams_update_params(p, params)
    })

    # Calibrate all numbers ----
    observeEvent(input$calibrate_number, {
        # Rescale so that the model matches the total observed number
        p <- calibrateNumber(params())
        params_old(p)
        tuneParams_add_to_logs(logs, p, params)
        # Trigger an update of sliders
        trigger_update(runif(1))
    })

    # Match number of double-clicked species ----
    observeEvent(input$match_species_number, {
        if (is.null(input$match_species_number$x)) return()
        lvls <- input$match_species_number$domain$discrete_limits$x
        sp <- lvls[round(input$match_species_number$x)]
        p <- params()
        sp_idx <- which(p@species_params$species == sp)

        # Temporarily set observed number to the clicked number, then
        # match that number, then restore observed number
        obs <- p@species_params$number_observed[[sp_idx]]
        p@species_params$number_observed[[sp_idx]] <-
            input$match_species_number$y
        p <- matchNumbers(p, species = sp)
        p@species_params$number_observed[[sp_idx]] <- obs

        tuneParams_update_abundance(p, sp, params, params_old)
        
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

    # Match all numbers ----
    observeEvent(input$match_numbers, {
        p <- matchNumbers(params())
        sp_idx <- which(p@species_params$species == input$sp)
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))
        tuneParams_update_abundance(p, input$sp, params, params_old)
    })
}

#' @rdname numberTab
numberTabUI <- function(params, ...) {
    p <- isolate(params())

    tl <- tagList()
    # plot Number ----
    tl <- tagList(tl,
                  popify(plotOutput("plotTotalNumber",
                             click = "number_click",
                             dblclick = "match_species_number"),
                         placement = "left",
                         title = "Comparison between model and observed numbers",
                         content = "For each species this plots the observed number (square) and the model number (circle). You will want to get these into alignment. You can click in the column for a species to select that species. If you double-click in a column the number of that species will be scaled to give the observed number."),
                  uiOutput("number_sel"))

    # calibration buttons ----
    tl <- tagList(tl,
                  popify(actionButton("calibrate_number", "Calibrate"),
                         title = "Calibrate model",
                         content = "Rescales the entire model so that the total of all observed numbers agrees with the total of the model numbers for the same species."),
                  popify(actionButton("match_numbers", "Match"),
                         title = "Match numbers",
                         content = "Moves the entire size spectrum for each species up or down to give the observed number value. It does that by multiplying the egg density by the ratio of observed number to model number. After that adjustment you should run to steady state by hitting the Steady button, after which the number will be a bit off again. You can repeat this process if you like to get ever closer to the observed number.")
    )
    tl
}
