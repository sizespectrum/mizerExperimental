#' Serve tab with catch data
#'
#' @inheritParams biomassTab
#' @param catch Data frame holding binned observed catch data. The data can be
#'   binned either into length bins or weight bins. In the former case the data
#'   frame should have columns \code{length} and \code{dl} holding the start of
#'   the size bins in cm and the width of the size bins in cm respectively. In
#'   the latter case the data frame should have columns \code{weight} and
#'   \code{dw} holding the start of the size bins in grams and the width of the
#'   size bins in grams. The data frame also needs to have the columns
#'   \code{species} (the name of the species), \code{gear} (the name of the
#'   gear) and \code{catch} (the number of individuals of a particular species
#'   caught by a particular gear in a size bin).
catchTab <- function(input, output, session, params, logs, trigger_update,
                     catch = NULL, ...) {

    if (!is.null(catch)) {
        assert_that(
            is.data.frame(catch),
            "catch" %in% names(catch),
            "species" %in% names(catch),
            "gear" %in% names(catch),
            all(c("length", "dl") %in% names(catch)) |
                all(c("weight", "dw") %in% names(catch))
        )
    }

    # Tune catchability ----
    # The Catch Tune button calculates the ratio of observed and
    # model yield and then multiplies the catchability by that ratio. It
    # then runs the system to steady state.
    observeEvent(input$tune_catch, {
        p <- isolate(params())
        sp <- isolate(input$sp)
        gear <- isolate(input$gear)
        sp_idx <- which.max(p@species_params$species == sp)
        gp_idx <- which(p@gear_params$species == sp,
                        p@gear_params$gear == gear)
        gp <- p@gear_params[gp_idx, ]
        if ("yield_observed" %in% names(gp) && !is.na(gp$yield_observed) &&
            gp$yield_observed > 0) {
            total <- sum(p@initial_n[sp_idx, ] * p@w * p@dw *
                             getFMortGear(p)[gear, sp, ])
            catchability <-
                gp$catchability *
                p@species_params$yield_observed[sp_idx] / total
            updateSliderInput(session, "catchability",
                              value = catchability)
            # The above update of the slider will also trigger update of
            # the params object and the plot
        }
    })

    # Catch density for selected species ----
    output$plotCatchDist <- renderPlotly({
        plotlyYieldVsSize(params(), species = req(input$sp),
                          gear = input$gear,
                          catch = catch, x_var = input$catch_x)
    })
    
    # Select clicked species ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$yield_click, {
        if (is.null(input$yield_click$x)) return()
        lvls <- input$yield_click$domain$discrete_limits$x
        sp <- lvls[round(input$yield_click$x)]
        if (sp != input$sp) {
            updateSelectInput(session, "sp",
                              selected = sp)
        }
    })
    
    # Total yield by species ----
    output$plotTotalYield <- renderPlot({
        plotYieldVsSpecies(params(), gear = input$gear) +
            theme(text = element_text(size = 18))
    })

    # Input field for observed yield ----
    output$yield_sel <- renderUI({
        p <- isolate(params())
        sp <- input$sp
        gear <- input$gear
        spgear <- paste(sp, gear, sep = ", ")
        div(style = "display:inline-block",
            numericInput("yield_observed",
                         paste0("Observed yield for ", spgear, " [g/year]"),
                         value = p@gear_params[spgear, "yield_observed"]))
    })

    # Adjust observed yield ----
    observeEvent(input$yield_observed, {
        p <- params()
        spgear <- paste(input$sp, input$gear, sep = ", ")
        p@gear_params[spgear, "yield_observed"] <- input$yield_observed
        tuneParams_update_params(p, params)
        },
        ignoreInit = TRUE)

    # Output of model yield ----
    output$yield_total <- renderText({
        p <- params()
        sp <- input$sp
        gear <- input$gear
        spgear <- paste(sp, gear, sep = ", ")
        total <- sum(p@initial_n[sp, ] * p@w * p@dw *
                         getFMortGear(p)[gear, sp, ])
        paste("Model yield for ", spgear, " [g/year]: ", total)
    })
    
    # Calibrate all yields ----
    observeEvent(input$calibrate_yield, {
        # Rescale so that the model matches the total observed yield
        p <- calibrateYield(params())
        tuneParams_add_to_logs(logs, p, params)
        # Trigger an update of sliders
        trigger_update(runif(1))
    })
    
    # Match yield of double-clicked species ----
    observeEvent(input$match_species_yield, {
        if (is.null(input$match_species_yield$x)) return()
        lvls <- input$match_species_yield$domain$discrete_limits$x
        sp <- lvls[round(input$match_species_yield$x)]
        p <- params()
        sp_idx <- which(p@species_params$species == sp)
        
        # Temporarily set observed yield to the clicked yield, then
        # match that yield, then restore observed yield
        obs <- p@gear_params[spgear, "yield_observed"]
        p@gear_params[spgear, "yield_observed"] <- 
            input$match_species_yield$y
        p <- matchYields(p, species = sp)
        p@gear_params[spgear, "yield_observed"] <- obs
        
        tuneParams_update_params(p, params)
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
    
    # Match all yields ----
    observeEvent(input$match_yields, {
        p <- matchYields(params())
        sp_idx <- which(p@species_params$species == input$sp)
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))
        tuneParams_update_params(p, params)
    })
}

#' @rdname catchTab
catchTabUI <- function(...) {
    tagList(
        # actionButton("tune_catch", "Tune catchability"),
        plotOutput("plotTotalYield",
                   click = "yield_click",
                   dblclick = "match_species_yield"),
        popify(uiOutput("yield_sel"),
               title = "Input observed yield",
               content = "Allows you to update the observed yield for this species."),
        textOutput("yield_total"),
        popify(actionButton("calibrate_yield", "Calibrate"),
               title = "Calibrate model",
               content = "Rescales the entire model so that the total of all observed yields agrees with the total of the model yields for the same species."),
        popify(actionButton("match_yields", "Match"),
               title = "Match yields",
               content = "Moves the entire size spectrum for each species up or down to give the observed yield. It does that by multiplying the egg density by the ratio of observed yield to model yield. After that adjustment you should run to steady state by hitting the Steady button, after which the yield will be a bit off again. You can repeat this process if you like to get ever closer to the observed yield."),
        plotlyOutput("plotCatchDist"),
        radioButtons("catch_x", "Show size in:",
                     choices = c("Weight", "Length"),
                     selected = "Length", inline = TRUE),
        h1("Total yield and size distribution of catch"),
        h2("Total yield"),
        p("The upper plot compares the yearly yield for each species in the model to the observed yield, if available."),
        p("The observed yield is taken from the 'yield_observed' column of the species parameter data frame. But if this is missing or needs to be changed you can do this with the input field below the upper plot. Note that this value is in grams/year."),
        h3("How to tune the yield"),
        p("To bring the yield of a species in the model in line with the observed value you can either change the abundance of large fish (for example by reducing their mortality from predation or the", a("background mortality", href = "#other"), "or you can change the", a("fishing parameters", href = "#fishing"), "."),
        h2("Size distribution of catch"),
        p("The lower plot shows the size distribution of the catch and again compares that to the observed size distribution, if available."),
        h3("How to tune size distribution"),
        p("To change the size distribution of catches you either need to change the size spectrum (for example by changing the mortality on large fish) or you need to adjust the ", a("fishing", href = "#fishing"), " selectivity curve by changing the 'L50' and 'L25' parameters.")
    )
}
