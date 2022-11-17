#' Growth tab for tuning gadget
#' inheritParams biomassTab
growthTab <- function(input, output, session, params, logs,
                      size_at_age = NULL, ...) {
    # Help button ----
    help_steps <- data.frame(
        element = c(NA),
        intro = c("This still needs to be written.")
    )
    observeEvent(
        input$growth_help,
        introjs(session, options = list(
            steps = help_steps)
        )
    )
    
    # Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$growth_click, {
        if (!is.null(input$growth_click$panelvar1) &&
            input$growth_click$panelvar1 != input$sp) {
            updateSelectInput(session, "sp",
                              selected = input$growth_click$panelvar1)
        }
    })
    # Double Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$growth_dblclick, {
        if (!is.null(input$growth_dblclick$panelvar1) &&
            input$growth_dblclick$panelvar1 != input$sp) {
            updateSelectInput(session, "sp",
                              selected = input$growth_dblclick$panelvar1)
        }
        # Toggle between "All" and "Selected species"
        if (input$all_growth == "All") {
            updateRadioButtons(session, "all_growth",
                               selected = "Selected species")
        } else {
            updateRadioButtons(session, "all_growth",
                               selected = "All")
        }
    })
    
    # von Bertalanffy parameters ----
    output$k_vb_sel <- renderUI({
        req(input$sp)
        if (!input$all_growth == "All") {
            k_vb <- params()@species_params[input$sp, "k_vb"]
            t0 <- params()@species_params[input$sp, "t0"]
            a <- params()@species_params[input$sp, "a"]
            b <- params()@species_params[input$sp, "b"]
            list(
                div(style = "display:inline-block",
                    numericInput("k_vb", "Von Bertalanffy k", value = k_vb, width = "9em")),
                div(style = "display:inline-block",
                    numericInput("t0", "t_0", value = t0, width = "6em")),
                p("Parameters for length-weight relationship w = a l^b"),
                div(style = "display:inline-block",
                    numericInput("a", "a", value = a, width = "8em")),
                div(style = "display:inline-block",
                    numericInput("b", "b", value = b, width = "8em"))
            )
        }
    })
    
    ## Adjust von Bertalanffy ####
    observeEvent(
        list(input$k_vb, input$t0, input$a, input$b),
        {
            p <- isolate(params())
            p@species_params[isolate(input$sp), "k_vb"] <- input$k_vb
            p@species_params[isolate(input$sp), "t0"] <- input$t0
            p@species_params[isolate(input$sp), "a"] <- input$a
            p@species_params[isolate(input$sp), "b"] <- input$b
            params(p)
        },
        ignoreInit = TRUE)
    
    # Plot growth curves ----
    output$plotGrowthCurve <- renderPlot({
        p <- params()
        if (input$all_growth == "All") {
            plotGrowthCurves(p, species_panel = TRUE) +
                theme(text = element_text(size = 16))
        } else {
            plotGrowthCurves(p, species = input$sp, size_at_age = size_at_age) +
                theme(text = element_text(size = 16))
        }
    })
    
    # Plot feeding level ----
    output$plot_feeding_level <- renderPlotly({
        if (input$all_growth == "All") {
            plot <- plotFeedingLevel(params(), highlight = input$sp,
                                     include_critical = TRUE) +
                theme(text = element_text(size = 12))
        } else {
            plot <- plotFeedingLevel(params(), species = input$sp,
                                     include_critical = TRUE) +
                theme(text = element_text(size = 12))
        }
        ggplotly(plot, tooltip = c("Species", "w", "value"))
    })
}

#' @rdname growthTab
#' @inheritParams biomassTabUI
growthTabUI <- function(...) {
    tagList(
        popify(div(radioButtons("all_growth", "Show:",
                            choices = c("All", "Selected species"),
                            selected = "All", inline = TRUE),
                   style = "width: 200px; margin: auto;"),
               title = "Switch views",
               content = "Select whether to view growth curves for all species or just for the selected species. You can also toggle this by double-clicking on the plot. Single-clicking on the plot changes the selected species without changing the view."),
        plotOutput("plotGrowthCurve",
                   click = "growth_click",
                   dblclick = "growth_dblclick"),
        plotlyOutput("plot_feeding_level"),
        popify(uiOutput("k_vb_sel"),
               title = "Von Bertalanffy growth parameters",
               content = "Here you can update these parameters describing observed growth curves."),
        h2("Growth curves"),
        p("The upper plot shows growth curves: the size of an individual plotted against its age. Each growth curve plot shows the mizer growth curve in red and the von Bertalanffy growth curve in blue. The 'Show:' radio buttons let you choose to view the growth curves for all species at once or only that for the selected species. When viewing all growth curves you can click on one of the graphs to select that species. Double-clicking toggles between viewing all species and individual species."),
        p("The von Bertalanffy growth curve is determined by the von Bertalanffy parameters 'w_inf', 'k_vb' and 't0' as well as the length-weight relationship parameters 'a' and 'b' in the species parameter data frame. When viewing a single species, you can change these parameters using the input fields below the plots. The mizer growth curves in contrast are determined by the energy available for growth from feeding, after metabolic cost and investment into reproduction are taken into account."),
        h2("Feeding level"),
        p("In the lower plot the thick lines shows the feeding level, which reflects the degree of satiation. At feeding level 1 an individual is totally satiated and does not feed any more, so is insensitive to changes in prey availability. The faint lines show the critical feeding level, which is the feeding level below which an individual can no longer meet its metabolic cost and starves. The actual feeding level should lie in between these two extremes."),
        h3("Parameters affecting growth curves and feeding level"),
        p("The growth rate is strongly influenced by the predation rate coefficient 'gamma' and the maximum feeding rate parameter 'h'. Increasing 'gamma' will increase growth and increase feeding level. Increasing 'h' will also increase growth but decrease feeding level. So in practice you will change both to obtain the desired growth while maintaining an appropriate feeding level."),
        p("The growth rate is also influenced by losses to basic metabolism (parameters 'ks' and 'p') and activity (parameter 'k') and investment into", a("reproduction", href = "#reproduction"), ". For example, changing the exponent 'm' that determines how investment into reproduction scales with size will affect the growth rate of large individuals.")
    )
}