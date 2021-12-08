#' tuneParams tab showing size spectra
#' 
#' This tab shows:
#' 
#' * a plot of the biomass density as a function of log size,
#' including all species and the resource. 
#' * A slider for scaling the background (resource and possibly background
#' species).
#' * If there are background species, buttons to adjust or remove those
#' background species.
#' 
#' @inheritParams abundanceControl
#' @param logs Environment holding the log of steady states.
#' @param trigger_update Reactive value used for triggering update of
#'   species parameter sliders.
#' @param ... Unused
spectraTab <- function(input, output, session,
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
    
    ## Scale ####
    observeEvent(input$scale_bkgrd_by, {
        if (input$scale_bkgrd_by == 1) return(1)
        tryCatch({
            p <- scaleDownBackground(params(), input$scale_bkgrd_by)
            params(p)
        }, error = error_fun)
        updateSliderInput(session, "scale_bkgrd_by", value = 1)
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
}

#' @rdname spectraTab
#' 
#' @param help Boolean. If FALSE then the help text is not included on the tab.
#'   This is useful when including this tab as an element of another tab.
spectraTabUI <- function(params, help = TRUE, ...) {
    p <- isolate(params())
    
    tl <- tagList(plotlyOutput("plotSpectra"),
                  div(style = "display:inline-block;vertical-align:middle; width: 300px;",
                      popify(sliderInput("scale_bkgrd_by",
                                         "Scale background down by a factor of:",
                                         value = 1, min = 0.5, max = 2, step = 0.1),
                             title = "Scaling the background",
                             content = "You can scale down the background in which the fish find themselves (the resource and any background species that your model may contain). This allows you to line up your community spectrum with the background spectrum. Simply click on the factor by which to scale. Afterwards you will want to run to steady state. If you rescale by too large a factor the system may have difficulties finding the steady state. If that happens, just hit the Undo button and choose a smaller factor.")),
    )
    
    if (anyNA(p@A)) {
        tl <- tagList(tl, 
                      popify(actionButton("retune_background", "Adj bs"),
                             title = "Adjust background species",
                             content = "Adjust the biomasses of the background species in such a way that the total spectrum aligns well with the resource spectrum. Background species that are no longer needed because forground species have taken their place in the community spectrum are automatically removed."),
                      popify(actionButton("remove_background", "Rem bs"),
                             title = "Remove background species",
                             content = "Remove all background species."))
    }
    if (help) {
        tl <- tagList(tl,
                      h1("Size spectra"),
                      p("This tab shows the biomass size spectra of the individual fish species and of the resource, as well as the total size spectrum (in black)."),
                      p("This plot, as well as those on other tabs, is interactive in various ways. For example you can remove individual species from the plot by clicking on their name in the legend. Hovering over the lines pops up extra information. You can zoom into a portion of the plot by dragging a rectangle with the mouse while holding the left mouse button down."),
                      p("Remember that after any adjustment you make in this app, you need to hit the 'Steady' button before you will see the full multi-species consequences of the change.")
        )
    }
    tl
}
