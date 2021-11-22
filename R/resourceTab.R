#' Serve tab with resource plots
#'
#' @inheritParams biomassTab
resourceTab <- function(input, output, session, params, logs, ...) {

    # Plot resource ----
    output$plot_resource <- renderPlot({
        p <- params()
       mizerMR::plotResource(p)
    })

    # Plot resource predators ----
    output$plot_resource_pred <- renderPlotly({
        p <- params()
        if (input$resource_death_prop == "Proportion")
            mizerMR::plotlyResourcePred(p) else mizerMR::plotlyResourcePred(p, proportion = F)

    })
}

#' @rdname resourceTab
resourceTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_resource_pred"),
        radioButtons("resource_death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
        plotOutput("plot_resource", width = "84%")
    )
}
