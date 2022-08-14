#' Serve tab with resource plots
#'
#' @inheritParams biomassTab
#' @export
resourceTab <- function(input, output, session, params, logs, ...) {

    # Plot resource ----
    output$plot_resource <- renderPlot({
        mizerMR::plotResource(params()) + 
            theme(text = element_text(size = 16))
    })

    # Plot resource predators ----
    output$plot_resource_pred <- renderPlotly({
        proportion <- (input$resource_death_prop == "Proportion")
        mizerMR::plotlyResourcePred(params(), proportion = proportion)
    })
}

#' @rdname resourceTab
#' @export
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
