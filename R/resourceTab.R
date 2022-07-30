#' Serve tab with resource plots
#'
#' @inheritParams biomassTab
resourceTab <- function(input, output, session, params, logs, ...) {
    
    # Plot resource ----
    output$plot_resource <- renderPlot({
        plotResourceLevel(params()) + theme(text = element_text(size = 16))
    })
    
    # Plot resource predators ----
    output$plot_resource_pred <- renderPlotly({
        proportion <- (input$resource_death_prop == "Proportion")
        plotlyResourcePred(params(), proportion = proportion)
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