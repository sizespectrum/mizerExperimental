#' Serve tab with death plots
#'
#' @inheritParams biomassTab
deathTab <- function(input, output, session, params, logs, ...) {
    
    # Plot predators ----
    output$plot_pred <- renderPlotly({
        req(input$sp)
        plotDeath(params(), species = input$sp, 
                  proportion = input$death_prop == "Proportion")
    })
}

#' @rdname deathTab
deathTabUI <- function(...) {
    tagList(
        radioButtons("death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
        plotlyOutput("plot_pred")
    )
}