#' Serve tab with death plots
#'
#' @inheritParams biomassTab
deathTab <- function(input, output, session, params, logs,
                     diet = NULL, ...) {
    
    # Plot predators ----
    output$plot_pred <- renderPlotly({
        req(input$sp)
        p <- params()
        if (!is.null(diet)) {
            p <- matchEcopathDiet(p, diet)
        }
        plotDeath(p, species = input$sp, 
                  proportion = input$death_prop == "Proportion",
                  xtrans = input$death_xtrans)
    })
}

#' @rdname deathTab
deathTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_pred"),
        radioButtons("death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
        radioButtons("death_xtrans", "x-axis scale:",
                     choices = c("log10", "identity"),
                     selected = "log10", inline = TRUE)
    )
}