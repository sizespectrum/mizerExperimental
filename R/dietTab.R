#' Serve tab with diet plot
#'
#' @inheritParams biomassTab
dietTab <- function(input, output, session, params, logs, 
                    diet = NULL, ...) {
    
    # Plot diet ----
    output$plot_diet <- renderPlotly({
        req(input$sp)
        p <- params()
        if (!is.null(diet)) {
            p <- matchEcopathDiet(p, diet)
        }
        plotDietX(p, input$sp, xtrans = input$xtrans)
    })

}

#' @rdname dietTab
dietTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_diet"),
        radioButtons("xtrans", "x-axis scale:",
                     choices = c("log10", "identity"),
                     selected = "log10", inline = TRUE)
    )
}