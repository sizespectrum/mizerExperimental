#' Serve tab with resource plots
#'
#' @inheritParams biomassTab
resourceTab <- function(input, output, session, params, logs, ...) {
    
    # Plot resource ----
    output$plot_resource <- renderPlot({
        p <- params()
        select <- (p@cc_pp > 0)
        plot_dat <- data.frame(
            Size = p@w_full[select],
            value = p@initial_n_pp[select] / p@cc_pp[select]
        )
        ggplot(plot_dat) +
            geom_line(aes(Size, value)) +
            scale_x_log10("Resource size [g]") +
            ylab("Proportion of carrying capacity") +
            theme(text = element_text(size = 12))
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