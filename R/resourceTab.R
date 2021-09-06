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
        p <- params()
        species <- factor(p@species_params$species,
                          levels = p@species_params$species)
        select <- (p@cc_pp > 0)
        pred_rate <- p@species_params$interaction_resource *
            getPredRate(p)[, select]
        total <- colSums(pred_rate)
        ylab <- "Death rate [1/year]"
        if (input$resource_death_prop == "Proportion") {
            pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
            ylab = "Proportion of predation"
        }
        # Make data.frame for plot
        plot_dat <- data.frame(
            value = c(pred_rate),
            Predator = species,
            w = rep(p@w_full[select], each = dim(pred_rate)[[1]]))
        ggplot(plot_dat) +
            geom_area(aes(x = w, y = value, fill = Predator)) +
            scale_x_log10("Resource size [g]") +
            ylab(ylab) +
            scale_fill_manual(values = p@linecolour) +
            theme(text = element_text(size = 12))
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