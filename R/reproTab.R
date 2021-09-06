#' Serve tab with plot of reproductive success rates
#'
#' @inheritParams biomassTab
reproTab <- function(input, output, session, params, logs, ...) {
    # Help button ----
    help_steps <- data.frame(
        element = c(NA),
        intro = c("This still needs to be written.")
    )
    observeEvent(
        input$repro_help,
        introjs(session, options = list(
            steps = help_steps)
        )
    )
    # erepro plot ----
    output$plot_erepro <- renderPlotly({
        p <- params()
        foreground <- !is.na(p@A)
        df <- data.frame(Species = factor(p@species_params$species[foreground],
                                          levels = p@species_params$species[foreground]),
                         value = p@species_params$erepro[foreground])
        ggplot(df, aes(x = Species, y = value)) +
            geom_col() + geom_hline(yintercept = 1, color = "red") +
            scale_y_log10(name = "Reproductive success") +
            theme(text = element_text(size = 12)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })
    
    # Plot psi ----
    output$plot_psi <- renderPlotly({
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        w_min <- p@species_params$w_inf[sp] / 50
        sel <- p@w >= w_min & p@w <= p@species_params$w_inf[sp]
        df <- data.frame(Size = p@w[sel], value = p@psi[sp, sel])
        ggplot(df, aes(x = Size, y = value)) +
            geom_line(color = "blue") +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dashed") +
            geom_text(aes(x = p@species_params[sp, "w_mat"],
                          y = max(value * 0.8),
                          label = "\nw_mat"),
                      angle = 90) +
            geom_vline(xintercept = p@species_params[sp, "w_mat25"],
                       linetype = "dotted") +
            geom_text(aes(x = p@species_params[sp, "w_mat25"],
                          y = max(value * 0.6),
                          label = "\nw_mat25"),
                      angle = 90) +
            theme(text = element_text(size = 12)) +
            labs(x = "Size [g]", y = "Proportion of energy for reproduction")
    })
}

#' @rdname reproTab
reproTabUI <- function(...) {
    tagList(
        actionButton("repro_help", "Press for instructions"),
        plotlyOutput("plot_erepro"),
        plotlyOutput("plot_psi")
    )
}
