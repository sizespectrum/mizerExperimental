#' Serve tab with death plots
#'
#' @inheritParams biomassTab
deathTab <- function(input, output, session, params, logs, ...) {
    
    # Plot predators ----
    output$plot_pred <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()
        species <- factor(p@species_params$species,
                          levels = p@species_params$species)
        fish_idx_full <- (p@w_full >= p@species_params[sp, "w_min"]) &
            (p@w_full <= p@species_params[sp, "w_inf"])
        fish_idx <- (p@w >= p@species_params[sp, "w_min"]) &
            (p@w <= p@species_params[sp, "w_inf"])
        pred_rate <- p@interaction[, sp] *
            getPredRate(p)[, fish_idx_full]
        fishing <- getFMort(p)[sp, fish_idx]
        total <- colSums(pred_rate) + p@mu_b[sp, fish_idx] + fishing
        ylab <- "Death rate [1/year]"
        background <- p@mu_b[sp, fish_idx]
        if (input$death_prop == "Proportion") {
            pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
            background <- background / total
            fishing <- fishing / total
            ylab <- "Proportion of all death"
        }
        # Make data.frame for plot
        plot_dat <-
            rbind(
                data.frame(value = background,
                           Cause = "Background",
                           w = p@w[fish_idx]),
                data.frame(value = fishing,
                           Cause = "Fishing",
                           w = p@w[fish_idx]),
                data.frame(value = c(pred_rate),
                           Cause = species,
                           w = rep(p@w[fish_idx], each = dim(pred_rate)[[1]]))
            )
        ggplot(plot_dat) +
            geom_area(aes(x = w, y = value, fill = Cause)) +
            scale_x_log10() +
            labs(x = "Size [g]", y = ylab) +
            scale_fill_manual(values = p@linecolour)
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