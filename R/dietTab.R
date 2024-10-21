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
    
    # Plot prey ----
    output$plot_prey <- renderPlotly({
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        x <- log(p@w_full)
        dx <- x[2] - x[1]
        wp <- 10^req(input$pred_size)
        wp_idx <- sum(p@w <= wp)
        # Calculate total community abundance
        # Todo: take interaction matrix into account
        fish_idx <- (length(p@w_full) - length(p@w) + 1):length(p@w_full)
        total_n <- p@initial_n_pp
        total_n[fish_idx] <- total_n[fish_idx] +
            p@interaction[sp, ] %*% p@initial_n
        phix <- getPredKernel(p)[sp, wp_idx, ]
        n <- total_n * phix
        n_logw <- n * p@w_full
        b_logw <- n_logw * p@w_full
        # convert to proportions
        phix <- phix / sum(phix * dx)
        n <- n / sum(n * dx)
        n_logw <- n_logw / sum(n_logw * dx)
        b_logw <- b_logw / sum(b_logw * dx)
        df <- tibble::tibble(w = p@w_full,
                             `Feeding kernel` = phix,
                             `Number density` = n,
                             `Number density in log w` = n_logw,
                             `Biomass density in log w` = b_logw) %>%
            tidyr::gather(Type, value, `Feeding kernel`, 
                          `Number density`, `Number density in log w`, 
                          `Biomass density in log w`) %>%
            mutate(Type = factor(Type))
        ggplot(df) +
            geom_line(aes(w, value, color = Type)) +
            labs(x = "Weight [g]", y = "Density") +
            annotate("point", x = wp, y = 0, size = 4, colour = "blue") +
            scale_x_log10()
    })
    
    # Prey size slider ----
    output$pred_size_slider <- renderUI({
        p <- isolate(params())
        sp <- which.max(p@species_params$species == input$sp)
        sliderInput("pred_size", "log_10 predator size",
                    value = signif(log10(p@species_params$w_mat[sp]), 2),
                    min = signif(log10(p@species_params$w_min[sp]), 2),
                    max = signif(log10(p@species_params$w_max[sp]), 2),
                    step = 0.2,
                    width = "80%",
                    animate = animationOptions(loop = TRUE))
    })
}

#' @rdname dietTab
dietTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_diet"),
        radioButtons("xtrans", "x-axis scale:",
                     choices = c("log10", "identity"),
                     selected = "log10", inline = TRUE),
        plotlyOutput("plot_prey"),
        uiOutput("pred_size_slider")
    )
}