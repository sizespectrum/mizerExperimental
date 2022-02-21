#' Serve tab with diet plot
#'
#' @inheritParams biomassTab
#' @export
dietTab <- function(input, output, session, params, logs, ...) {

    # Plot diet ----
    output$plot_diet <- renderPlotly({
        req(input$sp)
        plot <- mizerMR::plotDiet(params(), species = input$sp) +
            theme(text = element_text(size = 12))
        ggplotly(plot, tooltip = c("size", "Proportion", "Prey"))
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
        totalx <- total_n * p@w_full
        #totalx <- totalx / sum(totalx * dx)
        phix <- getPredKernel(p)[sp, wp_idx, ]
        pr <- totalx * phix
        br <- pr * p@w_full
        # convert to proportions
        phix <- phix / sum(phix * dx)
        pr <- pr / sum(pr * dx)
        br <- br / sum(br * dx)
        df <- tibble::tibble(w = p@w_full,
                             Kernel = phix,
                             Biomass = br,
                             Numbers = pr) %>%
            tidyr::gather(Type, value, Kernel, Biomass, Numbers)
        ggplot(df) +
            geom_line(aes(w, value, color = Type)) +
            labs(x = "Weight [g]", y = "Density") +
            geom_point(aes(x = wp, y = 0), size = 4, colour = "blue") +
            scale_x_log10()
    })

    # Prey size slider ----
    output$pred_size_slider <- renderUI({
        p <- isolate(params())
        sp <- which.max(p@species_params$species == input$sp)
        sliderInput("pred_size", "log_10 predator size",
                    value = signif(log10(p@species_params$w_mat[sp]), 2),
                    min = signif(log10(p@species_params$w_min[sp]), 2),
                    max = signif(log10(p@species_params$w_inf[sp]), 2),
                    step = 0.2,
                    width = "80%",
                    animate = animationOptions(loop = TRUE))
    })
}

#' @rdname dietTab
#' @export
dietTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_diet"),
        plotlyOutput("plot_prey"),
        uiOutput("pred_size_slider")
    )
}
