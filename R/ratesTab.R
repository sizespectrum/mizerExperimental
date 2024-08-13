#' Serve tab with rates plots
#'
#' @inheritParams biomassTab
ratesTab <- function(input, output, session, params, logs, ...) {
    # Plot growth rates ----
    output$plotGrowth <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()
        
        max_w <- p@species_params[sp, "w_max"]
        if (input$axis == "Logarithmic") {
            min_w <- p@species_params[sp, "w_min"]
        } else {
            min_w = p@species_params[sp, "w_mat"] / 10 # min(1, p@species_params[sp, "w_min"])
        }
        sel <- p@w >= min_w & p@w <= max_w
        len <- sum(sel)
        growth <- getEGrowth(p)[sp, sel]
        growth_and_repro <- getEReproAndGrowth(p)[sp, sel]
        metab <- p@metab[sp, sel]
        income <- growth_and_repro + metab
        repro <- growth_and_repro - growth
        df <- data.frame(
            w = rep(p@w[sel], 4),
            Type = c(rep("Growth", len),
                     rep("Income", len),
                     rep("Metabolic loss", len),
                     rep("Reproduction", len)),
            value = c(growth, income, metab, repro)
        )
        y_annotation <- max(df$value) * 0.2
        pl <- ggplot(df, aes(x = w, y = value, color = Type)) +
            geom_line() +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dotted") +
            geom_vline(xintercept = p@species_params[sp, "w_max"],
                       linetype = "dotted") +
            theme(text = element_text(size = 12)) +
            labs(x = "Size [g]", y = "Rate [g/year]")  +
            annotate("text",
                     x = p@species_params[sp, "w_mat"],
                     y = y_annotation,
                     label = "\nMaturity",
                     angle = 90)  +
            annotate("text",
                     x = p@species_params[sp, "w_max"],
                     y = y_annotation,
                     label = "\nMaximum",
                     angle = 90)
        if (input$axis == "Logarithmic") {
            pl <- pl + scale_x_log10()
        }
        pl
    })
    
    # Plot death rates ----
    output$plotDeath <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()
        
        max_w <- p@species_params[sp, "w_max"]
        if (input$axis == "Logarithmic") {
            min_w <- p@species_params[sp, "w_min"]
        } else {
            min_w = p@species_params[sp, "w_mat"] / 10# min(1, p@species_params[sp, "w_min"])
        }
        sel <- p@w >= min_w & p@w <= max_w
        len <- sum(sel)
        df <- data.frame(
            w = rep(p@w[sel], 4),
            Type = c(rep("Total", len),
                     rep("Predation", len),
                     rep("Fishing", len),
                     rep("Background", len)),
            value = c(getMort(p)[sp, sel],
                      getPredMort(p)[sp, sel],
                      getFMort(p)[sp, sel],
                      p@mu_b[sp, sel])
        )
        y_annotation <- max(df$value) * 0.2
        pl <- ggplot(df, aes(x = w, y = value, color = Type)) +
            geom_line() +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dotted") +
            geom_vline(xintercept = p@species_params[sp, "w_max"],
                       linetype = "dotted") +
            theme(text = element_text(size = 12)) +
            labs(x = "Size [g]", y = "Rate [1/year]")  +
            annotate("text",
                     x = p@species_params[sp, "w_mat"],
                     y = y_annotation,
                     label = "\nMaturity",
                     angle = 90)  +
            annotate("text",
                     x = p@species_params[sp, "w_max"],
                     y = y_annotation,
                     label = "\nMaximum",
                     angle = 90)
        if (input$axis == "Logarithmic") {
            pl <- pl + scale_x_log10()
        }
        pl
    })
}

#' @rdname ratesTab
ratesTabUI <- function(...) {
    tagList(
        radioButtons("axis", "x-axis scale:",
                     choices = c("Logarithmic", "Normal"),
                     selected = "Logarithmic", inline = TRUE),
        plotlyOutput("plotGrowth"),
        plotlyOutput("plotDeath")
    )
}