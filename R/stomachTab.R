#' Serve tab with stomach data
#'
#' @inheritParams catchTab
#' @param stomach Data frame holding observations of prey items in predator
#'   stomachs. The required columns are
#'   \itemize{
#'   \item \code{species} holding the name of the predator species,
#'   \item \code{wpredator} with the weight in grams of the predator,
#'   \item \code{wprey} with the weight of the prey item.
#'   }
#'   In case prey items of the same weight have been aggregated in the data
#'   frame then there should be a column \code{Nprey} saying how many prey
#'   items have been aggregated in each row.
stomachTab <- function(input, output, session, params, logs, stomach, ...) {
    if (!is.null(stomach)) {
        assert_that(
            is.data.frame(stomach),
            "wprey" %in% names(stomach),
            "wpredator" %in% names(stomach),
            "species" %in% names(stomach)
        )
        if (!("Nprey" %in% names(stomach))) stomach$Nprey <- 1
        stomach <- stomach %>%
            mutate(logpredprey = log(wpredator / wprey),
                   weight = Nprey / sum(Nprey),
                   weight_biomass = Nprey * wprey / sum(Nprey * wprey),
                   weight_kernel = Nprey / wprey^(1 + alpha - lambda),
                   weight_kernel = weight_kernel / sum(weight_kernel))
    }

    # Plot stomach content ----
    output$plot_stomach <- renderPlot({
        req(input$sp)
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)

        df <- tibble(
            x = x,
            Kernel = double_sigmoid(
                x,
                p_l = input$p_l,
                s_l = input$s_l,
                p_r = input$p_r,
                s_r = input$s_r,
                ex = input$ex)) %>%
            mutate(Numbers = Kernel / exp((1 + alpha - lambda) * x),
                   Biomass = Numbers / exp(x),
                   Kernel = Kernel / sum(Kernel) / dx,
                   Numbers = Numbers / sum(Numbers) / dx,
                   Biomass = Biomass / sum(Biomass) / dx) %>%
            gather(key = Type, value = "Density",
                   Numbers, Biomass)

        pl + geom_line(data = df,
                       aes(x, Density, colour = Type),
                       size = 3)
        st <- stomach %>%
            filter(species == input$sp)
    })
}

stomachTabUI <- function() {
    tagList(
        plotlyOutput("plot_steady")
    )
}