#' @export
simTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_sim")
    )
}

#' @export
simTab <- function(input, output, session, params, logs, ...) {

    ## Plot run to steady ####
    output$plot_sim <- renderPlotly({
        sim <- tuneParams_run_steady(params(), return_sim = TRUE,
                                     params = params, logs = logs,
                                     session = session, input = input)
        plotlyBiomass(sim)
    })
}
