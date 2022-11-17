simTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_sim")
    )
}

simTab <- function(input, output, session, params, logs, match, ...) {
    
    ## Plot run to steady ####
    output$plot_sim <- renderPlotly({
        sim <- tuneParams_run_steady(params(), return_sim = TRUE,
                                     params = params, logs = logs,
                                     session = session, input = input,
                                     match = match)
        plotlyBiomass(sim)
    })
}
