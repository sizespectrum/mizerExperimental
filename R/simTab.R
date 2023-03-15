simTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_sim")
    )
}

simTab <- function(input, output, session, params, params_old, logs, ...) {
    
    ## Plot run to steady ####
    output$plot_sim <- renderPlotly({
        sim <- tuneParams_run_steady(params(), return_sim = TRUE,
                                     params = params, params_old = params_old,
                                     logs = logs,
                                     session = session, input = input)
        plotlyBiomass(sim)
    })
}
