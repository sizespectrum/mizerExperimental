#' Serve tab with catch data
#'
#' @inheritParams biomassTab
#' @param catch Data frame holding binned observed catch data. The data can be
#'   binned either into length bins or weight bins. In the former case the data
#'   frame should have columns \code{length} and \code{dl} holding the start of
#'   the size bins in cm and the width of the size bins in cm respectively. In
#'   the latter case the data frame should have columns \code{weight} and
#'   \code{dw} holding the start of the size bins in grams and the width of the
#'   size bins in grams. The data frame also needs to have the columns
#'   \code{species} (the name of the species), \code{gear} (the name of the
#'   gear) and \code{catch} (the number of individuals of a particular species
#'   caught by a particular gear in a size bin).
catchTab <- function(input, output, session, params, logs, trigger_update,
                     catch = NULL, ...) {

    if (!is.null(catch)) {
        assert_that(
            is.data.frame(catch),
            "catch" %in% names(catch),
            "species" %in% names(catch),
            "gear" %in% names(catch),
            all(c("length", "dl") %in% names(catch)) |
                all(c("weight", "dw") %in% names(catch))
        )
    }

    # Catch density for selected species ----
    output$plotCatchDist <- renderPlotly({
        plotlyYieldVsSize(params(), species = req(input$sp),
                          gear = input$gear,
                          catch = catch, x_var = input$catch_x)
    })
    
    # Total yield by species ----
    output$plotTotalYield <- renderPlot({
        plotYieldVsSpecies(params(), gear = input$gear) +
            theme(text = element_text(size = 18))
    })
}

#' @rdname catchTab
catchTabUI <- function(...) {
    tagList(
        plotlyOutput("plotCatchDist"),
        radioButtons("catch_x", "Show size in:",
                     choices = c("Weight", "Length"),
                     selected = "Length", inline = TRUE),
        plotOutput("plotTotalYield")
    )
}
