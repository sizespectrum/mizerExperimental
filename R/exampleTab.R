# To define a new tab for the tuneParams() gadget you need to supply two
# functions, one to define the user interface and one to produce the outputs
# and process any inputs. The name of the functions should start with a unique
# name for the tab, starting with a lower-case letter, 
# followed by either "TabUI" or "Tab".

exampleTabUI <- function(...) {
    # This defines the user interface for the tab. It this example we will
    # include a plot and an input field
    tagList(
        plotlyOutput("plot_example"),
        radioButtons("example_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
    )
}

exampleTab <- function(input, output, session, params, logs, ...) {
    
    ## Plot ####
    output$plot_example <- renderPlotly({
        plotlyDeath(params(), 
                    species = input$sp, 
                    proportion = (input$example_prop == "Proportion"))
    })
}

# You can define a title for the tab which is used to label the tab rider
# in the user interface. If you do not choose a name, then the title will
# be the same as the name of the tab, so in this example it would be "example".
exampleTabTitle <- "E.g."