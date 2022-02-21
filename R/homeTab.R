#' tuneParams home tab
#'
#' This tab contains a brief introduction to the app
#' and the sponsors
#'
#'
#'
#' @inheritParams abundanceControl
#' @param logs Environment holding the log of steady states.
#' @param trigger_update Reactive value used for triggering update of
#'   species parameter sliders.
#' @param ... Unused
#' @export
homeTab <- function(input, output, session,
                    params, logs, trigger_update, ...) {

    output$mizer <- renderImage({
        list(src = system.file("logos", "mizerHome.png",
                               package = "mizerExperimental"))
    }, deleteFile = FALSE)

    output$banner <- renderImage({
        list(src = system.file("logos", "tuneParamsHome.png",
                               package = "mizerExperimental"))
    }, deleteFile = FALSE)
}

#' @rdname homeTab
#'
#' @param help Boolean. If FALSE then the help text is not included on the tab.
#'   This is useful when including this tab as an element of another tab.
#' @export
homeTabUI <- function(params, help = TRUE, ...) {

    mainPanel(
        imageOutput("mizer"),

        h2("Tuning and exploring size spectrum models with multiple background resources"),

        # p('This application allows you to explore a wide range of outputs of multi-species size spectrum
        #   models developed using a publicly available R package',
        #   a('mizer.', href = 'http://sizespectrum.org/mizer/'),em('mizer'),
        #   'modelling framework has been used to investigate a range of aquatic size-structured ecosystems.
        #   Its assumptions, equations, user guides, examples and references are described in detail on ',
        #   a('http://sizespectrum.org/mizer/.', href = 'http://sizespectrum.org/mizer/')),
        #
        # p('Here, the shiny R, application has been adapted for inland and coastal ecosystems by introducing multiple
        #   size structured background resources. This enables modelling of independent, size structured energy pathways,
        #   such as pelagic, benthic, macroalgal and others. By specifying different fish diet preferences for alternative
        #   resources and different resource size distributions, emergent fish diets more accurately resemble empirically
        #   observed ontogenetic dietary transitions. The multiple resource extension is implemented in a free', em('mizer'),
        #   'add-on', a('mizerMR', href = 'https://github.com/sizespectrum/mizerMR'),
        #   'and is important for modelling ecosystems, where
        #   independent pelagic and benthic energy pathways play a significant role.'),

        p("This application can be used to investigate outputs of already existing models and to develop new models.
          You can explore species and resource size spectra, diets, biomasses and yields, sources of mortality across
          different fish sizes, growth, reproduction and other emergent properties. For a quick set of instructions,
          click “Help” on the top left of this application. "),

        p("To run the application locally, follow these steps:"),

        p("remotes::install_github('sizespectrum/mizerExperimental@TasModel')"),

        p("library(mizerExperimental)"),

        p("load any other necessary library such as mizerMR using 'library()' then run"),

        p("tuneParams(object)"),

        p("where 'object' is an object of class mizerParams, containing the ecosystem's
          parameters."),

        p("This shiny R application has been developed by Gustav Delius and Romain Forestier.
          Its development was supported by the European Regional Development Fund (project No 01.2.2-LMT-K-718-02-0006)
          under grant agreement with the Research Council of Lithuania (LMTLT)."),

        imageOutput("banner")
    )

}

