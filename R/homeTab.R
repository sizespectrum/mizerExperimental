#' tuneParams home tab
#'
#' This tab shows:
#'
#'
#'
#' @inheritParams abundanceControl
#' @param logs Environment holding the log of steady states.
#' @param trigger_update Reactive value used for triggering update of
#'   species parameter sliders.
#' @param ... Unused
homeTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {

    output$eu <- renderImage({
        list(src = normalizePath(file.path('./www/EU.jpg')))
    }, deleteFile = FALSE)
#
#     output$imas <- renderImage({
#         list(src = normalizePath(file.path('./www/IMAS.jpg')))
#     }, deleteFile = FALSE)
#
#     output$mizer <- renderImage({
#         list(src = normalizePath(file.path('./www/mizer.jpg')))
#     }, deleteFile = FALSE)
#
#     output$nature <- renderImage({
#         list(src = normalizePath(file.path('./www/NatureResearchCentre.png')))
#     }, deleteFile = FALSE)

    output$banner <- renderImage({
        list(src = normalizePath(file.path('./www/tuneParamsHome.png')))
    }, deleteFile = FALSE)


}

#' @rdname homeTab
#'
#' @param help Boolean. If FALSE then the help text is not included on the tab.
#'   This is useful when including this tab as an element of another tab.
homeTabUI <- function(params, help = TRUE, ...) {

    mainPanel(

        #"Home Page", # text shown on the tab

        imageOutput("mizer", width = "100%"),

        # img(src ='EU.jpg', width = '100%', align = "center"),
        # img(src = normalizePath(file.path('./www/EU.jpg')), width = '100%', align = "center"),


        h2("Tuning and exploring multi-species size spectrum models parameters"), # title at hop of page

        p("This application allows you to explore a wide range of outputs of multi-species size spectrum models
          developed using the modelling package mizer (http://sizespectrum.org/mizer/). Mizer modelling framework
          has been used to investigate a range of aquatics size-structured ecosystems. Its assumptions, equations,
          user guides, examples and references are described in detail on http://sizespectrum.org/mizer/."),

        p("This shiny R application can be used for both exploring outputs of already existing models, but also
          to tune parameters for models that are being developed. You can explore species and resource size spectra,
          diets, biomasses and yields, sources of mortality across different sizes, growth and reproduction and so on.
          For a quick set of instructions, click “Help” on the top left of this application. Mizer modelling community
          has a range of online tutorials and running various online courses. "),

        p("To run this application you will need a mizer add-on mizerMR, which enables to model ecosystems with multiple
          size-structured background resources, such as plankton, benthos and others (e.g. different types of benthos).
          You will also have to use the “experimental” mizer branch, which has a lot of new features but is not yet in
          an official CRAN release."),

        p("This shiny R application has been developed by Gustav Delius and Romain Forestier. Its development was
          supported by the European Regional Development Fund (project No 01.2.2-LMT-K-718-02-0006) under grant
          agreement with the Research Council of Lithuania (LMTLT)."),

        # img(src ="IMAS.jpg", width = '25%', align = "left"),
        # img(src ='mizer.png', width = '25%', align = "center"),
        # img(src ='NatureResearchCentre.png', width = '25%', align = "center"),
        # img(src ='SIF.jpg', width = '25%', align = "right"),


        # imageOutput("imas",),
        # imageOutput("eu"),
        # imageOutput("nature"),
        imageOutput("banner", width = "100%")

    )

}

