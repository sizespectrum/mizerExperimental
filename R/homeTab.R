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


}

#' @rdname homeTab
#'
#' @param help Boolean. If FALSE then the help text is not included on the tab.
#'   This is useful when including this tab as an element of another tab.
homeTabUI <- function(params, help = TRUE, ...) {

    tabPanel(

        "Home Page", # text shown on the tab

        h2("title at top of page"), # title at hop of page

        p("first para goes here"),

        p("2nd para goes here"),

        p("3rd para goes here"),

        p("4th para goes here")#,

        #img(src ='logos.png', width = '100%', align = "center")

    )

}
