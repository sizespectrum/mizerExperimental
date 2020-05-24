#' Launch shiny gadget for tuning parameters
#'
#' The shiny gadget has sliders for the model parameters and tabs with a
#' variety of plots to visualise the steady-state
#'
#' @param p MizerParams object to tune. If missing, the gadget tries to recover
#'   information from log files left over from aborted previous runs.
# #' @param stomach Data frame holding observations of prey items in predator
# #'   stomachs. The required columns are
# #'   \itemize{
# #'   \item \code{species} holding the name of the predator species,
# #'   \item \code{wpredator} with the weight in grams of the predator,
# #'   \item \code{wprey} with the weight of the prey item.
# #'   }
# #'   In case prey items of the same weight have been aggregated in the data
# #'   frame then there should be a column \code{Nprey} saying how many prey
# #'   items have been aggregated in each row.
#' @param controls A list with the names of the sections of input
#'   parameters that should be displayed in the sidebar. Each entry in the
#'   list is a string. For an entry "foo" there needs to be a function
#'   "fooInputs" that defines the input elements and a function "foo" that
#'   processes those inputs to change the params object.
#' @tabs A list with the names of tabs.
#' @param ... Other params needed by individual tabs.
#'
#' This gadget is meant for tuning a model to steady state. It is not meant for
#' tuning the dynamics of the model. That should be done in a second step using
#' functions like `setRmax()` or `changeResource()`.
#'
#' The function opens a shiny gadget, an interactive web page. This page has
#' a side panel with controls for various model parameters and a main panel
#' with tabs for various diagnostic plots.
#'
#' After you click the "Done" button in the side panel, the function will return
#' the parameter object in the state at that time, with `Rmax` set to `Inf`
#' and `erepro` set to the value it had after the last run to steady state.
#'
#' The gadget keeps a log of all steady states you create while working with
#' the gadget. You can go back to the last steady state by hitting the "Undo"
#' button. You can go back an arbitrary number of states and also go forward
#' again. There is also a button to go right back to the initial steady state.
#' When you leave the gadget by hitting the "Done" button, this log is cleared.
#' If you stop the gadget from RStudio by hitting the "Stop" button, then the
#' log is left behind. You can then restart the gadget by calling `tuneParams()`
#' without a `params` argument and it will re-instate the states from the log.
#'
#' At any time the gadget allows the user to download the current params object
#' as an .rds file via the "Download" button in the "File" section, or to
#' upload a params object from an .rds file.
#'
#' There are currently several restrictions on what the gadget can do:
#'
#' The gadget currently assumes that each species is selected by only one gear.
#' It allows the user to change the parameters for that gear. It also enforces
#' the same effort for all gears. It sets all efforts to that for the first
#' gear and then allows the user to change that single effort value.
#'
#' @return The tuned MizerParams object
#' @md
#' @export
tuneParams <- function(p,
                       controls = list("egg",
                                       "predation",
                                       "fishing",
                                       "reproduction",
                                       "other",
                                       "interaction",
                                       "resource"),
                       tabs = list("Spectra",
                                   "Biomass",
                                   "Growth",
                                   "Repro",
                                   "Catch",
                                   "Rates",
                                   "Prey",
                                   "Diet",
                                   "Death",
                                   "Resource",
                                   "Sim"),
                       ...) {
    # Define some local variables to avoid "no visible bindings for global
    # variable" warnings in CMD check
    wpredator <- wprey <- Nprey <- weight_kernel <- L_inf <-
        Legend <- w_mat <- erepro <- Type <- Abundance <- Catch <-
        Kernel <- Numbers <- Cause <- psi <- Predator <- Density <- NULL

    # Flags to skip certain observers ----
    flags <- new.env()

    # Prepare logs for undo/redo functionality ----
    logs <- new.env()
    logs$files <- vector(mode = "character")
    logs$idx <- 0

    if (missing(p)) {
        # Try to recover old log files ----
        logs$files <- sort(list.files(path = tempdir(),
                                pattern = "mizer_params_...._.._.._at_.._.._..\\.rds",
                                full.names = TRUE))
        logs$idx <- length(logs$files)
        if (logs$idx == 0) {
            stop("You need to specify a MizerParams object. ",
                 "There are no temporary parameter files to recover.")
        }
        p <- readRDS(logs$files[logs$idx])
    } else {
        validObject(p)
        p <- prepare_params(p)
    }

    # User interface ----
    ui <- fluidPage(
        shinyjs::useShinyjs(),

        sidebarLayout(

            ## Sidebar ####
            sidebarPanel(
                actionButton("sp_steady", "Steady"),
                actionButton("undo", "", icon = icon("undo")),
                actionButton("redo", "", icon = icon("redo")),
                actionButton("undo_all", "", icon = icon("fast-backward")),
                actionButton("done", "Done", icon = icon("check"),
                             onclick = "setTimeout(function(){window.close();},500);"),
                tags$br(),
                uiOutput("sp_sel"),
                # Add links to input sections
                lapply(controls, function(section) {
                    list("->",
                          tags$a(section, href = paste0("#", section)))
                }),
                "->",
                tags$a("File", href = "#file"),
                tags$br(),
                tags$div(id = "params",
                         uiOutput("sp_params"),
                         uiOutput("file_management")
                ),
                tags$head(tags$style(
                    type = 'text/css',
                    '#params { max-height: 60vh; overflow-y: auto; }'
                )),
                width = 3
            ),  # endsidebarpanel

            ## Main panel ####
            mainPanel(
                uiOutput("tabs")
            )  # end mainpanel
        )  # end sidebarlayout
    )

    server <- function(input, output, session) {

        ## Store params object as a reactive value ####
        params <- reactiveVal(p)
        add_to_logs(logs, p)  # This allows us to get back to the initial state
        if (logs$idx == length(logs$files)) shinyjs::disable("redo")
        if (logs$idx <= 1) {
            shinyjs::disable("undo")
            shinyjs::disable("undo_all")
        }

        # The file name will be empty until the user uploads a params file
        output$filename <- renderText("")

        # Define a reactive value for triggering an update of species sliders
        trigger_update <- reactiveVal(0)

        ## UI for side bar ####
        # Drop-down menu for selecting active species
        output$sp_sel <- renderUI({
            p <- isolate(params())
            species <- as.character(p@species_params$species[!is.na(p@A)])
            selectInput("sp", "Species:", species)
        })
        # Sliders for the species parameters
        output$sp_params <- renderUI({
            # The parameter sliders get updated whenever the species selector
            # changes
            req(input$sp)
            # or when the trigger is set somewhere
            trigger_update()
            # but not each time the params change
            p <- isolate(params())
            sp <- p@species_params[input$sp, ]

            lapply(controls,
                   function(section) {
                       do.call(paste0(section, "ControlUI"),
                               list(p = p, sp = sp))
                   })
        })

        fileManagement(input, output, session, params, logs)

        # Serve controls ####
        for (section in controls) {
            fun <- paste0(section, "Control")
            do.call(fun, list(input = input,
                              output = output,
                              session = session,
                              params = params,
                              flags = flags))
        }

        ## UI for tabs ####
        output$tabs <- renderUI({
            tablist <- lapply(tabs, function(tab) {
                tabPanel(tab, do.call(paste0(tolower(tab), "TabUI"), list()))
            })
            args <- c(id = "mainTabs", type = "tabs", tablist)
            do.call(tabsetPanel, args)
        })

        ## Serve tabs ####
        for (tab in tabs) {
            fun <- paste0(tolower(tab), "Tab")
            do.call(fun, list(input = input,
                              output = output,
                              session = session,
                              params = params,
                              logs = logs, ...))
        }

        ## Steady ####
        # triggered by "Steady" button in sidebar
        observeEvent(input$sp_steady, {
            run_steady(params(), params = params,
                       logs = logs, session = session)
        })

        ## Undo ####
        observeEvent(input$undo, {
            if (logs$idx <= 1) return()
            p_new <- readRDS(logs$files[logs$idx])
            p_old <- params()
            # if the params have not changed, go to the previous one
            if (all(p_old@species_params == p_new@species_params, na.rm = TRUE)) {
                logs$idx <<- logs$idx - 1
                shinyjs::enable("redo")
                p_new <- readRDS(logs$files[logs$idx])
                if (logs$idx == 1) {
                    shinyjs::disable("undo")
                    shinyjs::disable("undo_all")
                }
            }
            params(p_new)
            # Trigger an update of sliders
            trigger_update(runif(1))
        })
        ## Redo ####
        observeEvent(input$redo, {
            if (logs$idx >= length(logs$files)) return()
            logs$idx <<- logs$idx + 1
            params(readRDS(logs$files[logs$idx]))
            # Trigger an update of sliders
            trigger_update(runif(1))
            shinyjs::enable("undo")
            shinyjs::enable("undo_all")
            if (logs$idx == length(logs$files)) shinyjs::disable("redo")
        })
        ## Cancel ####
        observeEvent(input$undo_all, {
            if (logs$idx > 1) shinyjs::enable("redo")
            shinyjs::disable("undo")
            shinyjs::disable("undo_all")
            logs$idx <- 1
            params(readRDS(logs$files[logs$idx]))
            # Trigger an update of sliders
            trigger_update(runif(1))
        })

        ## Done ####
        # When the user hits the "Done" button we want to clear the logs and
        # return with the latest params object
        observeEvent(input$done, {
            file.remove(logs$files)
            stopApp(params())
        })

    } #the server

    runGadget(ui, server, viewer = browserViewer())
}
