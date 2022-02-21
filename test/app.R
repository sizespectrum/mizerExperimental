library(mizerExperimental)
library(mizerMR)
library(mizerStarvation)
library(rintrojs)

params <- SEAparams
controls = c("abundance",
             "predation",
             "fishing",
             "reproduction",
             "other",
             "interaction",
             "resource")
tabs = c("Home",
         "Spectra",
         "Abundance",
         "Growth",
         "Repro",
         "Catch",
         "Diet",
         "Death",
         "Resource",
         "Rates",
         "Sim")
match = c("none", "number", "biomass", "yield")
preserve = c("erepro", "reproduction_level", "R_max")

# match <- match.arg(match)
# Define some local variables to avoid "no visible bindings for global
# variable" warnings in CMD check
wpredator <- wprey <- Nprey <- weight_kernel <- L_inf <-
    Legend <- w_mat <- erepro <- Type <- Abundance <- Catch <-
    Kernel <- Numbers <- Cause <- psi <- Predator <- Density <- NULL
# I am not sure why this is needed, but without it the tooltips won't show.
require("shinyBS")

# Flags to skip certain observers ----
flags <- new.env()

# Prepare logs for undo/redo functionality ----
logs <- new.env()
logs$files <- vector(mode = "character")
logs$idx <- 0

if (missing(params)) {
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
    p <- params # just because I was lazy and because I am using params later
    validObject(p)
    # Add the info that should be preserved to the species_params for later
    # recall
    # preserve <- match.arg(preserve)
    if (preserve == "reproduction_level") {
        p@species_params$tuneParams_old_repro_level <-
            getReproductionLevel(p)
    }
    if (preserve == "R_max") {
        p@species_params$tuneParams_old_R_max <- p@species_params$R_max
    } else {
        p@species_params$tuneParams_old_erepro <- p@species_params$erepro
    }
    p <- prepare_params(p)
}

# User interface ----
ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "cerulean"),
    shinyjs::useShinyjs(),
    rintrojs::introjsUI(),
    tags$script(HTML("$(function(){
          $(document).keydown(function(e) {
          if (e.which == 83) {
            $('#sp_steady').click()
          }
          if (e.which == 78) {
            $('#next_sp').click()
          }
          if (e.which == 80) {
            $('#previous_sp').click()
          }
        });})")),
    tags$head(
        tags$style(HTML(".center{float:center;}"))),

    sidebarLayout(

        ## Sidebar ####
        sidebarPanel(
            width = 3,
            introBox(
                tipify(actionButton("help", "Help"),
                       title = "Start the introductory instructions"),
                tipify(downloadButton("params", ""),
                       title = "Download the current params object"),
                tipify(actionButton("done", "Return", icon = icon("check"),
                                    onclick = "setTimeout(function(){window.close();},500);"),
                       title = "Return the current params objects to R"),
                data.step = 8,
                data.intro = "At any point you can press the download button to save the current state of the params object. When you press the 'Return' button, the gadget will close and the current params object will be returned. The undo log will be cleared."
            ),
            introBox(
                tipify(actionButton("sp_steady", HTML("<u>s</u>teady")),
                       title = "Find steady state. Keyboard shortcut: s"),
                # We should not put a tooltip on the Undo or Redo buttons
                # because they get stuck when the button gets disabled
                actionButton("undo", "", icon = icon("undo")),
                actionButton("redo", "", icon = icon("redo")),
                actionButton("undo_all", "", icon = icon("fast-backward")),
                data.step = 5,
                data.intro = "Each time you change a parameter, the spectrum of the selected species is immediately recalculated. However this does not take into account the effect on the other species. It therefore also does not take into account the second-order effect on the target species that is induced by the changes in the other species. To calculate the true multi-species steady state you have to press the 'Steady' button or hit 's' on the keyboard. You should do this frequently, before changing the parameters too much. Otherwise there is the risk that the steady state can not be found any more. Another advantage of calculating the steady-state frequently is that the app keeps a log of all steady states. You can go backwards and forwards among the previously calculated steady states with the 'Undo' and 'Redo' buttons. The last button winds back all the way to the initial state."
            ),
            tags$br(),
            introBox(uiOutput("sp_sel"),
                     data.step = 2,
                     data.position = "right",
                     data.intro = "Here you select the species whose parameters you want to change or whose properties you want to concentrate on."
            ),
            introBox(
                introBox(
                    # Add links to input sections
                    lapply(controls, function(section) {
                        list("->",
                             tags$a(section, href = paste0("#", section)))
                    }),
                    data.step = 4,
                    data.intro = "There are many parameters, organised into sections. To avoid too much scrolling you can click on a link to jump to a section."),
                tags$br(),
                tags$div(id = "params",
                         uiOutput("sp_params")
                ),
                tags$head(tags$style(
                    type = 'text/css',
                    '#params { max-height: 60vh; overflow-y: auto; }'
                )),
                data.step = 3,
                data.intro = "Here you find controls for changing model parameters. The controls for species-specific parameters are for the species you have chosen above. Many of the controls are sliders that you can move by dragging or by clicking. As you change parameters, the plots in the main panel will immediately update."
            )
        ),  # endsidebarpanel

        ## Main panel ####
        mainPanel(
            width = 9,
            introBox(uiOutput("tabs"),
                     data.step = 1,
                     data.intro = "This main panel has tabs that display various aspects of the steady state of your model. At the bottom of each tab you find text explanations for that tab. You may need to scroll down in the tab to see them. Individual components may show tooltips when you hover over them."
            )
        )  # end mainpanel
    )  # end sidebarlayout
)

server <- function(input, output, session) {
    hintjs(session)
    ## Store params object as a reactive value ####
    params <- reactiveVal(p)
    params_old <- reactiveVal(p)
    tuneParams_add_to_logs(logs, p)  # This allows us to get back to the initial state
    if (logs$idx == length(logs$files)) shinyjs::disable("redo")
    if (logs$idx <= 1) {
        shinyjs::disable("undo")
    }

    # Define a reactive value for triggering an update of species sliders
    trigger_update <- reactiveVal(0)

    ## UI for side bar ####
    # Drop-down menu for selecting active species
    output$sp_sel <- renderUI({
        p <- isolate(params())
        species <- as.character(p@species_params$species[!is.na(p@A)])
        tagList(
            popify(selectInput("sp", "Species to tune:", species),
                   placement = "right",
                   title = "Species to tune",
                   content = "Here you select the species whose parameters you want to change or whose properties you want to concentrate on. You can also sometimes change this selection by clicking on a species in some of the plots."),
            tipify(actionButton("previous_sp", HTML("<u>p</u>revious")),
                   title = "Select previous species. Keyboard shortcut: p"),
            tipify(actionButton("next_sp", HTML("<u>n</u>ext")),
                   title = "Select next species. Keyboard shortcut: n"))
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

        lapply(controls,
               function(section) {
                   do.call(paste0(section, "ControlUI"),
                           list(p = p, input = input))
               })
    })

    # Serve controls ####
    for (section in controls) {
        fun <- paste0(tolower(section), "Control")
        do.call(fun, list(input = input,
                          output = output,
                          session = session,
                          params = params,
                          params_old = params_old,
                          flags = flags,
                          trigger_update = trigger_update))
    }

    ## UI for tabs ####
    output$tabs <- renderUI({
        tablist <- lapply(tabs, function(tab) {
            tabname <- tab
            substr(tabname, 1, 1) <- tolower(substr(tab, 1, 1))
            tab_content <- div(
                style = "max-height: 94vh; overflow-y: auto; overflow-x: hidden;",
                do.call(paste0(tabname, "TabUI"),
                        list(params = params)))
            tabPanel(tab, tab_content)
        })
        args <- c(id = "mainTabs", type = "tabs", tablist)
        do.call(tabsetPanel, args)
    })

    ## Serve tabs ####
    for (tab in tabs) {
        tabname <- tab
        substr(tabname, 1, 1) <- tolower(substr(tab, 1, 1))
        fun <- paste0(tabname, "Tab")
        do.call(fun, list(input = input,
                          output = output,
                          session = session,
                          params = params,
                          logs = logs,
                          trigger_update = trigger_update))
    }

    # Help button ----
    observeEvent(
        input$help,
        introjs(session)
    )

    ## Steady ####
    # triggered by "Steady" button in sidebar
    observeEvent(input$sp_steady, {
        tuneParams_run_steady(params(), params = params,
                              params_old = params_old,
                              logs = logs, session = session, input = input,
                              match = match)
    })

    ## Previous ####
    observeEvent(input$previous_sp, {
        p <- params()
        all_species <- species_params(p)$species[!is.na(p@A)]
        no_sp <- length(all_species)
        idx <- which(all_species == input$sp)
        prev <- ((idx - 2) %% no_sp) + 1
        updateSelectInput(session, "sp",
                          selected = all_species[[prev]])
    })
    ## Next ####
    observeEvent(input$next_sp, {
        p <- params()
        all_species <- species_params(p)$species[!is.na(p@A)]
        no_sp <- length(all_species)
        idx <- which(all_species == input$sp)
        updateSelectInput(session, "sp",
                          selected = all_species[[(idx %% no_sp) + 1]])
    })

    ## Undo ####
    observeEvent(input$undo, {
        if (logs$idx <= 1) return()
        p_new <- readRDS(logs$files[logs$idx])
        p_old <- params()
        # if the params have not changed, go to the previous one
        if ((nrow(p_old@species_params) == nrow(p_new@species_params)) &&
            all(p_old@species_params == p_new@species_params, na.rm = TRUE)) {
            logs$idx <- logs$idx - 1
            shinyjs::enable("redo")
            p_new <- readRDS(logs$files[logs$idx])
            if (logs$idx == 1) {
                shinyjs::disable("undo")
            }
        }
        params(p_new)
        params_old(p_new)
        # Trigger an update of sliders
        rm(list = ls(flags), pos = flags)
        trigger_update(runif(1))
    })
    ## Redo ####
    observeEvent(input$redo, {
        if (logs$idx >= length(logs$files)) return()
        logs$idx <- logs$idx + 1
        p <- readRDS(logs$files[logs$idx])
        params(p)
        params_old(p)
        # Trigger an update of sliders
        rm(list = ls(flags), pos = flags)
        trigger_update(runif(1))
        shinyjs::enable("undo")
        shinyjs::enable("undo_all")
        if (logs$idx == length(logs$files)) shinyjs::disable("redo")
    })
    ## Undo All ####
    observeEvent(input$undo_all, {
        if (logs$idx > 1) shinyjs::enable("redo")
        shinyjs::disable("undo")
        logs$idx <- 1
        p <- readRDS(logs$files[logs$idx])
        params(p)
        params_old(p)
        # Trigger an update of sliders
        rm(list = ls(flags), pos = flags)
        trigger_update(runif(1))
    })

    ## Prepare for download of params object ####
    output$params <- downloadHandler(
        filename = "tuned_params.rds",
        content = function(file) {
            saveRDS(finalise_params(params()), file = file)
        })

    ## Return ####
    # When the user hits the "Return" button we want to clear the logs and
    # return with the latest params object
    observeEvent(input$done, {
        file.remove(logs$files)
        stopApp(finalise_params(params()))
    })

} #the server

runGadget(ui, server, viewer = browserViewer())
# shinyApp(ui = ui, server = server)

