prepare_params <- function(p) {
    p@species_params$species <- as.character(p@species_params$species)
    rownames(p@species_params) <- p@species_params$species
    p <- set_species_param_default(p, "a", 0.006)
    p <- set_species_param_default(p, "b", 3)
    p <- set_species_param_default(p, "t0", 0)
    rdi <- getRDI(p)
    rdd <- getRDD(p)
    p@species_params$erepro <- p@species_params$erepro * rdd / rdi
    p@species_params$R_max <- Inf
    return(p)
}

#' @export
tuneParams_update_species <- function(sp, p, params) {
    # wrap the code in trycatch so that when there is a problem we can
    # simply stay with the old parameters
    tryCatch({
        # The spectrum for the changed species is calculated with new
        # parameters but in the context of the original community
        # Compute death rate for changed species
        mumu <- getMort(p)[sp, ]
        # compute growth rate for changed species
        gg <- getEGrowth(p)[sp, ]
        # Compute solution for changed species
        w_inf_idx <- sum(p@w < p@species_params[sp, "w_inf"])
        idx <- p@w_min_idx[sp]:(w_inf_idx - 1)
        if (any(gg[idx] == 0)) {
            weight <- p@w[which.max(gg[idx] == 0)]
            showModal(modalDialog(
                title = "Zero growth rate",
                paste0("With these parameter values the ", sp,
                       " does not have enough food to cover its metabolic cost"),
                easyClose = TRUE
            ))
        }
        n0 <- p@initial_n[sp, p@w_min_idx[sp]]
        p@initial_n[sp, ] <- 0
        p@initial_n[sp, p@w_min_idx[sp]:w_inf_idx] <-
            c(1, cumprod(gg[idx] / ((gg + mumu * p@dw)[idx + 1]))) *
            n0
        if (any(is.infinite(p@initial_n))) {
            stop("Candidate steady state holds infinities")
        }
        if (any(is.na(p@initial_n) || is.nan(p@initial_n))) {
            stop("Candidate steady state holds non-numeric values")
        }

        # Retune the value of erepro so that we get the correct level of
        # reproduction
        i <- which(p@species_params$species == sp)
        rdd <- getRDD(p)[i]
        gg0 <- gg[p@w_min_idx[i]]
        mumu0 <- mumu[p@w_min_idx[i]]
        DW <- p@dw[p@w_min_idx[i]]
        p@species_params$erepro[i] <- p@species_params$erepro[i] *
            n0 * (gg0 + DW * mumu0) / rdd

        # Update the reactive params object
        params(p)
    },
    error = function(e) {
        showModal(modalDialog(
            title = "Invalid parameters",
            HTML(paste0("These parameter values lead to an error.<br>",
                        "The error message was:<br>", e)),
            easyClose = TRUE
        ))
        params(p)}
    )
}


# Define function that runs to steady state using `steady()` and
# then adds the new steady state to the logs
#' @export
tuneParams_run_steady <- function(p, params, logs, session, return_sim = FALSE) {

    tryCatch({
        # Create a Progress object
        progress <- shiny::Progress$new(session)
        on.exit(progress$close())

        # Run to steady state
        if (return_sim) {
            # This is for the "Steady" tab where we want to show the
            # evolution of biomass over time during the run to steady
            # to diagnose eventual problems.
            return(steady(p, t_max = 100, tol = 1e-2,
                          return_sim = TRUE,
                          progress_bar = progress))
        } else {
            p <- steady(p, t_max = 100, tol = 1e-2,
                        progress_bar = progress)
            # Update the reactive params object
            params(p)
            tuneParams_add_to_logs(logs, p)
        }
    },
    error = function(e) {
        showModal(modalDialog(
            title = "Invalid parameters",
            HTML(paste0("These parameter do not lead to an acceptable steady state. ",
                        "Please choose other values.<br>",
                        "The error message was:<br>", e)),
            easyClose = TRUE
        ))}
    )
}

#' @export
tuneParams_add_to_logs <- function(logs, p) {
    # Save params object to disk
    time = format(Sys.time(), "_%Y_%m_%d_at_%H_%M_%S")
    file = paste0(tempdir(), "/mizer_params", time, ".rds")
    saveRDS(p, file = file)
    # Update logs
    if (logs$idx < length(logs$files)) {
        file.remove(logs$files[(logs$idx + 1):length(logs$files)])
    }
    logs$files <- append(logs$files[min(1, logs$idx):logs$idx], file)
    logs$idx <- logs$idx + 1
    shinyjs::disable("redo")
    if (logs$idx > 1) {
        shinyjs::enable("undo")
        shinyjs::enable("undo_all")
    }
}

fileManagement <- function(input, output, session, params, logs) {

    output$file_management <- renderUI(
        tagList(
            tags$h3(tags$a(id = "file"), "File management"),
            textOutput("filename"),
            fileInput("upload", "Upload new params",
                      accept = ".rds"),
            downloadButton("params", "Download params")))

    ## Handle upload of params object ####
    observeEvent(input$upload, {
        inFile <- input$upload
        tryCatch({
            p <- readRDS(inFile$datapath)
            validObject(p)
            # Update species selector
            species <- as.character(p@species_params$species[!is.na(p@A)])
            updateSelectInput(session, "sp",
                              choices = species,
                              selected = species[1])

            # Update the reactive params object
            params(prepare_params(p))
            output$filename <- renderText(paste0("Previously uploaded file: ",
                                                 inFile$name))
        },
        error = function(e) {
            showModal(modalDialog(
                title = "Invalid parameter file",
                HTML(paste0("Trying to load that file led to an error.<br>",
                            "The error message was:<br>", e)),
                easyClose = TRUE
            ))
            p <- params()}
        )
    })

    ## Prepare for download of params object ####
    output$params <- downloadHandler(
        filename = "params.rds",
        content = function(file) {
            saveRDS(params(), file = file)
        })
}
