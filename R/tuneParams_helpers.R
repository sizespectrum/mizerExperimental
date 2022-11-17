prepare_params <- function(p) {
    p@species_params$species <- as.character(p@species_params$species)
    rownames(p@species_params) <- p@species_params$species
    p <- set_species_param_default(p, "a", 0.006)
    p <- set_species_param_default(p, "b", 3)
    p <- set_species_param_default(p, "k_vb", NA)
    p <- set_species_param_default(p, "t0", 0)
    return(p)
}

# This is called when a params object is downloaded or when the done button
# is pressed
finalise_params <- function(p) {
    if ("tuneParams_old_repro_level" %in% names(p@species_params)) {
        p <- setBevertonHolt(p, reproduction_level =
                                 p@species_params$tuneParams_old_repro_level)
        p@species_params$tuneParams_old_repro_level <- NULL
    }
    if ("tuneParams_old_R_max" %in% names(p@species_params)) {
        p <- setBevertonHolt(p, R_max =  p@species_params$tuneParams_old_R_max)
        p@species_params$tuneParams_old_R_max <- NULL
    }
    if ("tuneParams_old_erepro" %in% names(p@species_params)) {
        p <- setBevertonHolt(p, erepro =  p@species_params$tuneParams_old_erepro)
        p@species_params$tuneParams_old_erepro <- NULL
    }
    p
}


tuneParams_update_species <- function(sp, p, params, params_old) {
    # wrap the code in trycatch so that when there is a problem we can
    # simply stay with the old parameters
    tryCatch({
        # The spectrum for the changed species is calculated with new
        # parameters but in the context of the original community
        p_old <- params_old()
        n <- p_old@initial_n
        n_pp <- p_old@initial_n_pp
        n_other <- p_old@initial_n_other
        # Compute death rate for changed species
        mumu <- getMort(p, n = n, n_pp = n_pp, n_other = n_other)[sp, ]
        # compute growth rate for changed species
        gg <- getEGrowth(p, n = n, n_pp = n_pp, n_other = n_other)[sp, ]
        # Compute solution for changed species
        w_inf_idx <- sum(p@w < p@species_params[sp, "w_inf"])
        idx <- p@w_min_idx[sp]:(w_inf_idx - 1)
        if (any(gg[idx] == 0)) {
            stop("With these parameter values the ", sp,
                 " does not have enough food to cover its metabolic cost")
        }
        n0 <- p@initial_n[sp, p@w_min_idx[sp]]
        p@initial_n[sp, ] <- 0
        p@initial_n[sp, p@w_min_idx[sp]:w_inf_idx] <-
            c(1, cumprod(gg[idx] / ((gg + mumu * p@dw)[idx + 1]))) *
            n0
        if (any(is.infinite(p@initial_n))) {
            stop("Candidate steady state holds infinities")
        }
        if (any(is.na(p@initial_n) | is.nan(p@initial_n))) {
            stop("Candidate steady state holds non-numeric values")
        }

        # Update the reactive params object
        params(p)
    },
    error = function(e) {
        error_fun(e)
        params(p)
    })
}


# Define function that runs to steady state using `steady()` and
# then adds the new steady state to the logs
tuneParams_run_steady <- function(p, params, params_old, logs, session, input,
                                  match, return_sim = FALSE) {

    tryCatch({
        # Create a Progress object
        progress <- shiny::Progress$new(session)
        on.exit(progress$close())
        
        if ("growth" %in% match) {
            p <- matchGrowth(p)
        }
        if ("biomass" %in% match) {
            p <- matchBiomasses(p)
        } 
        if ("yield" %in% match) {
            p <- matchYields(p)
        }
        
        # Run to steady state
        if (return_sim) {
            # This is for the "Steady" tab where we want to show the
            # evolution of biomass over time during the run to steady
            # to diagnose eventual problems.
            return(mizer::steady(p, t_max = 100, tol = 1e-2,
                          return_sim = TRUE,
                          progress_bar = progress))
        }
        p <- mizer::steady(p, t_max = 100, tol = 1e-2,
                           progress_bar = progress)

        # Update the egg slider
        sp_idx <- which.max(p@species_params$species == isolate(input$sp))
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))

        # Update the reactive params objects
        params(p)
        params_old(p)
        tuneParams_add_to_logs(logs, p)
    },
    error = error_fun)
}


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

error_fun <- function(e) {
    showModal(modalDialog(
        title = "Invalid parameters",
        HTML(paste0("These parameter do not lead to an acceptable steady state. ",
                    "Please choose other values.<br>",
                    "The error message was:<br>", e)),
        easyClose = TRUE
    ))}

# Convert the tab name given by the user to lower case, because the names of
# the tab functions will always start with lower case.
tab_name <- function(tab) {
    tabname <- tab
    substr(tabname, 1, 1) <- tolower(substr(tab, 1, 1))
    tabname
}

# Return the title for the tab. This is either defined by the tab author or
# otherwise is the tab name supplied by the user.
tab_title <- function(tab) {
    tabname <- tab_name(tab)
    title_var <- paste0(tabname, "TabTitle")
    if (!is.null(title <- get0(title_var))) {
        if (!is.string(title)) {
            stop(title_var, "should contain a string with the title for the tab")
        }
        return(title)
    }
    tab
}
