#' Controlling the resource parameters in the tuning gadget
#' @inheritParams abundanceControl
resourceControl <- function(input, output, session, params, flags, ...) {
    observe({
        req(input$kappa,
            input$lambda,
            input$w_pp_cutoff)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_resource)) {
            flags$sp_old_resource <- sp
            return()
        }
        p@resource_params$kappa <- input$kappa
        p@resource_params$lambda <- input$lambda
        p@resource_params$w_pp_cutoff <- input$w_pp_cutoff
        npp <- input$kappa * p@w_full^(-input$lambda)
        npp[p@w_full > input$w_pp_cutoff] <- 0
        initialNResource(p) <- npp
        tuneParams_update_params(p, params)
    })
}

#' @rdname resourceControl
#' @inheritParams abundanceControlUI
resourceControlUI <- function(p, input) {

    tagList(
        tags$h3(tags$a(id = "resource"), "Resource"),
        numericInput("lambda", "Sheldon exponent 'lambda'",
                     value = p@resource_params$lambda,
                     min = 1.9, max = 2.2, step = 0.005),
        numericInput("kappa", "Resource coefficient 'kappa'",
                     value = p@resource_params$kappa),
        numericInput("w_pp_cutoff", "Largest resource",
                     value = p@resource_params$w_pp_cutoff,
                     min = 1e-10,
                     max = 1e3))
}