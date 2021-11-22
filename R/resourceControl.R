#' Controlling the resource parameters in the tuning gadget
#' @inheritParams abundanceControl
resourceControl <- function(input, output, session, params, flags, ...) {
    observe({
        re <- isolate(input$re) # to know which background is selected
        p <- isolate(params())
        req(input$kappa,
            input$lambda,
            input$log_r_pp,
            input$w_pp_cutoff,
            input$n_resource)
        if(!is.null(getComponent(p, "MR")))
        {
            no_re <- which(p@other_params$other$MR$resource_params$resource == re)
            mizerMR::resource_params(p)$kappa[no_re] <- input$kappa
            mizerMR::resource_params(p)$lambda[no_re] <- input$lambda
            mizerMR::resource_params(p)$r_pp[no_re]  <- (10^input$log_r_pp)
            mizerMR::resource_params(p)$w_max[no_re] <- input$w_pp_cutoff
            mizerMR::resource_params(p)$n[no_re]     <- input$n_resource
            mizerMR::initialNResource(p) <- mizerMR::resource_capacity(p)

        } else {
        p <- setResource(p,
                         kappa = input$kappa,
                         lambda = input$lambda,
                         r_pp = 10^input$log_r_pp,
                         w_pp_cutoff = input$w_pp_cutoff,
                         n = input$n_resource)
        mu <- getResourceMort(p)
        p@initial_n_pp <- p@rr_pp * p@cc_pp / (p@rr_pp + mu)
        }

        params(p)
    })
}

#' @rdname resourceControl
#' @inheritParams abundanceControlUI
resourceControlUI <- function(p, sp, re) {

    if(!is.null(getComponent(p, "MR")))
    {
        tagList(
            tags$h3(tags$a(id = "resource"), "Resource"),
            numericInput("lambda", "Sheldon exponent 'lambda'",
                         value = p@other_params$other$MR$resource_params[re,]$lambda,
                         min = 1.9, max = 2.2, step = 0.005),
            numericInput("kappa", "Resource coefficient 'kappa'",
                         value = p@other_params$other$MR$resource_params[re,]$kappa),
            sliderInput("log_r_pp", "log10 Resource replenishment rate",
                        value = log10(p@other_params$other$MR$resource_params[re,]$r_pp),
                        min = -1, max = 4, step = 0.05),
            numericInput("n_resource", "Exponent of replenishment rate",
                         value = p@other_params$other$MR$resource_params[re,]$n,
                         min = 0.6, max = 0.8, step = 0.005),
            numericInput("w_pp_cutoff", "Largest resource",
                         value = p@other_params$other$MR$resource_params[re,]$w_max,
                         min = 1e-10,
                         max = 1e3))
        } else {
    log_r_pp <- log10(p@resource_params$r_pp)
    tagList(
        tags$h3(tags$a(id = "resource"), "Resource"),
        numericInput("lambda", "Sheldon exponent 'lambda'",
                     value = p@resource_params$lambda,
                     min = 1.9, max = 2.2, step = 0.005),
        numericInput("kappa", "Resource coefficient 'kappa'",
                     value = p@resource_params$kappa),
        sliderInput("log_r_pp", "log10 Resource replenishment rate",
                    value = log_r_pp, min = -1, max = 4, step = 0.05),
        numericInput("n_resource", "Exponent of replenishment rate",
                     value = p@resource_params$n,
                     min = 0.6, max = 0.8, step = 0.005),
        numericInput("w_pp_cutoff", "Largest resource",
                     value = p@resource_params$w_pp_cutoff,
                     min = 1e-10,
                     max = 1e3))
        }
}
