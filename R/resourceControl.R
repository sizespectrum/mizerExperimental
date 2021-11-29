#' Controlling the resource parameters in the tuning gadget
#' @inheritParams abundanceControl
resourceControl <- function(input, output, session, params, flags, ...) {

    observe({
        re <- isolate(input$re)
        p <- isolate(params())
        req(input$kappa,
            input$lambda,
            input$log_r_pp,
            input$w_pp_cutoff,
            input$n_resource)
        if(!is.null(getComponent(p, "MR")))
        {
            sel_re <- which(p@other_params$other$MR$resource_params$resource == re)
            mizerMR::resource_params(p)$kappa[sel_re]  <- input$kappa
            mizerMR::resource_params(p)$lambda[sel_re] <- input$lambda
            mizerMR::resource_params(p)$r_pp[sel_re]   <- (10^input$log_r_pp)
            mizerMR::resource_params(p)$w_max[sel_re]  <- input$w_pp_cutoff
            mizerMR::resource_params(p)$n[sel_re]      <- input$n_resource
            mizerMR::resource_params(p)$w_min[sel_re]  <- input$w_min_resource
            mu <- getResourceMort(p)
            mizerMR::initialNResource(p) <- mizerMR::resource_rate(p) *
                mizerMR::resource_capacity(p) / (mizerMR::resource_rate(p) + mu)
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
resourceControlUI <- function(p, input) {

    if(!is.null(getComponent(p, "MR"))) # is there one or more resources?
    {
        resources <-  as.character(p@other_params$other$MR$resource_params$resource)
        if(is.null(input$re)) # need to initialise slider when app starts
        {
            tagList(
                tags$h3(tags$a(id = "resource"), "Resource"),
                selectInput("re", "Resource to tune:", resources, selected = resources[1]))
            # not adding the parameter sliders as this section just runs once at the start
        } else {
            tagList(
                tags$h3(tags$a(id = "resource"), "Resource"),

                selectInput(inputId = "re", label = "Resource to tune:", choices = resources, selected = input$re),

                numericInput("lambda", "Sheldon exponent 'lambda'",
                             value = p@other_params$other$MR$resource_params[input$re,]$lambda,
                             min = 1.9, max = 2.2, step = 0.005),
                numericInput("kappa", "Resource coefficient 'kappa'",
                             value = p@other_params$other$MR$resource_params[input$re,]$kappa),
                sliderInput("log_r_pp", "log10 Resource replenishment rate",
                            value = log10(p@other_params$other$MR$resource_params[input$re,]$r_pp),
                            min = -1, max = 4, step = 0.05),
                numericInput("n_resource", "Exponent of replenishment rate",
                             value = p@other_params$other$MR$resource_params[input$re,]$n,
                             min = 0.6, max = 0.8, step = 0.005),
                numericInput("w_pp_cutoff", "Largest resource",
                             value = p@other_params$other$MR$resource_params[input$re,]$w_max,
                             min = 1e-10,
                             max = 1e3),
                numericInput("w_min_resource", "Smallest resource",
                             value = p@other_params$other$MR$resource_params[input$re,]$w_min,
                             min = p@w_full[1],
                             max = 1e3))
        }
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
