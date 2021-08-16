eggControlUI <- function(p, sp) {
    n0 <- p@initial_n[sp$species, p@w_min_idx[sp$species]]
    tagList(
        tags$h3(tags$a(id = "egg"), "Abundance"),
        sliderInput("n0", "Egg density",
                    value = n0,
                    min = signif(n0 / 10, 3),
                    max = signif(n0 * 2, 3),
                    step = n0 / 50))
}

eggControl <- function(input, output, session, params, flags) {
    observe({
        n0 <- req(input$n0)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_n0)) {
            flags$sp_old_n0 <- sp
            return()
        }
        updateSliderInput(session, "n0",
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 2, 3))
        # rescale abundance to new egg density
        p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 /
            p@initial_n[sp, p@w_min_idx[sp]]

        # Update the reactive params object
        params(p)
    })
}

resourceControlUI <- function(p, sp) {
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

resourceControl <- function(input, output, session, params, flags) {
    observe({
        req(input$kappa,
            input$lambda,
            input$log_r_pp,
            input$w_pp_cutoff,
            input$n_resource)
        p <- setResource(isolate(params()),
                         kappa = input$kappa,
                         lambda = input$lambda,
                         r_pp = 10^input$log_r_pp,
                         w_pp_cutoff = input$w_pp_cutoff,
                         n = input$n_resource)
        mu <- getResourceMort(p)
        p@initial_n_pp <- p@rr_pp * p@cc_pp / (p@rr_pp + mu)
        params(p)
    })
}

otherControlUI <- function(p, sp) {
    tagList(
        tags$h3(tags$a(id = "other"), "Other"),
        sliderInput("ks", "Coefficient of standard metabolism 'ks'",
                    value = sp$ks,
                    min = signif(sp$ks / 2, 2),
                    max = signif((sp$ks + 0.1) * 1.5, 2),
                    step = 0.05),
        numericInput("p", "Exponent of metabolism 'p'",
                     value = sp$p,
                     min = 0.6, max = 0.8, step = 0.005),
        sliderInput("k", "Coefficient of activity 'k'",
                    value = sp$k,
                    min = signif(sp$k / 2, 2),
                    max = signif((sp$k + 0.1) * 1.5, 2),
                    step = 0.01),
        sliderInput("z0", "External mortality 'z0'",
                    value = sp$z0,
                    min = signif(sp$z0 / 2, 2),
                    max = signif((sp$z0 + 0.1) * 1.5, 2),
                    step = 0.05),
        sliderInput("alpha", "Assimilation efficiency 'alpha'",
                    value = sp$alpha,
                    min = 0,
                    max = 1)
    )
}

otherControl <- function(input, output, session, params, flags) {
    observe({
        req(input$alpha, input$ks, input$k, input$z0)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_other)) {
            flags$sp_old_other <- sp
            return()
        }
        # Update slider min/max so that they are a fixed proportion of the
        # parameter value
        updateSliderInput(session, "ks",
                          min = signif(input$ks / 2, 2),
                          max = signif((input$ks + 0.1) * 1.5, 2))
        updateSliderInput(session, "k",
                          min = signif(input$k / 2, 2),
                          max = signif((input$k + 0.1) * 1.5, 2))
        updateSliderInput(session, "z0",
                          min = signif(input$z0 / 2, 2),
                          max = signif((input$z0 + 0.1) * 1.5, 2))

        p@species_params[sp, "alpha"] <- input$alpha
        p@species_params[sp, "ks"]    <- input$ks
        p@species_params[sp, "k"]     <- input$k
        p@species_params[sp, "z0"]    <- input$z0
        p <- setMetabolicRate(p)
        p <- setExtMort(p)
        tuneParams_update_species(sp, p, params)
    })

    observeEvent(
        input$p,
        {
            p <- params()
            sp <- input$sp

            # change ks so that metabolic rate at maturity stays the same
            p@species_params[[sp, "ks"]] <- p@species_params[[sp, "ks"]] *
                p@species_params[[sp, "w_mat"]] ^
                (p@species_params[[sp, "p"]] - input$p)
            p@species_params[[sp, "p"]] <- input$p
            ks <- p@species_params[[sp, "ks"]]
            updateSliderInput(session, "ks",
                              value = ks,
                              min = signif(ks / 2, 2),
                              max = signif((ks + 0.1) * 1.5, 2))
        },
        ignoreInit = TRUE)
}

reproductionControlUI <- function(p, sp) {
    tagList(
        tags$h3(tags$a(id = "reproduction"), "Reproduction"),
        sliderInput("w_mat", "w_mat", value = sp$w_mat,
                    min = signif(sp$w_mat / 2, 2),
                    max = signif(sp$w_mat * 1.5, 2)),
        sliderInput("wfrac", "w_mat25/w_mat", value = sp$w_mat25/sp$w_mat,
                    min = 0.5,
                    max = 1,
                    step = 0.01),
        sliderInput("w_inf", "w_inf", value = sp$w_inf,
                    min = signif(sp$w_inf / 2, 2),
                    max = signif(sp$w_inf * 1.5, 2)),
        sliderInput("m", "m", value = sp$m,
                    min = sp$n,
                    max = sp$n * 2,
                    step = 0.01)
    )
}

reproductionControl <- function(input, output, session, params, flags) {
    observeEvent(
        list(input$w_mat, input$wfrac, input$w_inf, input$m),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_repro)) {
                flags$sp_old_repro <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "w_mat",
                              min = signif(input$w_mat / 2, 2),
                              max = signif(input$w_mat * 1.5, 2))
            updateSliderInput(session, "w_inf",
                              min = signif(input$w_inf / 2, 2),
                              max = signif(input$w_inf * 1.5, 2))

            p@species_params[sp, "w_mat25"]   <- input$w_mat * input$wfrac
            p@species_params[sp, "w_mat"]   <- input$w_mat
            p@species_params[sp, "w_inf"]   <- input$w_inf
            p@species_params[sp, "m"]     <- input$m

            p <- setReproduction(p)
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)
}

predationControlUI <- function(p, sp) {
    tagList(
        tags$h3(tags$a(id = "predation"), "Predation"),
        sliderInput("gamma", "Predation rate coefficient 'gamma'",
                    value = sp$gamma,
                    min = signif(sp$gamma / 2, 3),
                    max = signif(sp$gamma * 1.5, 3),
                    step = sp$gamma / 50, ticks = FALSE),
        sliderInput("h", "max feeding rate 'h'",
                    value = sp$h,
                    min = signif(sp$h / 2, 2),
                    max = signif(sp$h * 1.5, 2)),
        numericInput("q", "Exponent of search volume 'q'",
                     value = sp$q,
                     min = 0.6, max = 0.8, step = 0.005),
        numericInput("n", "Exponent of max feeding rate 'n'",
                     value = sp$n,
                     min = 0.6, max = 0.8, step = 0.005),
        sliderInput("beta", "Preferred predator-prey mass ratio 'beta'",
                    value = sp$beta,
                    min = signif(sp$beta / 2, 2),
                    max = signif(sp$beta * 1.5, 2)),
        sliderInput("sigma", "Width of size selection function 'sigma'",
                    value = sp$sigma,
                    min = signif(sp$sigma / 2, 2),
                    max = signif(sp$sigma * 1.5, 2),
                    step = 0.05)
    )
}

predationControl <- function(input, output, session, params, flags) {
    ## Adjust predation kernel ####
    observeEvent(
        list(input$beta, input$sigma),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_kernel)) {
                flags$sp_old_kernel <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "beta",
                              min = signif(input$beta / 2, 2),
                              max = signif(input$beta * 1.5, 2))
            updateSliderInput(session, "sigma",
                              min = signif(input$sigma / 2, 2),
                              max = signif(input$sigma * 1.5, 2))
            p@species_params[sp, "beta"]  <- input$beta
            p@species_params[sp, "sigma"] <- input$sigma
            p <- setPredKernel(p)
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)

    ## Adjust predation ####
    observeEvent(
        list(input$gamma, input$h, input$q),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_pred)) {
                flags$sp_old_pred <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "gamma",
                              min = signif(input$gamma / 2, 3),
                              max = signif(input$gamma * 1.5, 3))
            updateSliderInput(session, "h",
                              min = signif(input$h / 2, 2),
                              max = signif(input$h * 1.5, 2))
            p@species_params[sp, "gamma"] <- input$gamma
            p@species_params[sp, "h"]     <- input$h
            p@species_params[sp, "q"]     <- input$q
            p <- setSearchVolume(p)
            p <- setMaxIntakeRate(p)
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)

    ## Adjust growth exponent ####
    observeEvent(
        input$n,
        {
        p <- params()
        sp <- input$sp
        if (!identical(sp, flags$sp_old_n)) {
            flags$sp_old_n <- sp
            return()
        }
        # change h so that max intake rate at maturity stays the same
        p@species_params[[sp, "h"]] <- p@species_params[[sp, "h"]] *
            p@species_params[[sp, "w_mat"]] ^
            (p@species_params[[sp, "n"]] - input$n)
        p@species_params[[sp, "n"]] <- input$n
        h <- p@species_params[[sp, "h"]]
        updateSliderInput(session, "h",
                          value = h,
                          min = signif(h / 2, 2),
                          max = signif(h * 1.5, 2))
    },
    ignoreInit = TRUE)
}

fishingControlUI <- function(p, sp) {
    # If there are several gears, we only use the effort for the first.
    # If this is changed by the user, all efforts will be set the same.
    effort <- p@initial_effort[[1]]
    gp <- p@gear_params[p@gear_params$species == sp$species, ]
    if (nrow(gp) != 1) {
        showModal(modalDialog(
            title = "Invalid gear specification",
            HTML(paste0("Currently you can only use models where each ",
                        "species is caught by only one gear. In this model ",
                        sp$species, " is caught by ", nrow(gp), " gears.")),
            easyClose = TRUE
        ))
    }
    l1 <- list(tags$h3(tags$a(id = "fishing"), "Fishing"),
               sliderInput("catchability", "Catchability",
                           value = gp$catchability,
                           min = signif(max(0, gp$catchability / 2 - 1), 2),
                           max = signif(max(gp$catchability * 2, 2), 2),
                           step = 0.01),
               sliderInput("effort", "Effort",
                           value = effort,
                           min = 0,
                           max = signif((effort + 1) * 1.5, 2)))

    if (gp$sel_func == "knife_edge") {
        l1 <- c(l1, list(
            sliderInput("knife_edge_size", "knife_edge_size",
                        value = gp$knife_edge_size,
                        min = 1,
                        max = signif(gp$knife_edge_size * 2, 2),
                        step = 0.1)))
    } else if (gp$sel_func == "sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput("l50", "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput("ldiff", "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif(max(gp$l50 / 4, (gp$l50 - gp$l25)*1.1), 2),
                        step = 0.1)))
    } else if (gp$sel_func == "double_sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput("l50", "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput("ldiff", "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif(max(gp$l50 / 4, (gp$l50 - gp$l25)*1.1), 2),
                        step = 0.1),
            sliderInput("l50_right", "L50 right",
                        value = gp$l50_right,
                        min = 1,
                        max = signif(sp$l50_right * 2, 2),
                        step = 0.1),
            sliderInput("ldiff_right", "L50-L25 right",
                        value = gp$l25_right - gp$l50_right,
                        min = 0.1,
                        max = signif(max(gp$l50_right / 4,
                                         (gp$l25_right - gp$l50_right)*1.1), 2),
                        step = 0.1)
        ))
    }
    l1
}

fishingControl <- function(input, output, session, params, flags) {
    observeEvent(
        list(input$catchability,
             input$effort,
             input$knife_edge_size,
             input$l50,
             input$ldiff),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_fishing)) {
                flags$sp_old_fishing <- sp
                return()
            }
            gp_idx <- which(p@gear_params$species == sp)
            if (length(gp_idx) != 1) {
                showModal(modalDialog(
                    title = "Invalid gear specification",
                    HTML(paste0("Currently you can only use models where each ",
                                "species is caught by only one gear")),
                    easyClose = TRUE
                ))
            }

            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            p@gear_params[gp_idx, "catchability"]  <- input$catchability
            updateSliderInput(session, "catchability",
                              min = signif(max(input$catchability / 2 - 1, 0), 2),
                              max = signif(max(input$catchability * 2, 2), 2))
            updateSliderInput(session, "effort",
                              max = signif((input$effort + 1) * 1.5, 2))

            if (p@gear_params[gp_idx, "sel_func"] == "knife_edge") {
                updateSliderInput(session, "knife_edge_size",
                                  max = signif(input$knife_edge_size * 2, 2))
                p@gear_params[gp_idx, "knife_edge_size"]   <- input$knife_edge_size
            }
            if (p@gear_params[gp_idx, "sel_func"] == "sigmoid_length" ||
                p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
                updateSliderInput(session, "l50",
                                  max = signif(input$l50 * 2, 2))
                updateSliderInput(session, "ldiff",
                                  max = signif(input$l50 / 10, 2))
                p@gear_params[gp_idx, "l50"]   <- input$l50
                p@gear_params[gp_idx, "l25"]   <- input$l50 - input$ldiff
            }
            if (p@gear_params[gp_idx, "sel_func"] == "double_sigmoid_length") {
                p@gear_params[gp_idx, "l50_right"]   <- input$l50_right
                p@gear_params[gp_idx, "l25_right"]   <- input$l50_right + input$ldiff_right
                updateSliderInput(session, "l50_right",
                                  max = signif(input$l50_right * 2, 2))
                updateSliderInput(session, "ldiff_right",
                                  max = signif(input$l50_right / 10, 2))
            }

            p <- setFishing(p, initial_effort = input$effort)
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)
}

interactionControlUI <- function(p, sp) {
    l1 <- list(
        tags$h3(tags$a(id = "interaction"), "Prey interactions"),
        sliderInput("interaction_resource", "Resource",
                    value = sp$interaction_resource,
                    min = 0,
                    max = 1,
                    step = 0.001))
    for (i in p@species_params$species[!is.na(p@A)]) {
        inter_var <- paste0("inter_", i)
        l1 <- c(l1, list(
            sliderInput(inter_var, i,
                        value = p@interaction[sp$species, i],
                        min = 0,
                        max = 1,
                        step = 0.001)
        ))
    }
    l1
}

interactionControl <- function(input, output, session, params, flags) {
    observe({
        req(input$interaction_resource)
        p <- isolate(params())
        sp <- isolate(input$sp)
        # The following req()a are required, otherwise changes in the sliders
        # do not trigger. I am not sure why.
        for (i in p@species_params$species[!is.na(p@A)]) {
            inter_var <- paste0("inter_", i)
            req(input[[inter_var]])
        }
        if (!identical(sp, flags$sp_old_inter)) {
            flags$sp_old_inter <- sp
            return()
        }
        p@species_params[sp, "interaction_resource"] <-
            input$interaction_resource
        for (i in p@species_params$species[!is.na(p@A)]) {
            inter_var <- paste0("inter_", i)
            p@interaction[sp, i] <- input[[inter_var]]
        }
        tuneParams_update_species(sp, p, params)
    })
}
