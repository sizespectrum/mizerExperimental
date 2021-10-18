# Hackiness to get past the 'no visible binding ... ' warning
utils::globalVariables(
    c("Age", "Biomass", "Yield", "Cause", "Kernel",
      "L_inf", "Legend", "Numbers", "Predator", "Size", "Species",
      "Type", "erepro", "value", "w_mat"))

spectraTabUI <- function(params, ...) {
    p <- isolate(params())
    has_bio <- ("biomass_observed" %in% names(p@species_params)) &&
      !all(is.na(p@species_params$biomass_observed))
    tl <- tagList(plotlyOutput("plotSpectra"),
        div(style = "display:inline-block;vertical-align:middle; width: 300px;",
            popify(sliderInput("scale_bkgrd_by", 
                               "Scale background down by a factor of:",
                               value = 1, min = 0.5, max = 2, step = 0.1),
                   title = "Scaling the background",
                   content = "You can scale down the background in which the fish find themselves (the resource and any background species that your model may contain). This allows you to line up your community spectrum with the background spectrum. Simply click on the factor by which to scale. Afterwards you will want to run to steady state. If you rescale by too large a factor the system may have difficulties finding the steady state. If that happens, just hit the Undo button and choose a smaller factor.")),
        )
    
    if (has_bio) {
      tl <- tagList(tl,
                    popify(actionButton("scale_system", "Calibrate"),
                           title = "Calibrate model",
                           content = "Rescales the entire model so that the total of all observed biomasses agrees with the total of the model biomasses for the same species."),
                    popify(actionButton("tune_egg_all", "Match"),
                           title = "Match biomasses",
                           content = "Moves the entire size spectrum for each species up or down to give the observed biomass value. It does that by multiplying the egg density by the ratio of observed biomass to model biomass. After that adjustment you should run to steady state by hitting the Steady button, after which the biomass will be a bit off again. You can repeat this process if you like to get ever closer to the observed biomass."))
    }
    if (anyNA(p@A)) {
        tl <- tagList(tl, 
        popify(actionButton("retune_background", "Adj bs"),
             title = "Adjust background species",
             content = "Adjust the biomasses of the background species in such a way that the total spectrum aligns well with the resource spectrum. Background species that are no longer needed because forground species have taken their place in the community spectrum are automatically removed."),
        popify(actionButton("remove_background", "Rem bs"),
               title = "Remove background species",
               content = "Remove all background species."))
    }
    if (has_bio) {
        tl <- tagList(tl,
        plotOutput("plotTotalBiomass",
                   click = "biomass_click",
                   dblclick = "tune_egg"),
        uiOutput("biomass_sel"))
    }
    tl <- tagList(tl,
        h1("Size spectra"),
        p("This tab shows the biomass size spectra of the individual fish species and of the resource, as well as the total size spectrum (in black)."),
        p("This plot, as well as those on other tabs, is interactive in various",
          "ways. For example you can remove individual species from the plot by",
          "clicking on their name in the legend. Hovering over the lines pops",
          "up extra information. You can zoom into a portion of the plot by",
          "dragging a rectangle with the mouse while holding the left mouse",
          "button down."),
        p("Remember that after any adjustment you make in this app, you need",
          "to hit the 'Steady' button before you will see the full ",
          "multi-species consequences of the change."),
    )
}

spectraTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {

    ## Plot spectra ####
    output$plotSpectra <- renderPlotly({
        # if (input$binning == "Logarithmic") {
            power <- 2
        # } else {
        #     power <- 1
        # }
        plot <- plotSpectra(params(), power = power, highlight = input$sp, 
                            total = TRUE) +
            theme(text = element_text(size = 12))
        ggplotly(plot, tooltip = c("Species", "w", "value"))
    })

    ## Scale ####
    observeEvent(input$scale_bkgrd_by, {
        p <- scaleDownBackground(params(), input$scale_bkgrd_by)
        updateSliderInput(session, "scale_bkgrd_by", value = 1)
        params(p)
    })

    ## Retune background ####
    observeEvent(input$retune_background, {
        p <- adjustBackgroundSpecies(params())
        # For now we won't disable the button because of a bug in shinyBS
        # whereby the tooltip stays forever on disabled buttons.
        # if (!anyNA(p@A)) {
        #     shinyjs::disable("retune_background")
        #     removeTooltip(session, "retune_background")
        # }
        params(p)
    })
    
    ## Remove background ####
    observeEvent(input$remove_background, {
      p <- removeBackgroundSpecies(params())
      # For now we won't disable the button because of a bug in shinyBS
      # whereby the tooltip stays forever on disabled buttons.
      # if (!anyNA(p@A)) {
      #   removeTooltip(session, "remove_background")
      #   shinyjs::disable("remove_background")
      #   shinyjs::disable("retune_background")
      # }
      params(p)
    })
    
    # Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$biomass_click, {
      if (is.null(input$biomass_click$x)) return()
      lvls <- input$biomass_click$domain$discrete_limits$x
      sp <- lvls[round(input$biomass_click$x)]
      if (sp != input$sp) {
        updateSelectInput(session, "sp",
                          selected = sp)
      }
    })
    
    # Plot total biomass ----
    output$plotTotalBiomass <- renderPlot({
      plotBiomassVsSpecies(params()) +
        theme(text = element_text(size = 18))
    })
    
    
    # Biomass selector ----
    output$biomass_sel <- renderUI({
      sp <- input$sp
      p <- isolate(params())
      species_params <- p@species_params[sp, ]
      if (is.null(species_params$biomass_observed) ||
          is.na(species_params$biomass_observed)) {
        species_params$biomass_observed <- 0
      }
      if (is.null(species_params$biomass_cutoff) ||
          is.na(species_params$biomass_cutoff)) {
        species_params$biomass_cutoff <- 0
      }
      list(
        div(style = "display:inline-block",
            numericInput("biomass_observed",
                         paste0("Observed biomass for ", sp),
                         value = species_params$biomass_observed)),
        div(style = "display:inline-block",
            numericInput("biomass_cutoff", "Lower cutoff",
                         value = species_params$biomass_cutoff))
      )
    })
    
    # Adjust biomass observed ----
    observe({
      p <- isolate(params())
      biomass_observed <- req(input$biomass_observed)
      if (biomass_observed == 0) biomass_observed <- NA
      p@species_params[isolate(input$sp), "biomass_observed"] <-
        biomass_observed
      params(p)
    })
    
    # Rescale model ----
    observeEvent(input$scale_system, {
      # Rescale so that the model matches the total observed biomass
      p <- calibrateBiomass(params())
      params(p)
      tuneParams_add_to_logs(logs, p)
      # Trigger an update of sliders
      trigger_update(runif(1))
    })
    
    # to make biomass of current species agree with observation
    observeEvent(input$rescale, {
      p <- params()
      sp <- which.max(p@species_params$species == input$sp)
      if ("biomass_observed" %in% names(p@species_params) &&
          !is.na(p@species_params$biomass_observed[[sp]]) &&
          p@species_params$biomass_observed[[sp]] > 0) {
        cutoff <- p@species_params$biomass_cutoff[[sp]]
        if (is.null(cutoff) || is.na(cutoff)) {
          cutoff <- 0
        }
        biomass_observed <- p@species_params$biomass_observed[[sp]]
        biomass_model <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
        scale_by <- biomass_observed / biomass_model
        p <- scaleModel(p, factor = scale_by)
        params(p)
        tuneParams_add_to_logs(logs, p)
        # Trigger an update of sliders
        trigger_update(runif(1))
      }
    })
    
    # Tune egg density ----
    # The "Tune egg density" button calculates the ratio of observed and
    # model biomass and then multiplies the egg density by that ratio.
    observeEvent(input$tune_egg, {
      if (is.null(input$tune_egg$x)) return()
      lvls <- input$tune_egg$domain$discrete_limits$x
      sp <- lvls[round(input$tune_egg$x)]
      p <- params()
      sp_idx <- which.max(p@species_params$species == sp)
      if ("biomass_observed" %in% names(p@species_params) &&
          !is.na(p@species_params$biomass_observed[[sp_idx]]) &&
          p@species_params$biomass_observed[[sp_idx]] > 0) {
        cutoff <- p@species_params$biomass_cutoff[[sp_idx]]
        if (is.null(cutoff) || is.na(cutoff)) {
          cutoff <- 0
        }
        total <- sum((p@initial_n[sp_idx, ] * p@w * p@dw)[p@w >= cutoff])
        factor <- p@species_params$biomass_observed[[sp_idx]] / total
        p@initial_n[sp_idx, ] <- p@initial_n[sp_idx, ] * factor
      }
      params(p)
      if (sp == input$sp) {
        n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
        updateSliderInput(session, "n0",
                          value = n0,
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 10, 3))
      } else {
        updateSelectInput(session, "sp", selected = sp)
      }
    })
    observeEvent(input$tune_egg_all, {
      # I just copied and pasted the code form above into a loop.
      # I am sure this could be improved.
      p <- params()
      sp_sel <- which.max(p@species_params$species == input$sp)
      for (sp in seq_along(p@species_params$species)) {
        if ("biomass_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$biomass_observed[[sp]]) &&
            p@species_params$biomass_observed[[sp]] > 0) {
          cutoff <- p@species_params$biomass_cutoff[[sp]]
          if (is.null(cutoff) || is.na(cutoff)) {
            cutoff <- 0
          }
          total <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
          n0_old <- p@initial_n[sp, p@w_min_idx[[sp]]]
          n0 <- n0_old * p@species_params$biomass_observed[[sp]] / total
          # rescale abundance to new egg density
          p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 / n0_old
          
          if (sp == sp_sel) {
            updateSliderInput(session, "n0",
                              value = n0,
                              min = signif(n0 / 10, 3),
                              max = signif(n0 * 10, 3))
          }
        }
      }
      params(p)
    })
}

# The following is outdated since the biomass plot has moved to the Spectra tab
# The plot of the biomass distribution may need a new home.
# biomassTabUI <- function(...) {
#     tagList(
#         # actionButton("biomass_help", "Press for instructions"),
#         plotOutput("plotTotalBiomass",
#                    click = "biomass_click",
#                    dblclick = "tune_egg"),
#         actionButton("scale_system", "Calibrate scale"),
#         actionButton("tune_egg_all", "Adjust all"),
#         uiOutput("biomass_sel"),
#         h1("Biomass"),
#         p("This panel compares the biomass aof the model to",
#           "the observed values, where available. It also provides tools for",
#           "changing the model biomass."),
#         p("The 'Set scale' button rescales the entire model so that the",
#           "biomass for the selected species agrees perfectly with the observed",
#           "value."),
#         p("The 'Tune species' button attempts to move the entire size",
#           "spectrum for the selected species up or down to give the observed",
#           "biomass value. It does that by multiplying the egg density by the",
#           "ratio of observed biomass to model biomass. After that adjustment",
#           "you should hit the 'Steady' button to allow for multispecies",
#           "effects, after which the biomass will be a bit off again. You can",
#           "repeat this process if you like to get even closer to the observed",
#           "biomass."),
#         p("The 'Tune all' button does the same as the `Tune species` button",
#           "but for all species at once."),
#         p("The values for the observed biomass is taken from the",
#           "'biomass_observed' column in the species parameter data frame.",
#           "If these are missing or need changing",
#           "then you can enter values by hand using the input fields below the",
#           "plots."),
#         p("Usually the observed values are only for individuals above a lower",
#           "cutoff size. This cutoff size is recorded in the 'biomass_cutoff'",
#           "column of the species parameter data frame. Again you can enter",
#           "these also manually using the input field below the plots. For",
#           "example if you have observed values for the spawning stock biomass,",
#           "then you would set the cutoff size to the maturity size.")
#     )
# }
# 
# biomassTab <- function(input, output, session,
#                        params, logs, trigger_update, ...) {
#     # Help button 
#     help_steps <- data.frame(
#         element = c(NA),
#         intro = c("This still needs to be written.")
#     )
#     observeEvent(
#         input$biomass_help,
#         introjs(session, options = list(
#             steps = help_steps)
#         )
#     )
#     
#     # Click 
#     # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
#     observeEvent(input$biomass_click, {
#       if (is.null(input$biomass_click$x)) return()
#       lvls <- input$biomass_click$domain$discrete_limits$x
#       sp <- lvls[round(input$biomass_click$x)]
#       if (sp != input$sp) {
#         updateSelectInput(session, "sp",
#                           selected = sp)
#       }
#     })
# 
#     # Plot total biomass 
#     output$plotTotalBiomass <- renderPlot({
#         p <- params()
#         no_sp <- length(p@species_params$species)
#         cutoff <- p@species_params$biomass_cutoff
#         # When no cutoff known, set it to 0
#         if (is.null(cutoff)) cutoff <- 0
#         cutoff[is.na(cutoff)] <- 0
#         observed <- p@species_params$biomass_observed
#         if (is.null(observed)) observed <- 0
# 
#         # selector for foreground species
#         foreground <- !is.na(p@A)
#         foreground_indices <- (1:no_sp)[foreground]
#         biomass_model <- foreground_indices  # create vector of right length
#         for (i in seq_along(foreground_indices)) {
#             sp <- foreground_indices[i]
#             biomass_model[i] <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff[[sp]]])
#         }
#         species <- factor(p@species_params$species[foreground],
#                           levels = p@species_params$species[foreground])
#         df <- rbind(
#             data.frame(Species = species,
#                        Type = "Observed Biomass [g]",
#                        Biomass = observed[foreground]),
#             data.frame(Species = species,
#                        Type = "Model Biomass [g]",
#                        Biomass = biomass_model)
#         )
#         # Get rid of "Observed" entries for species without 
#         # observations (where we have set observed = 0)
#         df <- df[df$Biomass > 0, ] 
#         
#         ggplot(df) +
#             geom_point(aes(x = Species, y = Biomass, colour = Type),
#                        size = 8, alpha = 0.5) +
#             scale_y_continuous(name = "Biomass [g]", trans = "log10",
#                                breaks = log_breaks()) +
#             theme_grey(base_size = 12) +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#     })
# 
# 
#     # Biomass selector
#     output$biomass_sel <- renderUI({
#         sp <- req(input$sp)
#         p <- isolate(params())
#         species_params <- p@species_params[sp, ]
#         if (is.null(species_params$biomass_observed) ||
#             is.na(species_params$biomass_observed)) {
#             species_params$biomass_observed <- 0
#         }
#         if (is.null(species_params$biomass_cutoff) ||
#             is.na(species_params$biomass_cutoff)) {
#             species_params$biomass_cutoff <- 0
#         }
#         list(
#             div(style = "display:inline-block",
#                 numericInput("biomass_observed",
#                              paste0("Observed biomass for ", sp),
#                              value = species_params$biomass_observed)),
#             div(style = "display:inline-block",
#                 numericInput("biomass_cutoff", "Lower cutoff",
#                              value = species_params$biomass_cutoff))
#         )
#     })
# 
#     # Adjust biomass observed
#     observe({
#         p <- isolate(params())
#         p@species_params[isolate(input$sp), "biomass_observed"] <-
#             req(input$biomass_observed)
#         p@species_params[isolate(input$sp), "biomass_cutoff"] <-
#             req(input$biomass_cutoff)
#         params(p)
#     })
# 
#     # Plot biomass distribution
#     output$plotBiomassDist <- renderPlotly({
#         req(input$sp, input$biomass_cutoff, input$biomass_observed)
#         sp <- input$sp
#         p <- params()
#         biomass <- cumsum(p@initial_n[sp, ] * p@w * p@dw)
# 
#         max_w <- p@species_params[sp, "w_inf"]
#         min_w <- p@species_params[sp, "w_min"]
#         sel <- p@w >= min_w & p@w <= max_w
#         df <- data.frame(Size = p@w[sel], Biomass = biomass[sel])
#         df <- df[df$Biomass > 0, ]
#         pl <- ggplot(df, aes(x = Size, y = Biomass)) +
#             geom_line(color = "blue") + scale_x_log10() +
#             geom_vline(xintercept = p@species_params[sp, "w_mat"],
#                        linetype = "dotted") +
#             theme_grey(base_size = 12) +
#             labs(x = "Size [g]", y = "Cummulative biomass")  +
#             geom_text(aes(x = p@species_params[sp, "w_mat"],
#                           y = max(Biomass * 0.2),
#                           label = "\nMaturity"),
#                       angle = 90)
#         if (input$biomass_observed) {
#             cutoff_idx <- which.max(p@w >= input$biomass_cutoff)
#             target <- input$biomass_observed + biomass[[cutoff_idx]]
#             pl <- pl +
#                 geom_hline(yintercept = biomass[[cutoff_idx]]) +
#                 geom_vline(xintercept = input$biomass_cutoff) +
#                 geom_hline(yintercept = target, color = "green")
#         }
#         pl
#     })
# 
#     # Rescale model
#     observeEvent(input$scale_system, {
#         # Rescale so that the model matches the total observed biomass
#         p <- params() 
#         if ((!("biomass_observed" %in% names(p@species_params))) ||
#               all(is.na(p@species_params$biomass_observed))) {
#             return()
#         }
#         cutoff <- p@species_params$biomass_cutoff
#         # When no cutoff known, set it to 0
#         if (is.null(cutoff)) cutoff <- 0
#         cutoff[is.na(cutoff)] <- 0
#         observed <- p@species_params$biomass_observed
#         observed_total <- sum(observed, na.rm = TRUE)
#         sp_observed <- which(!is.na(observed))
#         model_total <- 0
#         for (sp_idx in sp_observed) {
#           model_total <- 
#             model_total + 
#             sum((p@initial_n[sp_idx, ] * p@w * p@dw)[p@w >= cutoff[[sp_idx]]])
#         }
#         p <- scaleModel(p, factor = observed_total / model_total)
#         params(p)
#         tuneParams_add_to_logs(logs, p)
#         # Trigger an update of sliders
#         trigger_update(runif(1))
#     })
#     
#     # to make biomass of current species agree with observation
#     observeEvent(input$rescale, {
#         p <- params()
#         sp <- which.max(p@species_params$species == input$sp)
#         if ("biomass_observed" %in% names(p@species_params) &&
#             !is.na(p@species_params$biomass_observed[[sp]]) &&
#             p@species_params$biomass_observed[[sp]] > 0) {
#             cutoff <- p@species_params$biomass_cutoff[[sp]]
#             if (is.null(cutoff) || is.na(cutoff)) {
#                 cutoff <- 0
#             }
#             biomass_observed <- p@species_params$biomass_observed[[sp]]
#             biomass_model <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
#             scale_by <- biomass_observed / biomass_model
#             p <- scaleModel(p, factor = scale_by)
#             params(p)
#             tuneParams_add_to_logs(logs, p)
#             # Trigger an update of sliders
#             trigger_update(runif(1))
#         }
#     })
# 
#     # Tune egg density 
#     # The "Tune egg density" button calculates the ratio of observed and
#     # model biomass and then multiplies the egg density by that ratio.
#     observeEvent(input$tune_egg, {
#         if (is.null(input$tune_egg$x)) return()
#         lvls <- input$tune_egg$domain$discrete_limits$x
#         sp <- lvls[round(input$tune_egg$x)]
#         p <- params()
#         sp_idx <- which.max(p@species_params$species == sp)
#         if ("biomass_observed" %in% names(p@species_params) &&
#                 !is.na(p@species_params$biomass_observed[[sp_idx]]) &&
#                 p@species_params$biomass_observed[[sp_idx]] > 0) {
#             cutoff <- p@species_params$biomass_cutoff[[sp_idx]]
#             if (is.null(cutoff) || is.na(cutoff)) {
#                 cutoff <- 0
#             }
#             total <- sum((p@initial_n[sp_idx, ] * p@w * p@dw)[p@w >= cutoff])
#             factor <- p@species_params$biomass_observed[[sp_idx]] / total
#             p@initial_n[sp_idx, ] <- p@initial_n[sp_idx, ] * factor
#         }
#         params(p)
#         if (sp == input$sp) {
#             n0 <- p@initial_n[sp_idx, p@w_min_idx[[sp_idx]]]
#             updateSliderInput(session, "n0",
#                               value = n0,
#                               min = signif(n0 / 10, 3),
#                               max = signif(n0 * 10, 3))
#         } else {
#             updateSelectInput(session, "sp", selected = sp)
#         }
#     })
#     observeEvent(input$tune_egg_all, {
#       # I just copied and pasted the code form above into a loop.
#       # I am sure this could be improved.
#       p <- params()
#       sp_sel <- which.max(p@species_params$species == input$sp)
#       for (sp in seq_along(p@species_params$species)) {
#         if ("biomass_observed" %in% names(p@species_params) &&
#             !is.na(p@species_params$biomass_observed[[sp]]) &&
#             p@species_params$biomass_observed[[sp]] > 0) {
#           cutoff <- p@species_params$biomass_cutoff[[sp]]
#           if (is.null(cutoff) || is.na(cutoff)) {
#             cutoff <- 0
#           }
#           total <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
#           n0_old <- p@initial_n[sp, p@w_min_idx[[sp]]]
#           n0 <- n0_old * p@species_params$biomass_observed[[sp]] / total
#           # rescale abundance to new egg density
#           p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 / n0_old
#           
#           if (sp == sp_sel) {
#             updateSliderInput(session, "n0",
#                               value = n0,
#                               min = signif(n0 / 10, 3),
#                               max = signif(n0 * 10, 3))
#           }
#         }
#       }
#       params(p)
#     })
# }

























simTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_sim")
    )
}

simTab <- function(input, output, session, params, logs, ...) {

    ## Plot run to steady ####
    output$plot_sim <- renderPlotly({
        sim <- tuneParams_run_steady(params(), return_sim = TRUE,
                          params = params, logs = logs,
                          session = session, input = input)
        plotlyBiomass(sim)
    })
}

# stomachTabUI <- function() {
#     tagList(
#         plotlyOutput("plot_steady")
#     )
# }
# #' Serve tab with stomach data
# #'
# #' @inheritParams catchTab
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
# stomachTab <- function(input, output, session, params, logs, stomach, ...) {
#     if (!is.null(stomach)) {
#         assert_that(
#             is.data.frame(stomach),
#             "wprey" %in% names(stomach),
#             "wpredator" %in% names(stomach),
#             "species" %in% names(stomach)
#         )
#         if (!("Nprey" %in% names(stomach))) stomach$Nprey <- 1
#         stomach <- stomach %>%
#             mutate(logpredprey = log(wpredator / wprey),
#                    weight = Nprey / sum(Nprey),
#                    weight_biomass = Nprey * wprey / sum(Nprey * wprey),
#                    weight_kernel = Nprey / wprey^(1 + alpha - lambda),
#                    weight_kernel = weight_kernel / sum(weight_kernel))
#     }
#
#     # Plot stomach content ----
#     output$plot_stomach <- renderPlot({
#         req(input$sp)
#         p <- params()
#         sp <- which.max(p@species_params$species == input$sp)
#
#         df <- tibble(
#             x = x,
#             Kernel = double_sigmoid(
#                 x,
#                 p_l = input$p_l,
#                 s_l = input$s_l,
#                 p_r = input$p_r,
#                 s_r = input$s_r,
#                 ex = input$ex)) %>%
#             mutate(Numbers = Kernel / exp((1 + alpha - lambda) * x),
#                    Biomass = Numbers / exp(x),
#                    Kernel = Kernel / sum(Kernel) / dx,
#                    Numbers = Numbers / sum(Numbers) / dx,
#                    Biomass = Biomass / sum(Biomass) / dx) %>%
#             gather(key = Type, value = "Density",
#                    Numbers, Biomass)
#
#         pl + geom_line(data = df,
#                        aes(x, Density, colour = Type),
#                        size = 3)
#         st <- stomach %>%
#             filter(species == input$sp)
#     })
# }
