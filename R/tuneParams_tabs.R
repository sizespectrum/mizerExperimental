# Hackiness to get past the 'no visible binding ... ' warning
utils::globalVariables(
    c("Age", "Biomass", "Yield", "Cause", "Kernel",
      "L_inf", "Legend", "Numbers", "Predator", "Size", "Species",
      "Type", "erepro", "value", "w_mat"))

spectraTabUI <- function(params, ...) {
    p <- isolate(params())
    has_bio <- ("biomass_observed" %in% names(p@species_params)) &&
      !all(is.na(p@species_params$biomass_observed))
    tl <- tagList(plotlyOutput("plotSpectra"))
    if (has_bio) {
        tl <- tagList(tl,
        popify(actionButton("scale_system", "Calibrate"),
               title = "Calibrate model",
               content = "Rescales the entire model so that the total of all observed biomasses agrees with the total of the model biomasses for the same species."),
        popify(actionButton("tune_egg_all", "Match"),
               title = "Match biomasses",
               content = "Moves the entire size spectrum for each species up or down to give the observed biomass value. It does that by multiplying the egg density by the ratio of observed biomass to model biomass. After that adjustment you should run to steady state by hitting the Steady button, after which the biomass will be a bit off again. You can repeat this process if you like to get ever closer to the observed biomass."))
    }
    tl <- tagList(tl,
        div(style = "display:inline-block;vertical-align:middle; width: 300px;",
            popify(sliderInput("scale_frgrd_by", 
                               "Scale background down by a factor of:",
                               value = 2, min = 0.5, max = 2, step = 0.05),
                   title = "Scaling the background",
                   content = "You can scale down the background in which the fish find themselves (the resource and any background species that your model may contain). This allows you to line up your community spectrum with the background spectrum. Choose the factor by which to scale and then hit the Go button. If you rescale by too large a factor the system may have difficulties finding the steady state. If that happens, just hit the Undo button and choose a smaller factor.")),
        popify(actionButton("scale_frgrd", "Go"),
               title = "Perform scaling of background",
               content = "The scaling factor is specified by the slider."))
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
    observeEvent(input$scale_frgrd, {
        p <- scaleDownBackground(params(), input$scale_frgrd_by)
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
      p@species_params[isolate(input$sp), "biomass_observed"] <-
        req(input$biomass_observed)
      p@species_params[isolate(input$sp), "biomass_cutoff"] <-
        req(input$biomass_cutoff)
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

growthTabUI <- function(...) {
    tagList(
        popify(radioButtons("all_growth", "Show:",
                            choices = c("All", "Selected species"),
                            selected = "All", inline = TRUE),
               title = "Switch views",
               content = "Select whether to view growth curves for all species or just for the selected species. You can also toggle this by double-clicking on the plot. Single-clicking on the plot changes the selected species without changing the view."),
        plotOutput("plotGrowthCurve",
                   click = "growth_click",
                   dblclick = "growth_dblclick"),
        textOutput("info"),
        plotlyOutput("plot_feeding_level"),
        uiOutput("k_vb_sel"),
        h1("Growth"),
        h2("Growth curves"),
        p("The upper plot shows growth curves: the size of an individual plotted against its age. Each growth curve plot shows the mizer growth curve in red and the von Bertalanffy growth curve in blue. The 'Show:' radio buttons let you choose to view the growth curves for all species at once or only that for the selected species. When viewing all growth curves you can double click on one of the graphs to select that species."),
        p("The von Bertalanffy growth curve is determined by the von Bertalanffy parameters 'w_inf', 'k_vb' and 't0' as well as the length-weight relationship parameters 'a' and 'b' in the species parameter data frame. You can change these manually using the input fields below the single-species growth curve plot. The mizer growth curves in contrast are determined by the energy available for growth from feeding, after metabolic cost and investment into reproduction are taken into account."),
        h2("Feeding level"),
        p("In the lower plot the thick lines shows the feeding level, which reflects the degree of satiation. At feeding level 1 an individual is totally satiated and does not feed any more, so is insensitive to changes in prey availability. The thin lines show the critical feeding level, which is the feeding level below which an individual can no longer meet its metabolic cost and starves. The actual feeding level should lie in between these two extremes."),
        h3("How to tune growth curves and feeding level"),
        p("The growth rate is strongly influenced by the predation rate coefficient 'gamma' and the maximum feeding rate parameter 'h'. Increasing 'gamma' will increase growth and increase feeding level. Increasing 'h' will also increase growth but decrease feeding level. So in practice you will change both to obtain the desired growth while maintaining an appropriate feeding level."),
        p("The growth rate is also influenced by losses to basic metabolism (parameters 'ks' and 'p') and activity (parameter 'k') and investment into", a("reproduction", href = "#reproduction"), ". For example changing the exponent 'm' that determines how investment into reproduction scales with size will affect the growth rate of large individuals.")
    )
}

growthTab <- function(input, output, session, params, logs, ...) {
    # Help button ----
    help_steps <- data.frame(
        element = c(NA),
        intro = c("This still needs to be written.")
    )
    observeEvent(
        input$growth_help,
        introjs(session, options = list(
            steps = help_steps)
        )
    )

    # Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$growth_click, {
        if (!is.null(input$growth_click$panelvar1) &&
            input$growth_click$panelvar1 != input$sp) {
            updateSelectInput(session, "sp",
                              selected = input$growth_click$panelvar1)
        }
    })
    # Double Click ----
    # See https://shiny.rstudio.com/articles/plot-interaction-advanced.html
    observeEvent(input$growth_dblclick, {
      if (!is.null(input$growth_dblclick$panelvar1) &&
          input$growth_dblclick$panelvar1 != input$sp) {
        updateSelectInput(session, "sp",
                          selected = input$growth_dblclick$panelvar1)
      }
      # Toggle between "All" and "Selected species"
      if (input$all_growth == "All") {
        updateRadioButtons(session, "all_growth",
                           selected = "Selected species")
      } else {
        updateRadioButtons(session, "all_growth",
                           selected = "All")
      }
    })

    # von Bertalanffy parameters ----
    output$k_vb_sel <- renderUI({
        req(input$sp)
        if (!input$all_growth == "All") {
            k_vb <- params()@species_params[input$sp, "k_vb"]
            t0 <- params()@species_params[input$sp, "t0"]
            a <- params()@species_params[input$sp, "a"]
            b <- params()@species_params[input$sp, "b"]
            list(
                div(style = "display:inline-block",
                    numericInput("k_vb", "Von Bertalanffy k", value = k_vb, width = "9em")),
                div(style = "display:inline-block",
                    numericInput("t0", "t_0", value = t0, width = "6em")),
                p("Parameters for length-weight relationship l = a w^b"),
                div(style = "display:inline-block",
                    numericInput("a", "a", value = a, width = "8em")),
                div(style = "display:inline-block",
                    numericInput("b", "b", value = b, width = "8em"))
            )
        }
    })

    ## Adjust von Bertalanffy ####
    observeEvent(
        list(input$k_vb, input$t0, input$a, input$b),
        {
        p <- isolate(params())
        p@species_params[isolate(input$sp), "k_vb"] <- input$k_vb
        p@species_params[isolate(input$sp), "t0"] <- input$t0
        p@species_params[isolate(input$sp), "a"] <- input$a
        p@species_params[isolate(input$sp), "b"] <- input$b
        params(p)
        },
        ignoreInit = TRUE)

    # Plot growth curves ----
    output$plotGrowthCurve <- renderPlot({
        p <- params()
        if (input$all_growth == "All") {
            plotGrowthCurves(p, species_panel = TRUE) +
            theme(text = element_text(size = 16))
        } else {
            plotGrowthCurves(p, species = input$sp) +
            theme(text = element_text(size = 16))
        }
    })

    # Plot feeding level ----
    output$plot_feeding_level <- renderPlotly({
        if (input$all_growth == "All") {
            plot <- plotFeedingLevel(params(), highlight = input$sp,
                                     include_critical = TRUE) +
              theme(text = element_text(size = 12))
        } else {
            plot <- plotFeedingLevel(params(), species = input$sp,
                                     include_critical = TRUE) +
              theme(text = element_text(size = 12))
        }
      ggplotly(plot, tooltip = c("Species", "w", "value"))
    })
}

reproTabUI <- function(...) {
    tagList(
        actionButton("repro_help", "Press for instructions"),
        plotlyOutput("plot_erepro"),
        plotlyOutput("plot_psi")
    )
}

reproTab <- function(input, output, session, params, logs, ...) {
    # Help button ----
    help_steps <- data.frame(
        element = c(NA),
        intro = c("This still needs to be written.")
    )
    observeEvent(
        input$repro_help,
        introjs(session, options = list(
            steps = help_steps)
        )
    )
    # erepro plot ----
    output$plot_erepro <- renderPlotly({
        p <- params()
        foreground <- !is.na(p@A)
        df <- data.frame(Species = factor(p@species_params$species[foreground],
                                          levels = p@species_params$species[foreground]),
                         value = p@species_params$erepro[foreground])
        ggplot(df, aes(x = Species, y = value)) +
            geom_col() + geom_hline(yintercept = 1, color = "red") +
            scale_y_log10(name = "Reproductive success") +
            theme(text = element_text(size = 12)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })

    # Plot psi ----
    output$plot_psi <- renderPlotly({
            p <- params()
            sp <- which.max(p@species_params$species == input$sp)
            w_min <- p@species_params$w_inf[sp] / 50
            sel <- p@w >= w_min & p@w <= p@species_params$w_inf[sp]
            df <- data.frame(Size = p@w[sel], value = p@psi[sp, sel])
            ggplot(df, aes(x = Size, y = value)) +
                geom_line(color = "blue") +
                geom_vline(xintercept = p@species_params[sp, "w_mat"],
                           linetype = "dashed") +
                geom_text(aes(x = p@species_params[sp, "w_mat"],
                              y = max(value * 0.8),
                              label = "\nw_mat"),
                          angle = 90) +
                geom_vline(xintercept = p@species_params[sp, "w_mat25"],
                           linetype = "dotted") +
                geom_text(aes(x = p@species_params[sp, "w_mat25"],
                              y = max(value * 0.6),
                              label = "\nw_mat25"),
                          angle = 90) +
                theme(text = element_text(size = 12)) +
                labs(x = "Size [g]", y = "Proportion of energy for reproduction")
    })
}

catchTabUI <- function(...) {
    tagList(
        # actionButton("tune_catch", "Tune catchability"),
        plotlyOutput("plotTotalYield"),
        popify(uiOutput("yield_sel"),
               title = "Input observed yield",
               content = "Allows you to update the observed yield for this species."),
        textOutput("yield_total"),
        plotlyOutput("plotCatchDist"),
        radioButtons("catch_x", "Show size in:",
                     choices = c("Weight", "Length"),
                     selected = "Length", inline = TRUE),
        h1("Total yield and size distribution of catch"),
        h2("Total yield"),
        p("The upper plot compares the yearly yield for each species in the model to the observed yield, if available."),
        p("The observed yield is taken from the 'yield_observed' column of the species parameter data frame. But if this is missing or needs to be changed you can do this with the input field below the upper plot. Note that this value is in grams/year."),
        h3("How to tune the yield"),
        p("To bring the yield of a species in the model in line with the observed value you can either change the abundance of large fish (for example by reducing their mortality from predation or the", a("background mortality", href = "#other"), "or you can change the", a("fishing parameters", href = "#fishing"), "."),
        h2("Size distribution of catch"),
        p("The lower plot shows the size distribution of the catch and again compares that to the observed size distribution, if available."),
        h3("How to tune size distribution"),
        p("To change the size distribution of catches you either need to change the size spectrum (for example by changing the mortality on large fish) or you need to adjust the ", a("fishing", href = "#fishing"), " selectivity curve by changing the 'L50' and 'L25' parameters.")
    )
}

#' Serve tab with catch data
#'
#' @param input Reactive holding the inputs
#' @param output Reactive holding the outputs
#' @param session Shiny session
#' @param params Reactive value holding a MizerParams object.
#' @param logs Environment holding the log of steady states.
#' @param catch Data frame holding binned observed catch data. The data can
#'   be binned either into length bins or weight bins. In the former case the data
#'   frame should have columns \code{length} and \code{dl} holding the start of
#'   the size bins in cm and the width of the size bins in cm respectively. In
#'   the latter case the data frame should have columns \code{weight} and
#'   \code{dw} holding the start of the size bins in grams and the width of the
#'   size bins in grams. The data frame also needs to have the columns
#'   \code{species} (the name of the species), \code{catch} (the number of
#'   individuals of a particular species caught in a size bin).
#' @param ... Not used.
catchTab <- function(input, output, session, params, logs,
                     catch = NULL, ...) {

    if (!is.null(catch)) {
        assert_that(
            is.data.frame(catch),
            "catch" %in% names(catch),
            "species" %in% names(catch),
            all(c("length", "dl") %in% names(catch)) |
                all(c("weight", "dw") %in% names(catch))
        )
    }

    # Tune catchability ----
    # The Catch Tune button calculates the ratio of observed and
    # model yield and then multiplies the catchability by that ratio. It
    # then runs the system to steady state.
    observeEvent(input$tune_catch, {
        p <- isolate(params())
        sp <- isolate(input$sp)
        sp_idx <- which.max(p@species_params$species == sp)
        gp_idx <- which(p@gear_params$species == sp)
        if (length(gp_idx) != 1) {
            showModal(modalDialog(
                title = "Invalid gear specification",
                HTML(paste0("Currently you can only use models where each ",
                            "species is caught by only one gear")),
                easyClose = TRUE
            ))
        }
        if ("yield_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$yield_observed[sp_idx]) &&
            p@species_params$yield_observed[sp_idx] > 0) {
            total <- sum(p@initial_n[sp_idx, ] * p@w * p@dw *
                             getFMort(p)[sp_idx, ])
            catchability <-
                p@gear_params$catchability[gp_idx] *
                p@species_params$yield_observed[sp_idx] / total
            updateSliderInput(session, "catchability",
                              value = catchability)
            # The above update of the slider will also trigger update of
            # the params object and the plot
        }
    })

    # Catch density for selected species ----
    output$plotCatchDist <- renderPlotly({
        req(input$sp)
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        p <- set_species_param_default(p, "a", 0.006)
        p <- set_species_param_default(p, "b", 3)
        a <- p@species_params[sp, "a"]
        b <- p@species_params[sp, "b"]

        # Check whether we have enough catch data for this species to plot it
        is_observed <- sum(catch$species == input$sp) > 3

        # To choose the range of sizes over which to plot we look at the range
        # of sizes for which a non-zero catch was observed. If no catch was
        # observed for the species, we use the range from w_mat/100 to w_inf.
        if (is_observed) {
            if ("length" %in% names(catch)) {
                l_min = min(catch$length[catch$species == input$sp])
                w_min = a * l_min ^ b
                l_max = max(catch$length[catch$species == input$sp])
                w_max = a * l_max ^ b
            } else {
                w_min = min(catch$weight[catch$species == input$sp])
                w_max = max(catch$weight[catch$species == input$sp])
            }
            w_min_idx <- sum(p@w < w_min)
            w_max_idx <- sum(p@w <= w_max)
        } else {
            w_min_idx <- sum(p@w < (p@species_params$w_mat[sp] / 100))
            w_max_idx <- sum(p@w <= p@species_params$w_inf[sp])
        }
        w_sel <- seq(w_min_idx, w_max_idx, by = 1)
        w <- p@w[w_sel]
        l = (p@w[w_sel] / a) ^ (1 / b)

        catch_w <- getFMort(p)[sp, w_sel] *
            p@initial_n[sp, w_sel]
        # We just want the distribution, so we rescale the density so its area is 1
        if (sum(catch_w) > 0) catch_w <- catch_w / sum(catch_w * p@dw[w_sel])
        # The catch density in l gets an extra factor of dw/dl
        catch_l <- catch_w * b * w / l
        df <- data.frame(w, l, catch_w, catch_l, Type = "Model catch")

        # We also include the abundance density because that helps to understand
        # the catch density
        catch_w <- p@initial_n[sp, w_sel]
        # We just want the distribution, so we rescale the density so its area is 1
        catch_w <- catch_w / sum(catch_w * p@dw[w_sel])
        # The catch density in l gets an extra factor of dw/dl
        catch_l <- catch_w * b * w / l
        abundance <- data.frame(w, l, catch_w, catch_l, Type = "Abundance")

        if (is_observed) {
            sel <- (catch$species == input$sp)
            if ("length" %in% names(catch)) {
                l <- catch$length[sel]
                dl <- catch$dl[sel]
                catch_l <- catch$catch[sel]
                # normalise to a density in l
                catch_l <- catch_l / sum(catch_l * dl)
                # To get the density in w we need to divide by dw/dl
                w <- a * l ^ b
                catch_w <- catch_l / b * l / w
            } else {
                w <- catch$weight[sel]
                dw <- catch$dw[sel]
                catch_w <- catch$catch[sel]
                # normalise to a density in w
                catch_w <- catch_w / sum(catch_w * dw)
                # To get the density in l we need to divide by dl/dw
                l <- (w / a)^(1/b)
                catch_l <- catch_w * b / l * w
            }
            df <- rbind(df, data.frame(w, l, catch_w, catch_l,
                                       Type = "Observed catch"))
        }
        # From the abundance only keep values that are no larger than
        # the maximum of the other shown densities.
        if (input$catch_x == "Weight") {
            abundance <- subset(abundance, catch_w < max(df$catch_w))
        } else {
            abundance <- subset(abundance, catch_l < max(df$catch_l))
        }
        # Add the abundance to the data frame last so that it shows up
        # last also in legend
        df <- rbind(df, abundance)

        if (input$catch_x == "Weight") {
            mat  <- p@species_params$w_mat[sp]
            pl <- ggplot(df) +
                geom_line(aes(x = w, y = catch_w, color = Type)) +
                geom_text(aes(x = mat, y = max(catch_w * 0.9), label = "\nMaturity"),
                          angle = 90) +
                labs(x = "Size [g]", y = "Normalised number density [1/g]")
        } else {
            mat <- (p@species_params$w_mat[sp] / a) ^ (1 / b)
            pl <- ggplot(df) +
                geom_line(aes(x = l, y = catch_l, color = Type)) +
                geom_text(aes(x = mat, y = max(catch_l * 0.9), label = "\nMaturity"),
                          angle = 90) +
                labs(x = "Size [cm]", y = "Normalised number density [1/cm]")
        }
        pl +
            geom_vline(xintercept = mat, linetype = "dotted")  +
            theme(text = element_text(size = 12)) +
            scale_colour_manual(values = c("Model catch" = "blue",
                                           "Observed catch" = "red",
                                           "Abundance" = "grey"))
    })

    # Total yield by species ----
    output$plotTotalYield <- renderPlotly({
        p <- params()
        no_sp <- length(p@species_params$species)
        observed <- p@species_params$yield_observed
        if (is.null(observed)) observed <- 0

        biomass <- sweep(p@initial_n, 2, p@w * p@dw, "*")
        yield_model <- rowSums(biomass * getFMort(p))

        # selector for foreground species
        foreground <- !is.na(p@A)
        foreground_indices <- (1:no_sp)[foreground]
        yield_model <- yield_model[foreground_indices]
        observed <- observed[foreground_indices]

        # Make sure species ordering is preserved in the plot
        species <- factor(p@species_params$species[foreground],
                          levels = p@species_params$species[foreground])

        df <- rbind(
            data.frame(Species = species,
                       Type = "Observed",
                       Yield = observed),
            data.frame(Species = species,
                       Type = "Model",
                       Yield = yield_model)
        )
        df <- df[df$Yield > 0, ]
        ggplot(df) +
            geom_col(aes(x = Species, y = Yield, fill = Type),
                     position = "dodge") +
            theme(text = element_text(size = 12)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            scale_y_continuous(name = "Yield [g/year]", trans = "log10",
                               breaks = log_breaks())
    })

    # Input field for observed yield ----
    output$yield_sel <- renderUI({
        p <- isolate(params())
        sp <- input$sp
        numericInput("yield_observed",
                     paste0("Observed total yield for ", sp, " [g/year]"),
                     value = p@species_params[sp, "yield_observed"])
    })

    # Adjust observed yield ----
    observeEvent(
        input$yield_observed,
        {
            p <- params()
            p@species_params[input$sp, "yield_observed"] <- input$yield_observed
            params(p)
        },
        ignoreInit = TRUE)

    # Output of model yield ----
    output$yield_total <- renderText({
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        total <- sum(p@initial_n[sp, ] * p@w * p@dw *
                         getFMort(p)[sp, ])
        paste("Model yield:", total, "g/year")
    })
}

ratesTabUI <- function(...) {
    tagList(
        radioButtons("axis", "x-axis scale:",
                     choices = c("Logarithmic", "Normal"),
                     selected = "Logarithmic", inline = TRUE),
        plotlyOutput("plotGrowth"),
        plotlyOutput("plotDeath")
    )
}

ratesTab <- function(input, output, session, params, logs, ...) {
    # Plot growth rates ----
    output$plotGrowth <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()

        max_w <- p@species_params[sp, "w_inf"]
        if (input$axis == "Logarithmic") {
            min_w <- p@species_params[sp, "w_min"]
        } else {
            min_w = p@species_params[sp, "w_mat"] / 10 # min(1, p@species_params[sp, "w_min"])
        }
        sel <- p@w >= min_w & p@w <= max_w
        len <- sum(sel)
        growth <- getEGrowth(p)[sp, sel]
        growth_and_repro <- getEReproAndGrowth(p)[sp, sel]
        metab <- p@metab[sp, sel]
        income <- growth_and_repro + metab
        repro <- growth_and_repro - growth
        df <- data.frame(
            w = rep(p@w[sel], 4),
            Type = c(rep("Growth", len),
                     rep("Income", len),
                     rep("Metabolic loss", len),
                     rep("Reproduction", len)),
            value = c(growth, income, metab, repro)
        )
        pl <- ggplot(df, aes(x = w, y = value, color = Type)) +
            geom_line() +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dotted") +
            geom_vline(xintercept = p@species_params[sp, "w_inf"],
                       linetype = "dotted") +
            theme(text = element_text(size = 12)) +
            labs(x = "Size [g]", y = "Rate [g/year]")  +
            geom_text(aes(x = p@species_params[sp, "w_mat"],
                          y = max(value * 0.2),
                          label = "\nMaturity"),
                      angle = 90)  +
            geom_text(aes(x = p@species_params[sp, "w_inf"],
                          y = max(value * 0.2),
                          label = "\nMaximum"),
                      angle = 90)
        if (input$axis == "Logarithmic") {
            pl <- pl + scale_x_log10()
        }
        pl
    })

    # Plot death rates ----
    output$plotDeath <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()

        max_w <- p@species_params[sp, "w_inf"]
        if (input$axis == "Logarithmic") {
            min_w <- p@species_params[sp, "w_min"]
        } else {
            min_w = p@species_params[sp, "w_mat"] / 10# min(1, p@species_params[sp, "w_min"])
        }
        sel <- p@w >= min_w & p@w <= max_w
        len <- sum(sel)
        df <- data.frame(
            w = rep(p@w[sel], 4),
            Type = c(rep("Total", len),
                     rep("Predation", len),
                     rep("Fishing", len),
                     rep("Background", len)),
            value = c(getMort(p)[sp, sel],
                      getPredMort(p)[sp, sel],
                      getFMort(p)[sp, sel],
                      p@mu_b[sp, sel])
        )
        pl <- ggplot(df, aes(x = w, y = value, color = Type)) +
            geom_line() +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dotted") +
            geom_vline(xintercept = p@species_params[sp, "w_inf"],
                       linetype = "dotted") +
            theme(text = element_text(size = 12)) +
            labs(x = "Size [g]", y = "Rate [1/year]")  +
            geom_text(aes(x = p@species_params[sp, "w_mat"],
                          y = max(value * 0.2),
                          label = "\nMaturity"),
                      angle = 90)  +
            geom_text(aes(x = p@species_params[sp, "w_inf"],
                          y = max(value * 0.2),
                          label = "\nMaximum"),
                      angle = 90)
        if (input$axis == "Logarithmic") {
            pl <- pl + scale_x_log10()
        }
        pl
    })
}

dietTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_diet"),
        plotlyOutput("plot_prey"),
        uiOutput("pred_size_slider")
    )
}

dietTab <- function(input, output, session, params, logs, ...) {

    # Plot diet ----
    output$plot_diet <- renderPlotly({
        req(input$sp)
        plotDiet(params(), input$sp)
    })
    
    # Plot prey ----
    output$plot_prey <- renderPlotly({
      p <- params()
      sp <- which.max(p@species_params$species == input$sp)
      x <- log(p@w_full)
      dx <- x[2] - x[1]
      wp <- 10^req(input$pred_size)
      wp_idx <- sum(p@w <= wp)
      # Calculate total community abundance
      # Todo: take interaction matrix into account
      fish_idx <- (length(p@w_full) - length(p@w) + 1):length(p@w_full)
      total_n <- p@initial_n_pp
      total_n[fish_idx] <- total_n[fish_idx] +
        p@interaction[sp, ] %*% p@initial_n
      totalx <- total_n * p@w_full
      #totalx <- totalx / sum(totalx * dx)
      phix <- getPredKernel(p)[sp, wp_idx, ]
      pr <- totalx * phix
      br <- pr * p@w_full
      # convert to proportions
      phix <- phix / sum(phix * dx)
      pr <- pr / sum(pr * dx)
      br <- br / sum(br * dx)
      df <- tibble::tibble(w = p@w_full,
                           Kernel = phix,
                           Biomass = br,
                           Numbers = pr) %>%
        tidyr::gather(Type, value, Kernel, Biomass, Numbers)
      ggplot(df) +
        geom_line(aes(w, value, color = Type)) +
        labs(x = "Weight [g]", y = "Density") +
        geom_point(aes(x = wp, y = 0), size = 4, colour = "blue") +
        scale_x_log10()
    })
    
    # Prey size slider ----
    output$pred_size_slider <- renderUI({
      p <- isolate(params())
      sp <- which.max(p@species_params$species == input$sp)
      sliderInput("pred_size", "log_10 predator size",
                  value = signif(log10(p@species_params$w_mat[sp]), 2),
                  min = signif(log10(p@species_params$w_min[sp]), 2),
                  max = signif(log10(p@species_params$w_inf[sp]), 2),
                  step = 0.2,
                  width = "80%",
                  animate = animationOptions(loop = TRUE))
    })
}

deathTabUI <- function(...) {
    tagList(
        radioButtons("death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
        plotlyOutput("plot_pred")
    )
}

deathTab <- function(input, output, session, params, logs, ...) {

    # Plot predators ----
    output$plot_pred <- renderPlotly({
        req(input$sp)
        sp <- input$sp
        p <- params()
        species <- factor(p@species_params$species,
                          levels = p@species_params$species)
        fish_idx_full <- (p@w_full >= p@species_params[sp, "w_min"]) &
            (p@w_full <= p@species_params[sp, "w_inf"])
        fish_idx <- (p@w >= p@species_params[sp, "w_min"]) &
            (p@w <= p@species_params[sp, "w_inf"])
        pred_rate <- p@interaction[, sp] *
            getPredRate(p)[, fish_idx_full]
        fishing <- getFMort(p)[sp, fish_idx]
        total <- colSums(pred_rate) + p@mu_b[sp, fish_idx] + fishing
        ylab <- "Death rate [1/year]"
        background <- p@mu_b[sp, fish_idx]
        if (input$death_prop == "Proportion") {
            pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
            background <- background / total
            fishing <- fishing / total
            ylab <- "Proportion of all death"
        }
        # Make data.frame for plot
        plot_dat <-
            rbind(
                data.frame(value = background,
                           Cause = "Background",
                           w = p@w[fish_idx]),
                data.frame(value = fishing,
                           Cause = "Fishing",
                           w = p@w[fish_idx]),
                data.frame(value = c(pred_rate),
                           Cause = species,
                           w = rep(p@w[fish_idx], each = dim(pred_rate)[[1]]))
            )
        ggplot(plot_dat) +
            geom_area(aes(x = w, y = value, fill = Cause)) +
            scale_x_log10() +
            labs(x = "Size [g]", y = ylab) +
            scale_fill_manual(values = p@linecolour)
    })
}

resourceTabUI <- function(...) {
    tagList(
        plotlyOutput("plot_resource_pred"),
        radioButtons("resource_death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE),
        plotOutput("plot_resource", width = "84%")
    )
}

resourceTab <- function(input, output, session, params, logs, ...) {

    # Plot resource ----
    output$plot_resource <- renderPlot({
        p <- params()
        select <- (p@cc_pp > 0)
        plot_dat <- data.frame(
            Size = p@w_full[select],
            value = p@initial_n_pp[select] / p@cc_pp[select]
        )
        ggplot(plot_dat) +
            geom_line(aes(Size, value)) +
            scale_x_log10("Resource size [g]") +
            ylab("Proportion of carrying capacity") +
            theme(text = element_text(size = 12))
    })

    # Plot resource predators ----
    output$plot_resource_pred <- renderPlotly({
        p <- params()
        species <- factor(p@species_params$species,
                          levels = p@species_params$species)
        select <- (p@cc_pp > 0)
        pred_rate <- p@species_params$interaction_resource *
            getPredRate(p)[, select]
        total <- colSums(pred_rate)
        ylab <- "Death rate [1/year]"
        if (input$resource_death_prop == "Proportion") {
            pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
            ylab = "Proportion of predation"
        }
        # Make data.frame for plot
        plot_dat <- data.frame(
            value = c(pred_rate),
            Predator = species,
            w = rep(p@w_full[select], each = dim(pred_rate)[[1]]))
        ggplot(plot_dat) +
            geom_area(aes(x = w, y = value, fill = Predator)) +
            scale_x_log10("Resource size [g]") +
            ylab(ylab) +
            scale_fill_manual(values = p@linecolour) +
            theme(text = element_text(size = 12))
    })
}

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
