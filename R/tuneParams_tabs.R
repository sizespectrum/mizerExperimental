# Hackiness to get past the 'no visible binding ... ' warning
utils::globalVariables(
    c("Age", "Biomass", "Yield", "Cause", "Kernel",
      "L_inf", "Legend", "Numbers", "Predator", "Size", "Species",
      "Type", "erepro", "value", "w_mat"))


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
#         tuneParams_update_params(p, params)
#     })
# 
#     # Plot biomass distribution
#     output$plotBiomassDist <- renderPlotly({
#         req(input$sp, input$biomass_cutoff, input$biomass_observed)
#         sp <- input$sp
#         p <- params()
#         biomass <- cumsum(p@initial_n[sp, ] * p@w * p@dw)
# 
#         max_w <- p@species_params[sp, "w_max"]
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
#         tuneParams_add_to_logs(logs, p, params)
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
#             tuneParams_add_to_logs(logs, p, params)
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
#         tuneParams_update_params(p, params)
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
#       tuneParams_update_params(p, params)
#     })
# }

