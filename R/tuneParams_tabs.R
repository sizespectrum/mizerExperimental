spectraTabUI <- function() {
    tagList(
        plotlyOutput("plotSpectra"),
        radioButtons("binning", "Binning:",
                     choices = c("Logarithmic", "Constant"),
                     selected = "Logarithmic", inline = TRUE),
        actionButton("scale", "Scale by 2x"),
        actionButton("retune_background",
                     "Retune background")
    )
}

spectraTab <- function(input, output, session, params, logs, ...) {

    ## Plot spectra ####
    output$plotSpectra <- renderPlotly({
        if (input$binning == "Logarithmic") {
            power <- 2
        } else {
            power <- 1
        }
        plotSpectra(params(), power = power, highlight = input$sp, total = TRUE) +
            theme_grey(base_size = 12)
    })

    ## Scale ####
    observeEvent(input$scale, {
        tuneParams_run_steady(rescaleAbundance(params(), factor = 2),
                   params = params, logs = logs, session = session)
    })

    ## Retune background ####
    observeEvent(input$retune_background, {
        p <- retuneBackground(params())
        if (!anyNA(p@A)) {
            shinyjs::disable("retune_background")
        }
        params(p)
    })
}

biomassTabUI <- function() {
    tagList(
        actionButton("biomass_help", "Press for instructions"),
        actionButton("tune_egg", "Tune egg density"),
        actionButton("rescale", "Rescale model"),
        plotlyOutput("plotTotalBiomass"),
        plotlyOutput("plotTotalAbundance"),
        uiOutput("biomass_sel"),
        plotlyOutput("plotBiomassDist")
    )
}

biomassTab <- function(input, output, session,
                       params, logs, trigger_update, ...) {
    # Help button ----
    help_steps <- data.frame(
        element = c(NA),
        intro = c("This still needs to be written.")
    )
    observeEvent(
        input$biomass_help,
        introjs(session, options = list(
            steps = help_steps)
        )
    )

    # Plot total biomass ----
    output$plotTotalBiomass <- renderPlotly({
        p <- params()
        no_sp <- length(p@species_params$species)
        cutoff <- p@species_params$cutoff_size
        # When no cutoff known, set it to maturity weight / 20
        if (is.null(cutoff)) cutoff <- p@species_params$w_mat / 20
        cutoff[is.na(cutoff)] <- p@species_params$w_mat[is.na(cutoff)] / 20
        observed <- p@species_params$biomass_observed
        if (is.null(observed)) observed <- 0

        # selector for foreground species
        foreground <- !is.na(p@A)
        foreground_indices <- (1:no_sp)[foreground]
        biomass_model <- foreground_indices  # create vector of right length
        for (i in seq_along(foreground_indices)) {
            sp <- foreground_indices[i]
            biomass_model[i] <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff[[sp]]])
        }
        species <- factor(p@species_params$species[foreground],
                          levels = p@species_params$species[foreground])
        df <- rbind(
            data.frame(Species = species,
                       Type = "Observed",
                       Biomass = observed[foreground]),
            data.frame(Species = species,
                       Type = "Model",
                       Biomass = biomass_model)
        )
        ggplot(df) +
            geom_col(aes(x = Species, y = Biomass, fill = Type),
                     position = "dodge") +
            scale_y_continuous(name = "Biomass [g]", trans = "log10",
                               breaks = log_breaks()) +
            theme_grey(base_size = 12) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })

    # Plot total abundance ----
    output$plotTotalAbundance <- renderPlotly({
        p <- params()
        no_sp <- length(p@species_params$species)
        cutoff <- p@species_params$cutoff_size
        # When no cutoff known, set it to maturity weight / 20
        if (is.null(cutoff)) cutoff <- p@species_params$w_mat / 20
        cutoff[is.na(cutoff)] <- p@species_params$w_mat[is.na(cutoff)] / 20
        observed <- p@species_params$abundance_observed
        if (is.null(observed)) observed <- 0

        # selector for foreground species
        foreground <- !is.na(p@A)
        foreground_indices <- (1:no_sp)[foreground]
        abundance_model <- foreground_indices  # create vector of right length
        for (i in seq_along(foreground_indices)) {
            sp <- foreground_indices[i]
            abundance_model[i] <- sum((p@initial_n[sp, ] * p@dw)[p@w >= cutoff[[sp]]])
        }
        species <- factor(p@species_params$species[foreground],
                          levels = p@species_params$species[foreground])
        df <- rbind(
            data.frame(Species = species,
                       Type = "Observed",
                       Abundance = observed[foreground]),
            data.frame(Species = species,
                       Type = "Model",
                       Abundance = abundance_model)
        )
        ggplot(df) +
            geom_col(aes(x = Species, y = Abundance, fill = Type),
                     position = "dodge") +
            scale_y_continuous(name = "Abundance", trans = "log10",
                               breaks = log_breaks()) +
            theme_grey(base_size = 12) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
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
        if (is.null(species_params$abundance_observed) ||
            is.na(species_params$abundance_observed)) {
            species_params$abundance_observed <- 0
        }
        if (is.null(species_params$cutoff_size) ||
            is.na(species_params$cutoff_size)) {
            species_params$cutoff_size <- 0
        }
        list(
            div(style = "display:inline-block",
                numericInput("biomass_observed",
                             paste0("Observed biomass for ", sp),
                             value = species_params$biomass_observed)),
            div(style = "display:inline-block",
                numericInput("abundance_observed",
                             paste0("Observed abundance for ", sp),
                             value = species_params$abundance_observed)),
            div(style = "display:inline-block",
                numericInput("cutoff_size", "Lower cutoff",
                             value = species_params$cutoff_size))
        )
    })

    ## Adjust biomass observed ####
    observe({
        p <- isolate(params())
        p@species_params[isolate(input$sp), "biomass_observed"] <-
            req(input$biomass_observed)
        p@species_params[isolate(input$sp), "abundance_observed"] <-
            req(input$abundance_observed)
        p@species_params[isolate(input$sp), "cutoff_size"] <-
            req(input$cutoff_size)
        params(p)
    })

    # Plot biomass distribution ----
    output$plotBiomassDist <- renderPlotly({
        req(input$sp, input$cutoff_size, input$biomass_observed)
        sp <- input$sp
        p <- params()
        biomass <- cumsum(p@initial_n[sp, ] * p@w * p@dw)

        max_w <- p@species_params[sp, "w_inf"]
        min_w <- p@species_params[sp, "w_min"]
        sel <- p@w >= min_w & p@w <= max_w
        df <- data.frame(Size = p@w[sel], Biomass = biomass[sel])
        pl <- ggplot(df, aes(x = Size, y = Biomass)) +
            geom_line(color = "blue") + scale_x_log10() +
            geom_vline(xintercept = p@species_params[sp, "w_mat"],
                       linetype = "dotted") +
            theme_grey(base_size = 12) +
            labs(x = "Size [g]", y = "Cummulative biomass")  +
            geom_text(aes(x = p@species_params[sp, "w_mat"],
                          y = max(Biomass * 0.2),
                          label = "\nMaturity"),
                      angle = 90)
        if (input$biomass_observed) {
            cutoff_idx <- which.max(p@w >= input$cutoff_size)
            target <- input$biomass_observed + biomass[[cutoff_idx]]
            pl <- pl +
                geom_hline(yintercept = biomass[[cutoff_idx]]) +
                geom_vline(xintercept = input$cutoff_size) +
                geom_hline(yintercept = target, color = "green")
        }
        pl
    })

    # Rescale model ----
    # to make biomass of current species agree with observation
    observeEvent(input$rescale, {
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        if ("biomass_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$biomass_observed[[sp]]) &&
            p@species_params$biomass_observed[[sp]] > 0) {
            cutoff <- p@species_params$cutoff_size[[sp]]
            if (is.null(cutoff) || is.na(cutoff)) {
                cutoff <- p@species_params$w_mat[[sp]] / 20
            }
            biomass_observed <- p@species_params$biomass_observed[[sp]]
            biomass_model <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
            scale_by <- biomass_observed / biomass_model
            p <- rescaleSystem(p, factor = scale_by)
            params(p)
            # Trigger an update of sliders
            trigger_update(runif(1))
        }
    })

    # Tune egg density ----
    # The "Tune egg density" button calculates the ratio of observed and
    # model biomass and then multiplies the egg density by that ratio.
    observeEvent(input$tune_egg, {
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        if ("biomass_observed" %in% names(p@species_params) &&
                !is.na(p@species_params$biomass_observed[[sp]]) &&
                p@species_params$biomass_observed[[sp]] > 0) {
            cutoff <- p@species_params$cutoff_size[[sp]]
            if (is.null(cutoff) || is.na(cutoff)) {
                cutoff <- p@species_params$w_mat[[sp]] / 20
            }
            total <- sum((p@initial_n[sp, ] * p@w * p@dw)[p@w >= cutoff])
            n0 <- input$n0 *
                p@species_params$biomass_observed[[sp]] / total
            print(total)
            print(p@species_params$biomass_observed[[sp]])
            # rescale abundance to new egg density
            p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 /
                p@initial_n[sp, p@w_min_idx[[sp]]]

            updateSliderInput(session, "n0",
                              value = n0,
                              min = signif(n0 / 10, 3),
                              max = signif(n0 * 10, 3))
        }
    })
}

growthTabUI <- function() {
    tagList(
        splitLayout(
            actionButton("growth_help", "Press for instructions"),
            radioButtons("all_growth", "Show:",
                         choices = c("All", "Selected species"),
                         selected = "All", inline = TRUE)
        ),
        plotOutput("plotGrowthCurve",
                   click = "growth_click"),
        textOutput("info"),
        uiOutput("k_vb_sel"),
        plotlyOutput("plot_feeding_level")
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

    observeEvent(input$growth_click, {
        if (input$growth_click$panelvar1 != input$sp) {
            updateSelectInput(session, "sp",
                              selected = input$growth_click$panelvar1)
            updateRadioButtons(session, "all_growth",
                               selected = "Selected species")
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
        no_sp <- length(p@species_params$species)
        if (input$all_growth == "All") {
            gc <- getGrowthCurves(p)[, , drop = FALSE] %>%
                cubelyr::as.tbl_cube(met_name = "Size") %>%
                as_tibble() %>%
                mutate(Legend = "Model",
                       Species = factor(Species, p@species_params$species))

            vb <- gc %>%
                mutate(Legend = "von Bertalanffy") %>%
                mutate(a = p@species_params[Species, "a"],
                       b = p@species_params[Species, "b"],
                       k_vb = p@species_params[Species, "k_vb"],
                       t0 = p@species_params[Species, "t0"]) %>%
                filter(!is.na(k_vb)) %>%
                mutate(L_inf = (p@species_params[Species, "w_inf"] / a)^(1 / b),
                       Size = a * (L_inf * (1 - exp(-k_vb * (Age - t0))))^b) %>%
                select(names(gc))

            ggplot(bind_rows(gc, vb)) +
                geom_line(aes(x = Age, y = Size, colour = Legend)) +
                scale_x_continuous(name = "Age [years]") +
                scale_y_continuous(name = "Size [g]") +
                geom_hline(aes(yintercept = w_mat),
                           data = tibble(Species = p@species_params$species[],
                                         w_mat = p@species_params$w_mat[]),
                           linetype = "dashed",
                           colour = "grey") +
                facet_wrap(~Species, scales = "free_y")
        } else {
            plotGrowthCurves(p, species = input$sp) +
                theme_grey(base_size = 12)
        }
    })

    # Plot feeding level ----
    output$plot_feeding_level <- renderPlotly({
        if (input$all_growth == "All") {
            plotFeedingLevel(params(), highlight = input$sp,
                             include_critical = TRUE) +
                theme_grey(base_size = 12)
        } else {
            plotFeedingLevel(params(), species = input$sp,
                             include_critical = TRUE) +
                theme_grey(base_size = 12)
        }
    })
}

reproTabUI <- function() {
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
        df <- data.frame(Species = factor(p@species_params$species[],
                                          levels = p@species_params$species),
                         erepro = p@species_params$erepro[])
        ggplot(df, aes(x = Species, y = erepro)) +
            geom_col() + geom_hline(yintercept = 1, color = "red") +
            scale_y_log10() +
            theme_grey(base_size = 12) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })

    # Plot psi ----
    output$plot_psi <- renderPlotly({
        if (!input$all_growth == "All") {
            p <- params()
            sp <- which.max(p@species_params$species == input$sp)
            w_min <- p@species_params$w_inf[sp] / 50
            sel <- p@w >= w_min & p@w <= p@species_params$w_inf[sp]
            df <- data.frame(Size = p@w[sel], psi = p@psi[sp, sel])
            ggplot(df, aes(x = Size, y = psi)) +
                geom_line(color = "blue") +
                geom_vline(xintercept = p@species_params[sp, "w_mat"],
                           linetype = "dotted") +
                theme_grey(base_size = 12) +
                labs(x = "Size [g]", y = "Proportion of energy for reproduction")  +
                geom_text(aes(x = p@species_params[sp, "w_mat"],
                              y = max(psi * 0.8),
                              label = "\nMaturity"),
                          angle = 90)
        }
    })
}

catchTabUI <- function() {
    tagList(
        actionButton("tune_catch", "Tune catchability"),
        uiOutput("catch_sel"),
        textOutput("catch_total"),
        plotlyOutput("plotCatchDist"),
        radioButtons("catch_x", "Show size in:",
                     choices = c("Weight", "Length"),
                     selected = "Length", inline = TRUE),
        plotlyOutput("plotTotalCatch")
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
    # model catch and then multiplies the catchability by that ratio. It
    # then runs the system to steady state.
    observeEvent(input$tune_catch, {
        p <- isolate(params())
        sp <- which.max(p@species_params$species == isolate(input$sp))
        if ("catch_observed" %in% names(p@species_params) &&
            !is.na(p@species_params$catch_observed[sp]) &&
            p@species_params$catch_observed[sp] > 0) {
            total <- sum(p@initial_n[sp, ] * p@w * p@dw *
                             getFMort(p)[sp, ])
            p@species_params$catchability[sp] <-
                p@species_params$catchability[sp] *
                p@species_params$catch_observed[sp] / total
            updateSliderInput(session, "catchability",
                              value = p@species_params$catchability[sp],
                              min = signif(max(0, p@species_params$catchability[sp] / 2 - 1), 2),
                              max = signif(max(p@species_params$catchability[sp] * 2, 2), 2))
            p <- setFishing(p)
            tryCatch({
                # Create a Progress object
                progress <- shiny::Progress$new(session)
                on.exit(progress$close())

                # Run to steady state
                p <- steady(p, t_max = 100, tol = 1e-2,
                            progress_bar = progress)

                # Update the reactive params object
                params(p)
                tuneParams_add_to_logs(logs, p)
            },
            error = function(e) {
                showModal(modalDialog(
                    title = "Invalid parameters",
                    HTML(paste0("These parameter do not lead to an acceptable steady state.",
                                "Please choose other values.<br>",
                                "The error message was:<br>", e)),
                    easyClose = TRUE
                ))}
            )
            params(p)
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
            theme_grey(base_size = 12) +
            scale_colour_manual(values = c("Model catch" = "blue",
                                           "Observed catch" = "red",
                                           "Abundance" = "grey"))
    })

    # Total catch by species ----
    output$plotTotalCatch <- renderPlotly({
        p <- params()
        no_sp <- length(p@species_params$species)
        observed <- p@species_params$catch_observed
        if (is.null(observed)) observed <- 0

        biomass <- sweep(p@initial_n, 2, p@w * p@dw, "*")
        catch_model <- rowSums(biomass * getFMort(p))

        # selector for foreground species
        foreground <- !is.na(p@A)
        foreground_indices <- (1:no_sp)[foreground]
        catch_model <- catch_model[foreground_indices]
        observed <- observed[foreground_indices]

        # Make sure species ordering is preserved in the plot
        species <- factor(p@species_params$species[foreground],
                          levels = p@species_params$species[foreground])

        df <- rbind(
            data.frame(Species = species,
                       Type = "Observed",
                       Catch = observed),
            data.frame(Species = species,
                       Type = "Model",
                       Catch = catch_model)
        )
        ggplot(df) +
            geom_col(aes(x = Species, y = Catch, fill = Type),
                     position = "dodge") +
            theme_grey(base_size = 12) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            scale_y_continuous(name = "Catch [megatonnes]", trans = "log10",
                               breaks = log_breaks()) +
            scale_fill_manual(values = c("Model" = "blue",
                                         "Observed" = "red"))
    })

    # Input field for observed catch ----
    output$catch_sel <- renderUI({
        p <- isolate(params())
        sp <- input$sp
        numericInput("catch_observed",
                     paste0("Observed total catch for ", sp, " (megatonnes)"),
                     value = p@species_params[sp, "catch_observed"])
    })

    # Adjust observed catch ----
    observeEvent(
        input$catch_observed,
        {
            p <- params()
            p@species_params[input$sp, "catch_observed"] <- input$catch_observed
            params(p)
        },
        ignoreInit = TRUE)

    # Output of model catch ----
    output$catch_total <- renderText({
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        total <- sum(p@initial_n[sp, ] * p@w * p@dw *
                         getFMort(p)[sp, ])
        paste("Model catch:", total)
    })
}

ratesTabUI <- function() {
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
            theme_grey(base_size = 12) +
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
            theme_grey(base_size = 12) +
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

preyTabUI <- function() {
    tagList(
        uiOutput("pred_size_slider"),
        plotlyOutput("plot_prey")
    )
}

preyTab <- function(input, output, session, params, logs, ...) {

    ## Prey size slider ----
    output$pred_size_slider <- renderUI({
        p <- isolate(params())
        sp <- which.max(p@species_params$species == input$sp)
        sliderInput("pred_size", "log predator size",
                    value = signif(log(p@species_params$w_mat[sp]), 2),
                    min = signif(log(p@species_params$w_min[sp]), 2),
                    max = signif(log(p@species_params$w_inf[sp]), 2),
                    step = 0.2,
                    width = "80%",
                    animate = animationOptions(loop = TRUE))
    })

    # Plot prey ----
    output$plot_prey <- renderPlotly({
        p <- params()
        sp <- which.max(p@species_params$species == input$sp)
        x <- log(p@w_full)
        dx <- x[2] - x[1]
        xp <- req(input$pred_size)
        wp <- exp(xp)
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
            tidyr::gather(Type, y, Kernel, Biomass, Numbers)
        ggplot(df) +
            geom_line(aes(w, y, color = Type)) +
            labs(x = "Weight [g]", y = "Density") +
            geom_point(aes(x = wp, y = 0), size = 4, colour = "blue") +
            scale_x_log10()
    })
}

dietTabUI <- function() {
    tagList(
        plotlyOutput("plot_diet")
    )
}

dietTab <- function(input, output, session, params, logs, ...) {

    # Plot diet ----
    output$plot_diet <- renderPlotly({
        req(input$sp)
        plotDiet(params(), input$sp)
    })
}

deathTabUI <- function() {
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

resourceTabUI <- function() {
    tagList(
        plotOutput("plot_resource", width = "84%"),
        plotlyOutput("plot_resource_pred"),
        radioButtons("resource_death_prop", "Show",
                     choices = c("Proportion", "Rate"),
                     selected = "Proportion",
                     inline = TRUE)
    )
}

resourceTab <- function(input, output, session, params, logs, ...) {

    # Plot resource ----
    output$plot_resource <- renderPlot({
        p <- params()
        select <- (p@cc_pp > 0)
        plot_dat <- data.frame(
            x = p@w_full[select],
            y = p@initial_n_pp[select] / p@cc_pp[select]
        )
        ggplot(plot_dat) +
            geom_line(aes(x, y)) +
            scale_x_log10("Resource size [g]") +
            ylab("Proportion of carrying capacity") +
            theme_grey(base_size = 16)
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
            theme_grey(base_size = 12)
    })
}

simTabUI <- function() {
    tagList(
        plotlyOutput("plot_sim")
    )
}

simTab <- function(input, output, session, params, logs, ...) {

    ## Plot run to steady ####
    output$plot_sim <- renderPlotly({
        sim <- tuneParams_run_steady(params(), return_sim = TRUE,
                          params = params, logs = logs,
                          session = session)
        plotBiomass(sim)
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
