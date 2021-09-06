#' Serve tab with catch data
#'
#' @inheritParams biomassTab
#' @param catch Data frame holding binned observed catch data. The data can
#'   be binned either into length bins or weight bins. In the former case the data
#'   frame should have columns \code{length} and \code{dl} holding the start of
#'   the size bins in cm and the width of the size bins in cm respectively. In
#'   the latter case the data frame should have columns \code{weight} and
#'   \code{dw} holding the start of the size bins in grams and the width of the
#'   size bins in grams. The data frame also needs to have the columns
#'   \code{species} (the name of the species), \code{catch} (the number of
#'   individuals of a particular species caught in a size bin).
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

#' @rdname catchTab
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
