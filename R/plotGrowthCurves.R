# TODO: move to mizer for next release
#' @export
plotGrowthCurves <- function(object, species = NULL, 
                             max_age = 20, percentage = FALSE, 
                             species_panel = FALSE, highlight = NULL,
                             size_at_age = NULL,
                             return_data = FALSE, ...) {
    assert_that(is.flag(percentage),
                is.flag(species_panel),
                is.flag(return_data),
                is.number(max_age))
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    sp <- params@species_params
    sp <- set_species_param_default(sp, "age_mat", age_mat_vB(params))
    
    # size at age
    if (!is.null(size_at_age)) {
        if (!"species" %in% names(size_at_age)) {
            stop("The size at age data frame needs to have a 'species' column.")
        }
        if (!"age" %in% names(size_at_age)) {
            stop("The size at age data frame needs to have an 'age' column.")
        }
        if (!any(c("weight", "length") %in% names(size_at_age))) {
            stop("The size at age data frame needs to have either a 'length' or a 'weight' column.")
        }
        if (!"weight" %in% names(size_at_age)) {
            size_at_age <- set_species_param_default(size_at_age, "a", 0.004, message = "Using a = 0.004 for missing weight-length conversion parameters.")
            size_at_age <- set_species_param_default(size_at_age, "b", 3, message = "Using b = 3 for missing weight-length conversion parameters.")
            size_at_age$weight <- size_at_age$a * size_at_age$length ^ size_at_age$b
        }
    }
    
    species <- valid_species_arg(params, species)
    selected_species <- species # needed later to not confuse variable and column name
    sp_sel <- sp$species %in% species
    ws <- getGrowthCurves(params, species, max_age, percentage)
    plot_dat <- reshape2::melt(ws)
    plot_dat$Species <- factor(plot_dat$Species, sp$species)
    plot_dat$Legend <- "model"
    
    # creating some VB
    if (all(c("a", "b", "k_vb") %in% names(sp))) {
        if ("t0" %in% names(sp)) {
            t0 <- sp$t0
        } else {
            t0 <- 0
        }
        VBdf <- data.frame("species" = sp$species, 
                           "w_inf" = sp$w_inf, 
                           "a" = sp$a, 
                           "b" = sp$b, 
                           "k_vb" = sp$k_vb, 
                           "t0" = t0) 
        VBdf$L_inf <- (VBdf$w_inf / VBdf$a) ^ (1 / VBdf$b)
        plot_dat2 <- plot_dat
        plot_dat2$value <- 
            apply(plot_dat, 1,
                  function(x) {
                      sel <- VBdf$species == x[1]
                      length <- VBdf$L_inf[sel] * 
                          (1 - exp(-VBdf$k_vb[sel] * 
                                       (as.numeric(x[2]) - VBdf$t0[sel])))
                      VBdf$a[sel] * length ^ VBdf$b[sel]
                  })
        plot_dat2$Legend <- "von Bertalanffy"
        plot_dat <- rbind(plot_dat, plot_dat2)
    }
    if (return_data) return(plot_dat)
    
    p <- ggplot(filter(plot_dat, Legend == "model")) + 
        geom_line(aes(x = Age, y = value, 
                      colour = Species, linetype = Species, linewidth = Species))
    y_label <- if (percentage) 
        "Percent of maximum size"
    else "Size [g]"
    # Need to keep species in order for legend
    legend_levels <- 
        intersect(c(dimnames(params@initial_n)$sp,
                    "Background", "Resource", "Total"),
                  plot_dat$Species)
    plot_dat$Species <- factor(plot_dat$Species, levels = legend_levels)
    linesize <- rep(0.8, length(legend_levels))
    names(linesize) <- names(params@linetype[legend_levels])
    linesize[highlight] <- 1.6
    p <- p + scale_x_continuous(name = "Age [Years]") + 
        scale_y_continuous(name = y_label) + 
        scale_colour_manual(values = params@linecolour[legend_levels]) + 
        scale_linetype_manual(values = params@linetype[legend_levels]) + 
        scale_discrete_manual("linewidth", values = linesize)
    
    # starting cases now
    if (!percentage)  {
        if (length(species) == 1) {
            idx <- which(sp$species == species)
            w_inf <- sp$w_inf[idx]
            p <- p + geom_hline(yintercept = w_inf, colour = "grey") + 
                annotate("text", 0, w_inf, vjust = -1, label = "Maximum")
            w_mat <- sp$w_mat[idx]
            age_mat <- sp$age_mat[idx]
            p <- p + geom_hline(yintercept = w_mat, linetype = "dashed", 
                                colour = "grey") + 
                geom_vline(xintercept = age_mat, linetype = "dashed", 
                           colour = "grey") +
                annotate("text", 0, w_mat, vjust = -1, label = "Maturity")
            if ("von Bertalanffy" %in% plot_dat$Legend) {
                p <- p + geom_line(data = filter(plot_dat, Legend == "von Bertalanffy"), 
                                   aes(x = Age, y = value))
            }
            if (!is.null(size_at_age)) {
                size_at_age <- filter(size_at_age, species == selected_species)
                p <- p + geom_point(aes(x = age, y = weight), data = size_at_age,
                                    alpha = 0.2)
            }
            
        } else if (species_panel) { # need to add either no panel if no param 
            # for VB or create a panel without VB
            p <- ggplot(plot_dat) +
                geom_line(aes(x = Age, y = value , colour = Legend)) +
                scale_x_continuous(name = "Age [years]") +
                scale_y_continuous(name = "Size [g]") +
                geom_hline(aes(yintercept = w_mat),
                           data = tibble(Species = factor(legend_levels),
                                         w_mat = sp$w_mat[sp_sel]),
                           linetype = "dashed",
                           colour = "grey") +
                geom_vline(aes(xintercept = age_mat),
                           data = tibble(Species = factor(legend_levels),
                                         age_mat = sp$age_mat[sp_sel]),
                           linetype = "dashed",
                           colour = "grey") +
                geom_hline(aes(yintercept = w_inf),
                           data = tibble(Species = factor(legend_levels),
                                         w_inf = sp$w_inf[sp_sel]),
                           linetype = "solid",
                           colour = "grey") +
                facet_wrap(~Species, scales = "free_y")
            
        }
        
    }
    return(p)
    
}