# TODO: move to mizer for next release
#' @export
plotGrowthCurves <- function(object, species = NULL, 
                             max_age = 20, percentage = FALSE, 
                             species_panel = FALSE, highlight = NULL,
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
    params <- set_species_param_default(params, "age_mat", age_mat_vB(params))
    species <- valid_species_arg(params, species)
    sp_sel <- params@species_params$species %in% species
    ws <- getGrowthCurves(params, species, max_age, percentage)
    plot_dat <- reshape2::melt(ws)
    plot_dat$Species <- factor(plot_dat$Species, params@species_params$species)
    plot_dat$Legend <- "model"
    
    # creating some VB
    if (all(c("a", "b", "k_vb") %in% names(params@species_params))) {
        if ("t0" %in% names(params@species_params)) {
            t0 <- params@species_params$t0
        } else {
            t0 <- 0
        }
        VBdf <- data.frame("species" = params@species_params$species, 
                           "w_inf" = params@species_params$w_inf, 
                           "a" = params@species_params$a, 
                           "b" = params@species_params$b, 
                           "k_vb" = params@species_params$k_vb, 
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
            idx <- which(params@species_params$species == species)
            w_inf <- params@species_params$w_inf[idx]
            p <- p + geom_hline(yintercept = w_inf, colour = "grey") + 
                annotate("text", 0, w_inf, vjust = -1, label = "Maximum")
            w_mat <- params@species_params$w_mat[idx]
            age_mat <- params@species_params$age_mat[idx]
            p <- p + geom_hline(yintercept = w_mat, linetype = "dashed", 
                                colour = "grey") + 
                geom_vline(xintercept = age_mat, linetype = "dashed", 
                           colour = "grey") +
                annotate("text", 0, w_mat, vjust = -1, label = "Maturity")
            if ("von Bertalanffy" %in% plot_dat$Legend) 
                p <- p + geom_line(data = filter(plot_dat, Legend == "von Bertalanffy"), 
                                   aes(x = Age, y = value))
            
        } else if (species_panel) { # need to add either no panel if no param 
            # for VB or create a panel without VB
            p <- ggplot(plot_dat) +
                geom_line(aes(x = Age, y = value , colour = Legend)) +
                scale_x_continuous(name = "Age [years]") +
                scale_y_continuous(name = "Size [g]") +
                geom_hline(aes(yintercept = w_mat),
                           data = tibble(Species = factor(legend_levels),
                                         w_mat = params@species_params$w_mat[sp_sel]),
                           linetype = "dashed",
                           colour = "grey") +
                geom_vline(aes(xintercept = age_mat),
                           data = tibble(Species = factor(legend_levels),
                                         age_mat = params@species_params$age_mat[sp_sel]),
                           linetype = "dashed",
                           colour = "grey") +
                geom_hline(aes(yintercept = w_inf),
                           data = tibble(Species = factor(legend_levels),
                                         w_inf = params@species_params$w_inf[sp_sel]),
                           linetype = "solid",
                           colour = "grey") +
                facet_wrap(~Species, scales = "free_y")
            
        }
    }
    return(p)
    
}