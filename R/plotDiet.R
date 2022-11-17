# TODO: remove upon next release of mizer
#' @export
plotDiet <- function(object, species = NULL, return_data = FALSE,
                     xtrans = c("log10", "identity")) {
    assert_that(is.flag(return_data))
    xtrans = match.arg(xtrans)
    params <- validParams(object)
    species <- valid_species_arg(object, species, return.logical = TRUE)
    diet <- getDiet(params)[species, , , drop = FALSE]
    names(dimnames(diet)) <- c("Predator", "w", "Prey")
    plot_dat <- melt(diet, value.name = "Proportion")
    prey <- dimnames(diet)$Prey
    # the plot looks better upsided down
    plot_dat$Prey <- factor(plot_dat$Prey, levels = rev(prey))
    
    plot_dat <- plot_dat[plot_dat$Proportion > 0.001, ]
    if (return_data) return(plot_dat)
    
    legend_levels <- 
        intersect(names(params@linecolour), plot_dat$Prey)
    p <- ggplot(plot_dat) +
        geom_area(aes(x = w, y = Proportion, fill = Prey)) +
        labs(x = "Size [g]", y = "Proportion") +
        scale_fill_manual(values = params@linecolour[legend_levels],
                          limits = legend_levels) +
        scale_x_continuous(trans = xtrans)
    if (sum(species) > 1) {
        p <- p + facet_wrap(vars(Predator))
    }
    p
}