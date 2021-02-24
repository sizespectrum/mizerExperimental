
#' Display frames
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Takes two data frames with plotting data and displays them side-by-side,
#' using the same axes and legend.
#'
#' @details
#' The two data frames each need to have the same three variables. The first
#' variable will go on the x-axis, the third on the y-axis with a logarithmic
#' scale. The second variable should be the species and will be used to group
#' the data and display with the linetype and linecolour specified by the
#' \code{linetype} and \code{linecolour} slots of the \code{params} object.
#'
#' The recommended way is to obtain the data frames using one of the supplied
#' functions, e.g., \code{\link{getBiomassFrame}}, \code{\link{getSSBFrame}}.
#'
#' @param f1 Data frame for left plot
#' @param f2 Data frame for right plot
#' @param params A MizerParams object
#' @param xlab Label for x-axis. Defaults to first variable name.
#' @param ylab Label for y-axis. Defaults to third variable name.
#' @param y_ticks The approximate number of ticks desired on the y axis
#'
#' @return ggplot2 object
#' @export
#' @family frame functions
#' @family plotting functions
#' @seealso \code{\link{plotting_functions}},
#'   \code{\link{getBiomassFrame}}, \code{\link{getSSBFrame}}
#' @examples
#' \dontrun{
#' # Set up example MizerParams and MizerSim objects
#' params <- suppressMessages(newMultispeciesParams(NS_species_params_gears, inter))
#' sim0 <- project(params, effort=0, t_max=20, progress_bar = FALSE)
#' sim1 <- project(params, effort=1, t_max=20, progress_bar = FALSE)
#'
#' # Display biomass from each simulation next to each other
#' displayFrames(getBiomassFrame(sim0), getBiomassFrame(sim1), params)
#' }
displayFrames <- function(f1, f2, params,
                          xlab = NA, ylab = NA,
                          y_ticks = 6) {
    var_names <- names(f1)
    if (!(length(var_names) == 3)) {
        stop("A frame needs to have three variables.")
    }
    if (!all(names(f2) == var_names)) {
        stop("Both frames need to have the same variable names.")
    }
    f <- rbind(cbind(f1, Simulation = 1), cbind(f2, Simulation = 2))

    if (is.na(xlab)) {
        xlab <- var_names[1]
    }
    if (is.na(ylab)) {
        ylab <- var_names[3]
    }
    ytrans <- "log10"
    breaks <- log_breaks(n = y_ticks)

    p <- ggplot(f, aes_string(x = names(f)[1], y = names(f)[3],
                              colour = names(f)[2], linetype = names(f)[2])) +
        scale_y_continuous(trans = ytrans, breaks = breaks,
                           labels = prettyNum, name = ylab) +
        scale_x_continuous(name = xlab) +
        geom_line() +
        facet_wrap(~ Simulation) +
        scale_colour_manual(values = params@linecolour) +
        scale_linetype_manual(values = params@linetype)
    return(p)
}


#' Get data frame of spawning stock biomass of species through time,
#' ready for ggplot2
#'
#' After running a projection, the spawning stock biomass of each species can be
#' plotted against time.
#'
#' @param sim An object of class \linkS4class{MizerSim}
#' @param species Name or vector of names of the species to be plotted. By
#'   default all foreground species are plotted.
#' @param start_time The first time to be plotted. Default is the beginning
#'   of the time series.
#' @param end_time The last time to be plotted. Default is the end of the
#'   time series.
#' @param ylim A numeric vector of length two providing limits of for the
#'   y axis. Use NA to refer to the existing minimum or maximum. Any values
#'   below 1e-20 are always cut off.
#' @param total A boolean value that determines whether the total SSB from
#'   all species is plotted as well. Default is FALSE
#'
#' @return A data frame that can be used in \code{\link{displayFrames}}
#' @export
#' @family frame functions
#' @seealso \code{\link{getSSB}}
getSSBFrame <- function(sim,
                        species = dimnames(sim@n)$sp[!is.na(sim@params@A)],
                        start_time = as.numeric(dimnames(sim@n)[[1]][1]),
                        end_time = as.numeric(dimnames(sim@n)[[1]][dim(sim@n)[1]]),
                        ylim = c(NA, NA), total = FALSE){
    b <- getSSB(sim)
    if (start_time >= end_time) {
        stop("start_time must be less than end_time")
    }
    # Select time range
    b <- b[(as.numeric(dimnames(b)[[1]]) >= start_time) &
               (as.numeric(dimnames(b)[[1]]) <= end_time), , drop = FALSE]
    b_total <- rowSums(b)
    # Include total
    if (total) {
        b <- cbind(b, Total = b_total)
    }
    bm <- mizer::melt(b)
    # Implement ylim and a minimal cutoff
    min_value <- 1e-20
    bm <- bm[bm$value >= min_value &
                 (is.na(ylim[1]) | bm$value >= ylim[1]) &
                 (is.na(ylim[2]) | bm$value <= ylim[2]), ]
    names(bm) <- c("Year", "Species", "SSB")
    # Force Species column to be a factor (otherwise if numeric labels are
    # used they may be interpreted as integer and hence continuous)
    bm$Species <- as.factor(bm$Species)
    # Select species
    bm <- bm[bm$Species %in% species, ]
    return(bm)
}


#' Get data frame of biomass of species through time, ready for ggplot2
#'
#' After running a projection, the biomass of each species can be plotted
#' against time. The biomass is calculated within user defined size limits
#' (min_w, max_w, min_l, max_l, see \code{\link{getBiomass}}). This function
#' returns a dataframe that can be displayed with
#' \code{\link{displayFrames}}.
#'
#' @param sim An object of class \linkS4class{MizerSim}
#' @param species Name or vector of names of the species to be plotted. By
#'   default all species are plotted.
#' @param start_time The first time to be plotted. Default is the beginning
#'   of the time series.
#' @param end_time The last time to be plotted. Default is the end of the
#'   time series.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param total A boolean value that determines whether the total biomass from
#'   all species is plotted as well. Default is FALSE.
#' @param ... Arguments passed to get_size_range_array
#'
#' @return A data frame that can be used in \code{\link{displayFrames}}
#' @export
#' @family frame functions
#' @seealso \code{\link{getBiomass}}, \code{\link{displayFrames}}
#' @examples
#' \dontrun{
#' # Set up example MizerParams and MizerSim objects
#' params <- suppressMessages(newMultispeciesParams(NS_species_params_gears, inter))
#' sim0 <- project(params, effort=0, t_max=20, progress_bar = FALSE)
#' sim1 <- project(params, effort=1, t_max=20, progress_bar = FALSE)
#'
#' # Display biomass from each simulation next to each other
#' displayFrames(getBiomassFrame(sim0), getBiomassFrame(sim1), params)
#' }
getBiomassFrame <- function(sim,
                            species = dimnames(sim@n)$sp[!is.na(sim@params@A)],
                            start_time = as.numeric(dimnames(sim@n)[[1]][1]),
                            end_time = as.numeric(dimnames(sim@n)[[1]][dim(sim@n)[1]]),
                            ylim = c(NA, NA), total = FALSE, ...) {
    b <- getBiomass(sim, ...)
    if (start_time >= end_time) {
        stop("start_time must be less than end_time")
    }
    # Select time range
    b <- b[(as.numeric(dimnames(b)[[1]]) >= start_time) &
               (as.numeric(dimnames(b)[[1]]) <= end_time), , drop = FALSE]
    b_total <- rowSums(b)
    # Include total
    if (total) {
        b <- cbind(b, Total = b_total)
        species <- c("Total", species)
    }
    bm <- mizer::melt(b)

    # Implement ylim and a minimal cutoff
    min_value <- 1e-20
    bm <- bm[bm$value >= min_value &
                 (is.na(ylim[1]) | bm$value >= ylim[1]) &
                 (is.na(ylim[2]) | bm$value <= ylim[2]), ]
    names(bm) <- c("Year", "Species", "Biomass")

    # Force Species column to be a factor (otherwise if numeric labels are
    # used they may be interpreted as integer and hence continuous).
    # Need to keep species in order for legend.
    species_levels <- c(dimnames(sim@n)$sp, "Background", "Resource", "Total")
    bm$Species <- factor(bm$Species, levels = species_levels)

    # Select species
    bm <- bm[bm$Species %in% species, ]

    return(bm)
}
