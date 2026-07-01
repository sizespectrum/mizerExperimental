#' Callback function for plotting biomass in real-time
#'
#' This function can be passed to the `callback` argument of [project()] to
#' plot the biomasses of the species in real-time as the simulation runs.
#' 
#' @param sim A [MizerSim] object.
#' @param t_idx The current time index.
#' @param ... Other arguments passed to the callback (such as `ylim`, `species`,
#'   `use_cutoff`, etc.).
#' @export
#' @examples
#' \dontrun{
#' params <- NS_params
#' # Open an external graphics window to see the real-time plot in RStudio
#' x11()
#' sim <- project(params, callback = biomass_callback)
#' }
biomass_callback <- local({
    # State variables captured by the closure
    weight <- NULL
    plot_species <- NULL
    cols <- NULL
    bm_prev <- NULL
    t_prev <- NULL
    log_y <- TRUE
    old_mar <- NULL
    
    function(sim, t_idx, ...) {
        params <- sim@params
        times <- as.numeric(dimnames(sim@n)[[1]])
        dots <- list(...)
        
        if (t_idx == 1) {
            # Set up species to trace
            if ("species" %in% names(dots)) {
                # Check that all specified species are actually in the model
                invalid_species <- setdiff(dots$species, params@species_params$species)
                if (length(invalid_species) > 0) {
                    warning("The following species specified in biomass_callback were not found: ",
                            paste(invalid_species, collapse = ", "))
                }
                plot_species <<- intersect(dots$species, params@species_params$species)
            } else {
                plot_species <<- names(params@linecolour)[names(params@linecolour) %in% params@species_params$species]
            }
            
            if (length(plot_species) == 0) {
                return()
            }
            
            # Precompute weights for biomass calculation
            use_cutoff <- if ("use_cutoff" %in% names(dots)) dots$use_cutoff else FALSE
            min_w <- if ("min_w" %in% names(dots)) dots$min_w else min(params@w)
            max_w <- if ("max_w" %in% names(dots)) dots$max_w else max(params@w)
            min_l <- if ("min_l" %in% names(dots)) dots$min_l else NULL
            max_l <- if ("max_l" %in% names(dots)) dots$max_l else NULL
            
            if (use_cutoff && "biomass_cutoff" %in% names(params@species_params)) {
                biomass_cutoff <- params@species_params$biomass_cutoff
                biomass_cutoff[is.na(biomass_cutoff)] <- min(params@w)
                size_range <- get_size_range_array(params, min_w = biomass_cutoff)
            } else {
                size_range <- get_size_range_array(params, min_w = min_w, max_w = max_w,
                                                   min_l = min_l, max_l = max_l)
            }
            
            if (isTRUE(params@second_order_w[["bin_average"]])) {
                weight <<- sweep(
                    mizer:::bin_average_weight(sweep(size_range, 2, params@w, "*")),
                    2, params@dw, "*")
            } else {
                weight <<- sweep(size_range, 2, params@w * params@dw, "*")
            }
            
            # Calculate initial biomass
            bm_init <- rowSums(sim@n[1, , ] * weight)
            
            log_y <<- if ("log_y" %in% names(dots)) dots$log_y else TRUE
            if ("log" %in% names(dots)) {
                log_axes <- parsePlotLog(dots$log, log_x = FALSE, log_y = log_y)
                log_y <<- log_axes$log_y
            }
            
            ylim_val <- if ("ylim" %in% names(dots)) dots$ylim else c(NA, NA)
            if (anyNA(ylim_val)) {
                bm_pos <- bm_init[bm_init > 0]
                if (length(bm_pos) == 0) {
                    ylim_val <- c(1e-2, 1e2)
                } else {
                    min_bm <- min(bm_pos)
                    max_bm <- max(bm_pos)
                    if (log_y) {
                        ylim_val[is.na(ylim_val)] <- c(min_bm / 10, max_bm * 10)
                    } else {
                        ylim_val[is.na(ylim_val)] <- c(0, max_bm * 1.1)
                    }
                }
            }
            
            old_mar <<- graphics::par(mar = c(5.1, 4.1, 4.1, 8.1))
            
            xlim_val <- c(min(times), max(times))
            log_str <- if (log_y) "y" else ""
            
            graphics::plot(NULL, xlim = xlim_val, ylim = ylim_val, log = log_str,
                           xlab = "Year", ylab = "Biomass [g]",
                           main = "Biomass Trace")
            
            cols <<- params@linecolour[plot_species]
            cols[is.na(cols)] <<- "black"
            
            graphics::legend(x = "topleft", inset = c(1.02, 0), legend = plot_species,
                             col = cols, lty = 1, lwd = 2, bg = "white", xpd = TRUE)
            
            grDevices::dev.flush()
            
            bm_prev <<- bm_init
            t_prev <<- times[[1]]
        } else {
            if (length(plot_species) == 0) {
                return()
            }
            
            bm_curr <- rowSums(sim@n[t_idx, , ] * weight)
            for (sp in plot_species) {
                if (is.na(bm_prev[sp]) || is.na(bm_curr[sp])) {
                    next
                }
                if (log_y && (bm_prev[sp] <= 0 || bm_curr[sp] <= 0)) {
                    next
                }
                graphics::segments(t_prev, bm_prev[sp], times[[t_idx]], bm_curr[sp], col = cols[sp], lwd = 2)
            }
            grDevices::dev.flush()
            Sys.sleep(0.01)
            bm_prev <<- bm_curr
            t_prev <<- times[[t_idx]]
            
            # If final step, restore old margins
            if (t_idx == length(times)) {
                graphics::par(old_mar)
            }
        }
    }
})
