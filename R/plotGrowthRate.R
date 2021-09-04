#' Plot growth rate as a function of weight w
plotGrowthVsW <- function(params, species = NULL) {
    params <- validParams(params)
    species <- valid_species_arg(params, species)
    sp_pars <- species_params(params)
    no_sp <- nrow(sp_pars)
    e <- getEGrowth(params)[species, , drop = FALSE]
    b <- sp_pars[["b"]] %||% rep(3, no_sp)
    w <- w(params)
    # Todo: make this more elegant
    vb <- e # to get right dimensions
    for (sp in species) {
        idx <- which(sp_pars$species == sp)
        vb[sp, ] <-  b[[idx]]  * sp_pars[sp, "k_vb"] *
            (sp_pars[sp, "w_inf"] ^ (1 / b[[idx]]) - w ^ (1 / b[[idx]])) *
            w ^ (1 - 1 / b[[idx]])
    }
    edf <- melt(e)
    edf$Type <- "Model"
    vbdf <- melt(vb)
    vbdf[vbdf$value < 0, "value"] <- 0
    vbdf$Type <- "von Bertalanffy"
    df <- rbind(edf, vbdf)
    ggplot(df, aes(x = w, y = value, color = Type)) +
        geom_line() +
        scale_x_log10() +
        facet_wrap(~sp, scales = "free_y")
}

#' Plot growth rate as a function of length
plotGrowthVsL <- function(params, species = NULL) {
    params <- validParams(params)
    species <- valid_species_arg(params, species)
    sp_pars <- species_params(params)
    no_sp <- nrow(sp_pars)
    e <- getEGrowth(params)[species, , drop = FALSE]
    av <- sp_pars[["a"]] %||% rep(0.006, no_sp)
    bv <- sp_pars[["b"]] %||% rep(3, no_sp)
    w <- w(params)
    # Todo: make this more elegant
    dldt <- e
    vb <- e # to get right dimensions
    for (sp in species) {
        idx <- which(sp_pars$species == sp)
        a <- av[[idx]]
        b <- bv[[idx]]
        l <- (w / a) ^ (1 / b)
        l_inf <- (sp_pars[sp, "w_inf"] / a) ^ (1 / b)
        dldt[sp, ] <- l ^ (1 - b) / a / b * e[sp, ]
        vb[sp, ] <-  sp_pars[sp, "k_vb"] * (l_inf - l)
    }
    edf <- melt(dldt)
    edf$Type <- "Model"
    vbdf <- melt(vb)
    vbdf[vbdf$value < 0, "value"] <- 0
    vbdf$Type <- "von Bertalanffy"
    df <- rbind(edf, vbdf)
    ggplot(df, aes(x = w, y = value, color = Type)) +
        geom_line() +
        scale_x_log10(name = "Length [cm]") +
        ylab("Growth rate [cm/year]") +
        facet_wrap(~sp, scales = "free_y")
}

`%||%` <- function(lhs, rhs) {
    if (!is.null(lhs)) {
        lhs
    } else {
        rhs
    }
}
