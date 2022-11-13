#' @export
get_h_default <- function(object) {
    if (is(object, "MizerParams")) {
        species_params <- object@species_params
    } else {
        species_params <- validSpeciesParams(object)
    }
    species_params <- set_species_param_default(species_params, "f0", 0.6)
    if (!("h" %in% colnames(species_params))) {
        species_params$h <- rep(NA, nrow(species_params))
    }
    missing <- is.na(species_params$h)
    if (any(missing)) {
        assert_that(is.numeric(species_params$f0),
                    noNA(species_params$alpha),
                    "alpha" %in% names(species_params))
        rlang::signal("No h provided for some species, so using age at maturity to calculate it.",
                      class = "info_about_default", var = "h", level = 3)
        if (!isTRUE(all.equal(species_params$n[missing], species_params$p[missing],
                              check.attributes = FALSE))) {
            rlang::signal("Because you have n != p, the default value for `h` is not very good.",
                          class = "info_about_default", var = "h", level = 1)
        }
        species_params <- species_params %>% 
            set_species_param_default("fc", 0.2) %>% 
            set_species_param_default("n", 3/4) %>% 
            set_species_param_default("age_mat", age_mat_vB(species_params))
        w_mat <- species_params$w_mat
        w_min <- species_params$w_min
        age_mat <- species_params$age_mat
        n <- species_params$n
        h <- (w_mat^(1 - n) - w_min^(1 - n)) / age_mat / (1 - n) / 
            species_params$alpha / (species_params$f0 - species_params$fc)
        
        if (any(is.na(h[missing])) || any(h[missing] <= 0)) {
            stop("Could not calculate h.")
        }
        species_params$h[missing] <- h[missing]
    }
    return(species_params$h)
}
