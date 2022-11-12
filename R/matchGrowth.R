#' Adjust model to produce observed growth
#'
#' Scales the search volume, the maximum consumption rate and the metabolic rate
#' all by the same factor in order to achieve a growth rate that allows
#' indivdiuals to reach their maturity size by their maturity age while keeping
#' the feeding level and the critical feeding level unchanged.
#'
#' Maturity size and age are taken from the `w_mat` and `age_mat` columns in the
#' species_params data frame.
#'
#' @param params A MizerParams object
#' @return A modified MizerParams object with rescaled search volume, maximum
#'   consumption rate and metabolic rate and rescaled species parameters
#'   `gamma`,`h`, `ks` and `k`.
#' @export
matchGrowth <- function(params) {
    assert_that(is(params, "MizerParams"))
    sp <- params@species_params
    if (!("age_mat" %in% names(sp))) {
        stop("This function requires an `age_mat` column in the species_params data frame.")
    }
    
    factor <- age_mat(params) / sp$age_mat

    params@search_vol <- params@search_vol * factor
    params@intake_max <- params@intake_max * factor
    params@metab <- params@metab * factor
    params@species_params$gamma <- sp$gamma * factor
    params@species_params$h <- sp$h * factor
    if ("ks" %in% names(sp)) {
        params@species_params$ks <- sp$ks * factor
    }
    if ("k" %in% names(sp)) {
        params@species_params$k <- sp$k * factor
    }
    
    params
}