#' Set semichemostat dynamics for resources
#' 
#' Uses the resource parameters provided in `resource_params` to set the
#' initial resource abundances and the resource rate and then sets the 
#' resource capacity so that the initial abundances are at steady state, given
#' the current consumption rate.
#' 
#' @param params A MizerParams object
#' @param resource_params A resource parameter dataframe
#' @return A MizerParams object with updated resources
#' @export
setResourceSemichemostat <- function(params, resource_params) {
    if (!is.null(getComponent(params, "MR"))) {
        # temporary code:
        comment(params@other_params$MR$capacity) <- NULL
        comment(params@other_params$MR$rate) <- NULL
        
        mizerMR::resource_params(params) <- resource_params
        mizerMR::initialNResource(params) <- mizerMR::resource_capacity(params)
        
        rr <- mizerMR::resource_rate(params)
        cc <- (rr + getResourceMort(params))/rr * mizerMR::initialNResource(params)
        cc[rr == 0] <- 0
        comment(cc) <- NULL
        params@other_params$MR$capacity <- cc
    } else {
        stop("This function is not yet implemented for single-resource models.")
    }
    params
}