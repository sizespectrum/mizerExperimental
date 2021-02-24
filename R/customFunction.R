#' Replace a mizer function with a custom version
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function allows you to make arbitrary changes to how mizer works by
#' allowing you to replace any mizer function with your own version. You
#' should do this only as a last resort, when you find that you can not use
#' the standard mizer extension mechanism to achieve your goal.
#'
#' @details
#' If the function you need to overwrite is one of the mizer rate functions,
#' then you should use `setRateFunction()` instead of this function. Similarly
#' you should use `setResource()` to change the resource dynamics and
#' `setReproduction()` to change the density-dependence in reproduction.
#'
#' If you find that your goal really does require you to overwrite a mizer
#' function, please also create an issue on the mizer issue tracker at
#' <https://github.com/sizespectrum/mizer/issues> to
#' describe your goal, because it will be interesting to the mizer community
#' and may motivate future improvements to the mizer functionality.
#'
#' Note that `customFunction()` only overwrites the function used by the mizer
#' code. It does not overwrite the function that is exported by mizer. This
#' is clarified when you run the code in the Examples section.
#'
#' @param name Name of mizer function to replace
#' @param fun The custom function to use as replacement
#' @export
#' @examples
#' \dontrun{
#' fake_project <- function(...) "Fake"
#' customFunction("project", fake_project)
#' mizer::project(NS_params) # This will print "Fake"
#' project(NS_params) # This will still use the old project() function
#' # To undo the effect:
#' customFunction("project", project)
#' mizer::project(NS_params) # This will again use the old project()
#' }
customFunction <- function(name, fun) {
    environment(fun) <- asNamespace('mizer')
    assignInNamespace(name, fun, ns = "mizer")
}
