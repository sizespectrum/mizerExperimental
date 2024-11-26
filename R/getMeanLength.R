#' Calculate the mean length of the community
#'
#' Calculates the mean length of the community through time. This is simply the
#' total biomass of the community divided by the abundance in numbers then converted 
#' the weights into species specific lengths based on species params. You can
#' specify minimum and maximum length or length range for the species. Lengths
#' take precedence over lengths (i.e. if both min_l and min_w are supplied, only
#' min_l will be used). You can also specify the species to be used in the
#' calculation.
#'
#' @param sim A \linkS4class{MizerSim} object
#' @inheritParams valid_species_arg
#' @inheritDotParams get_size_range_array -params
#'
#' @return A vector containing the mean length of the community through time
#' @export
#' @family functions for calculating indicators
#' @concept summary_function
#' @examples
#' mean_length <- getMeanlength(NS_sim)
#' years <- c("1967", "2010")
#' mean_length[years]
#' getMeanLength(NS_sim, species = c("Herring", "Sprat", "N.pout"))[years]
#' getMeanLength(NS_sim, min_w = 10, max_w = 5000)[years]

getMeanLength <- function (sim, species = NULL, ...) 
{
  assert_that(is(sim, "MizerSim"))
  species <- valid_species_arg(sim, species)
  n_species <- getN(sim, ...)
  biomass_species <- getBiomass(sim, ...)
  n_total <- apply(n_species[, species, drop = FALSE], 1, sum)
  biomass_total <- apply(biomass_species[, species, drop = FALSE], 
                         1, sum)
  mean_weight_species <- biomass_species/n_species
  conversion <- function() exp(log(mean_weight_species[, species, drop = FALSE]/sim@params@species_params["a"])/sim@params@species_params["b"]) 
  mean_length_species<- conversion()
  mean_length <- apply(mean_weight_species[, species,drop = FALSE], 1, sum)
  return(mean_length)
}
