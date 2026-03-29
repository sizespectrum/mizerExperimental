# Plot change in biomass over time

Plot change in biomass over time

## Usage

``` r
plotBiomassRelative(sim, sim_original = NULL, species = NULL, ...)

plotlyBiomassRelative(sim, sim_original = NULL, species = NULL, ...)
```

## Arguments

- sim:

  A MizerSim object

- sim_original:

  Another MizerSim object to compare the biomasses to. If NULL (default)
  then the biomasses are compared to the initial biomasses in `sim`.

- species:

  The species to be selected. Optional. By default all target species
  are selected. A vector of species names, or a numeric vector with the
  species indices, or a logical vector indicating for each species
  whether it is to be selected (TRUE) or not.

- ...:

  Parameters passed to `getBiomass()`

## Examples

``` r
plotBiomassRelative(NS_sim)

plotBiomassRelative(NS_sim, species = c("Cod", "Sole"))
```
