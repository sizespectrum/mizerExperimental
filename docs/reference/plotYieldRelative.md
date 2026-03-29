# Plot change in yield over time

Plot change in yield over time

## Usage

``` r
plotYieldRelative(sim, object_original, species = NULL, ...)

plotlyYieldRelative(sim, object_original, species = NULL, ...)
```

## Arguments

- sim:

  A MizerSim object

- object_original:

  A MizerParams or MizerSim object to calculate differences from.

- species:

  The species to be selected. Optional. By default all target species
  are selected. A vector of species names, or a numeric vector with the
  species indices, or a logical vector indicating for each species
  whether it is to be selected (TRUE) or not.

- ...:

  Parameters passed to `getYield()`

## Examples

``` r
plotYieldRelative(NS_sim, NS_params)
#> Warning: Removed 48 rows containing missing values or values outside the scale range
#> (`geom_line()`).

plotYieldRelative(NS_sim, NS_params, species = c("Cod", "Sole"))
```
