# Plot change in yield over time

**\[deprecated\]** A similar plot can now be produced with mizer's
[`mizer::plotRelative()`](https://sizespectrum.org/mizer/reference/plotRelative.html),
which works directly on the arrays returned by
[`mizer::getYield()`](https://sizespectrum.org/mizer/reference/getYield.html),
for example `plotRelative(getYield(object_original), getYield(sim))`.
Note that
[`mizer::plotRelative()`](https://sizespectrum.org/mizer/reference/plotRelative.html)
shows the symmetric relative difference \\2(N_2 - N_1)/(N_1 + N_2)\\
rather than the percentage change relative to the reference.

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
#> Warning: `plotYieldRelative()` was deprecated in mizerExperimental 3.0.0.
#> ℹ Please use `mizer::plotRelative()` instead.
#> ℹ For example `plotRelative(getYield(object_original), getYield(sim))`.
#> Warning: Removed 48 rows containing missing values or values outside the scale range
#> (`geom_line()`).

plotYieldRelative(NS_sim, NS_params, species = c("Cod", "Sole"))
```
