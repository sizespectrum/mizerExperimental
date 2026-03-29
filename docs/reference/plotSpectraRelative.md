# Plot the relative difference between two spectra

This plots the difference between the spectra relative to their average.
So if we denote the number density from the first object as \\N_1(w)\\
and that from the second object as \\N_2(w)\\, then this plot shows
\$\$2 (N_2(w) - N_1(w)) / (N_2(w) + N_1(w)).\$\$

## Usage

``` r
plotSpectraRelative(object1, object2, ...)

plotlySpectraRelative(object1, object2, ...)
```

## Arguments

- object1:

  An object of class MizerSim or MizerParams

- object2:

  An object of class MizerSim or MizerParams

- ...:

  Parameters passed to `plotSpectra()`

## Value

A ggplot2 object

## Details

The individual spectra are calculated by the
[`mizer::plotSpectra()`](https://sizespectrum.org/mizer/reference/plotSpectra.html)
function which is passed all additional arguments you supply. So you can
for example determine a size range over which to average the simulation
results via the `time_range` argument. See
[`mizer::plotSpectra()`](https://sizespectrum.org/mizer/reference/plotSpectra.html)
for more options.

Note that it does not matter whether the relative difference is
calculated for the number density or the biomass density or the biomass
density in log weight because the factors of \\w\\ by which the
densities differ cancels out in the relative difference.

## Examples

``` r
sim1 <- project(NS_params, t_max = 10)
sim2 <- project(NS_params, effort = 0.5, t_max = 10)
plotSpectraRelative(sim1, sim2)
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
