# Callback function for plotting biomass in real-time

This function can be passed to the `callback` argument of
[`project()`](https://sizespectrum.org/mizer/reference/project.html) to
plot the biomasses of the species in real-time as the simulation runs.

## Usage

``` r
biomass_callback(sim, t_idx, ...)
```

## Arguments

- sim:

  A [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim.html)
  object.

- t_idx:

  The current time index.

- ...:

  Other arguments passed to the callback (such as `ylim`, `species`,
  `use_cutoff`, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
params <- NS_params
# Open an external graphics window to see the real-time plot in RStudio
x11()
sim <- project(params, callback = biomass_callback)
} # }
```
