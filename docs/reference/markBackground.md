# Designate species as background species

Marks the specified set of species as background species. Background
species are handled differently in some plots and their abundance is
automatically adjusted in
[`adjustBackgroundSpecies()`](https://sizespectrum.org/mizerExperimental/reference/adjustBackgroundSpecies.md)
to keep the community close to the Sheldon spectrum.

## Usage

``` r
markBackground(object, species = NULL)
```

## Arguments

- object:

  An object of class MizerParams or MizerSim.

- species:

  The species to be selected. Optional. By default all target species
  are selected. A vector of species names, or a numeric vector with the
  species indices, or a logical vector indicating for each species
  whether it is to be selected (TRUE) or not.

## Value

An object of the same class as the `object` argument

## Examples

``` r
if (FALSE) { # \dontrun{
params <- newMultispeciesParams(NS_species_params_gears, inter)
sim <- project(params, effort=1, t_max=20, t_save = 0.2, progress_bar = FALSE)
sim <- markBackground(sim, species = c("Sprat", "Sandeel",
                                       "N.pout", "Dab", "Saithe"))
plotSpectra(sim)
} # }
```
