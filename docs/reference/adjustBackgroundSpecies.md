# Retunes abundance of background species.

Rescales all background species in such a way that the total community
spectrum is as close to the Sheldon power law as possible. Background
species that are no longer needed are removed. The reproductive
efficiencies of all species are retuned.

## Usage

``` r
adjustBackgroundSpecies(params)
```

## Arguments

- params:

  A MizerParams object

## Value

An object of type `MizerParams`

## See also

[`markBackground()`](https://sizespectrum.org/mizerExperimental/reference/markBackground.md)
