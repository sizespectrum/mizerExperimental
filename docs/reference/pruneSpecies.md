# Removes species with abundance below a threshold

This function simply removes the low-abundance species from the params
object. It does not recalculate the steady state for the remaining
species or retune their reproductive efficiencies.

## Usage

``` r
pruneSpecies(params, cutoff = 0.001)
```

## Arguments

- params:

  A MizerParams object

- cutoff:

  Species with an abundance at maturity size that is less than cutoff
  times community abundance will be removed. Default 1e-3.

## Value

An object of type `MizerParams`
