# Removes species with abundance below a threshold

This function simply removes the low-abundance species from the params
object. It does not recalculate the steady state for the remaining
species or retune their reproductive efficiencies.

## Arguments

- params:

  A MizerParams object

- cutoff:

  Species with an abundance at maturity size that is less than cutoff
  times community abundance will be removed. Default 1e-3.

- ...:

  Not used.

## Value

An object of type `MizerParams`

## Details

This is a generic function with a method for objects of class
MizerParams.
