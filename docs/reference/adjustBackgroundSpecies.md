# Retunes abundance of background species.

Rescales all background species in such a way that the total community
spectrum is as close to the Sheldon power law as possible. Background
species that are no longer needed are removed. The reproductive
efficiencies of all species are retuned.

## Arguments

- params:

  A MizerParams object

- ...:

  Not used.

## Value

An object of type `MizerParams`

## Details

This is a generic function with a method for objects of class
MizerParams.

## See also

[`markBackground()`](https://sizespectrum.org/mizer/reference/markBackground.html)
