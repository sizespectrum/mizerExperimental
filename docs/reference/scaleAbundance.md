# Rescale Abundance

Multiplies the abundances of all or of selected species by given factors
and then retunes the reproductive efficiencies accordingly.

## Usage

``` r
scaleAbundance(params, factor, ...)
```

## Arguments

- params:

  A mizer params object

- factor:

  The factor by which the abundance of each species is multiplied. This
  can be specified in two ways:

  - A named numeric vector where the name indicates the species and the
    value gives the factor for that species. Only the named species are
    affected.

  - A number that gives the factor for all foreground species.

- ...:

  Not used.

## Value

An object of type MizerParams

## Details

Does not run the system to steady state. For that you should call
[`steady()`](https://sizespectrum.org/mizer/reference/steady.html)
explicitly afterwards.

This is a generic function with a method for objects of class
MizerParams.
