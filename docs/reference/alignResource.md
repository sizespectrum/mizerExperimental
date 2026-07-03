# Rescale resource to be in line with fish community

This function sets the coefficient of the resource power law so that the
resource spectrum aligns approximately with the fish community spectrum
and it then rescales the search volumes of all fish species so that
their resource encounter rate stays the same. This will therefore modify
the encounter rate with other prey.

## Usage

``` r
alignResource(params, ...)
```

## Arguments

- params:

  A MizerParams object

- ...:

  Not used.

## Value

A MizerParams object with updated resource carrying capacity, resource
initial value, resource parameter lambda and species parameter gamma.

## Details

This is a generic function with a method for objects of class
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html).
