# Match observed yields

This function matches the observed yields of all the gears for all the
species by scaling the catchabilities by the ratio between current
modelled yield and observed yield.

## Usage

``` r
matchYield(
  params,
  species = NULL,
  gears = NULL,
  keep = c("egg", "biomass", "number"),
  ...
)
```

## Arguments

- params:

  A MizerParams object

- species:

  The species to be affected. Optional. By default all observed
  biomasses will be matched. A vector of species names, or a numeric
  vector with the species indices, or a logical vector indicating for
  each species whether it is to be affected (TRUE) or not.

- gears:

  The gears to be affected. Optional. By default all gears will be
  affected. A vector of gear names.

- keep:

  A string determining which quantity is to be kept constant. The
  choices are "egg" which keeps the egg density constant, "biomass"
  which keeps the total biomass of the species constant and "number"
  which keeps the total number of individuals constant.

- ...:

  Not used.

## Value

A MizerParams object with updated catchabilities

## Details

This is a generic function with a method for objects of class
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html).
