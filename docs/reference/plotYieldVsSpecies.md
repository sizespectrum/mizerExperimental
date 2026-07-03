# Plot the yield against species

This is a generic function with a method for objects of class
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html).

## Usage

``` r
plotYieldVsSpecies(params, gear = NULL, ...)
```

## Arguments

- params:

  A MizerParams object

- gear:

  Optional. The name of a gear. If supplied, only the yield from this
  gear will be displayed.

- ...:

  Not used.

## Examples

``` r
plotYieldVsSpecies(NS_params)
#> Warning: Removed 9 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

```
