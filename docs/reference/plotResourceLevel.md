# Plot the proportion of the resource spectrum(s) compared to their carrying capacity

Plot the proportion of the resource spectrum(s) compared to their
carrying capacity

## Usage

``` r
plotResourceLevel(object, return_data = FALSE)
```

## Arguments

- object:

  An object of class MizerSim or MizerParams.

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a data
frame with the three variables 'w', 'value', 'Resource' is returned.

## See also

[mizer::plotting_functions](https://sizespectrum.org/mizer/reference/plotting_functions.html)

Other plotting functions:
[`plotDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md),
[`plotEnergyBudget()`](https://sizespectrum.org/mizerExperimental/reference/plotEnergyBudget.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md),
[`plotYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsF.md),
[`plotYieldVsSize()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSize.md)

## Examples

``` r
# \donttest{
plotResourceLevel(NS_params)


# Returning the data frame
fr <- plotResourceLevel(NS_params, return_data = TRUE)
str(fr)
#> 'data.frame':    179 obs. of  3 variables:
#>  $ w       : num  2.12e-13 2.53e-13 3.02e-13 3.61e-13 4.30e-13 ...
#>  $ value   : num  1 1 1 1 1 ...
#>  $ Resource: chr  "Resource" "Resource" "Resource" "Resource" ...
# }
```
