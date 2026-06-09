# Plot the energy budget of each species through size.

This budget is divided between growth, income, metabolic loss and
reproduction.

## Usage

``` r
plotlyEnergyBudget(object, species = NULL, logarithmic = TRUE, ...)
```

## Arguments

- object:

  An object of class MizerSim or MizerParams.

- species:

  The name of the predator species for which to plot the mortality.

- logarithmic:

  A boolean value that determines whether values should be displayed
  logarithmicly or linearly. Default is TRUE.

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

- ...:

  Other arguments (currently unused)

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a list
composed of two slots is returned. First slot is a data frame with the
four variables 'w', 'value', 'Type', 'Species and the second slot is a
data frame with the five variables 'w_mat', 'w_max', 'Species',
'y_coord', 'Type' (to plot vertical lines).

## Details

This is a generic function with methods for objects of class MizerSim
and MizerParams. The method for a `MizerSim` object plots the energy
budget in the initial state of the simulation. An interactive version is
available as `plotlyEnergyBudget()`.

## See also

[plotting_functions](https://sizespectrum.org/mizer/reference/plotting_functions.html)

Other plotting functions:
[`plotDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerExperimental/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md),
[`plotYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsF.md),
[`plotYieldVsSize()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSize.md)

## Examples

``` r
# \donttest{
plotEnergyBudget(NS_params, species = "Cod")


# Returning the data frame
fr <- plotEnergyBudget(NS_params, return_data = TRUE)
str(fr)
#> List of 2
#>  $ :'data.frame':    3736 obs. of  4 variables:
#>   ..$ w      : num [1:3736] 0.001 0.00119 0.00142 0.0017 0.00203 ...
#>   ..$ value  : num [1:3736] 0.039 0.0437 0.0489 0.0548 0.0614 ...
#>   ..$ Type   : chr [1:3736] "Growth" "Growth" "Growth" "Growth" ...
#>   ..$ Species: Factor w/ 12 levels "Sprat","Sandeel",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ :'data.frame':    12 obs. of  5 variables:
#>   ..$ w_mat  : int [1:12] 13 4 23 99 21 75 78 39 105 165 ...
#>   ..$ w_max  : num [1:12] 33 36 100 334 324 ...
#>   ..$ Species: Factor w/ 12 levels "Sprat","Sandeel",..: 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ y_coord: num [1:12] 59.5 103.9 330.1 537.2 611.7 ...
#>   ..$ Type   : logi [1:12] NA NA NA NA NA NA ...
# }
```
