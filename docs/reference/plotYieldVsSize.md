# Plot the size distribution of the catch

Plots the normalised number density of the catch for a species as a
function of either length or weight. In addition to the catch in the
model, also the observed catch will be plotted if it is supplied via the
`catch` argument. Also superimposes a plot of the number density of all
individuals of the species.

## Usage

``` r
plotYieldVsSize(
  object,
  species = NULL,
  gear = NULL,
  catch = NULL,
  x_var = c("Weight", "Length"),
  return_data = FALSE,
  ...
)

plotlyYieldVsSize(
  object,
  species = NULL,
  gear = NULL,
  catch = NULL,
  x_var = c("Weight", "Length"),
  ...
)
```

## Arguments

- object:

  An object of class MizerSim or MizerParams.

- species:

  The name of the predator species for which to plot the mortality.

- gear:

  Optional. The name of a gear. If supplied, only the yield from this
  gear will be displayed.

- catch:

  Data frame holding binned observed catch data. The data can be binned
  either into length bins or weight bins. In the former case the data
  frame should have columns `length` and `dl` holding the start of the
  size bins in cm and the width of the size bins in cm respectively. In
  the latter case the data frame should have columns `weight` and `dw`
  holding the start of the size bins in grams and the width of the size
  bins in grams. The data frame also needs to have the columns `species`
  (the name of the species), `gear` (the name of the gear) and `catch`
  (the number of individuals of a particular species caught by a
  particular gear in a size bin).

- x_var:

  Determines whether to show the size distribution of the catch as a
  function of weight ("Weight") or as a function of length ("Length").
  Default is "Weight".

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

- ...:

  Other arguments (currently unused)

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a list
composed of two slots is returned. First slot is a data frame with the
four variables 'w' or 'l' (depending on `x_var`), 'Catch density',
'Type', 'Species and the second slot is a data frame with the four
variables 'w_mat', 'Species', 'y_coord', 'Type' (to plot vertical
lines).

## Details

This is a generic function with methods for objects of class MizerSim
and MizerParams. The method for a `MizerSim` object plots the catch in
the initial state of the simulation. An interactive version is available
as `plotlyYieldVsSize()`.

## See also

[plotting_functions](https://sizespectrum.org/mizer/reference/plotting_functions.html)

Other plotting functions:
[`plotDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md),
[`plotEnergyBudget()`](https://sizespectrum.org/mizerExperimental/reference/plotEnergyBudget.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerExperimental/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md),
[`plotYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsF.md)

## Examples

``` r
# \donttest{
plotYieldVsSize(NS_params, species = "Cod")


# Returning the data frame
fr <- plotYieldVsSize(NS_params, species = "Cod", return_data = TRUE)
str(fr)
#> List of 2
#>  $ :'data.frame':    73 obs. of  4 variables:
#>   ..$ w            : num [1:73] 14 16.7 19.9 23.8 28.4 ...
#>   ..$ Catch density: num [1:73] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ Type         : chr [1:73] "Model catch" "Model catch" "Model catch" "Model catch" ...
#>   ..$ Species      : Factor w/ 12 levels "Sprat","Sandeel",..: 11 11 11 11 11 11 11 11 11 11 ...
#>  $ :'data.frame':    1 obs. of  4 variables:
#>   ..$ w_mat  : int 1606
#>   ..$ Species: Factor w/ 12 levels "Sprat","Sandeel",..: 11
#>   ..$ y_coord: num 0.00027
#>   ..$ Type   : logi NA
# }
```
