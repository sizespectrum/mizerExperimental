# Plot a yield-vs-F curve

Calculates the yield of a species for a range of fishing mortalities for
that species while the fishing mortalities for the other species are
held fixed.

## Usage

``` r
plotYieldVsF(
  params,
  species,
  F_range,
  F_max = 1,
  F_min = 0,
  no_steps = 10,
  distance_func = distanceSSLogN,
  tol = 0.001,
  t_max = 100
)
```

## Arguments

- params:

  An object of class `MizerParams`.

- species:

  Name of the target species

- F_range:

  A sequence of fishing mortalities at which to evaluate the yield. If
  missing, it is set to
  `seq(from = 0, to = F_max, length.out = no_steps)`.

- F_max:

  The maximum fishing mortality. Used only if `F_range` is missing.

- F_min:

  The minimum fishing mortality. Used only if `F_range` is missing.

- no_steps:

  The number of steps to use. Only used if `F_range` is missing.

- distance_func:

  A function that will be called after every `t_per` years with both the
  previous and the new state and that should return a number that in
  some sense measures the distance between the states. By default this
  uses the function
  [`mizer::distanceSSLogN()`](https://sizespectrum.org/mizer/reference/distanceSSLogN.html)
  that you can use as a model for your own distance function.

- tol:

  The `projectToSteady` function stops when the relative change in the
  egg production RDI over t_per years is less than tol for every
  species.

- t_max:

  The longest time to run project to find steady state.

## Value

A ggplot object

## See also

getYieldVsF

Other plotting functions:
[`plotDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md),
[`plotEnergyBudget()`](https://sizespectrum.org/mizerExperimental/reference/plotEnergyBudget.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerExperimental/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md),
[`plotYieldVsSize()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
params <- newMultispeciesParams(NS_species_params_gears, inter)
plotYieldVsF(params, "Cod")
} # }
```
