# Calculates yield from steady sim

This function replaces a loop used multiple times within
[`getYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/getYieldVsF.md)

## Usage

``` r
yieldCalculator(
  params,
  effort_vec,
  idx_species,
  distance_func = distanceSSLogN,
  tol = 0.001,
  t_max = 100
)
```

## Arguments

- params:

  An object of class `MizerParams`.

- effort_vec:

  TODO: document

- idx_species:

  TODO: document

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

a vector of yield value of same length as `effort_vec`
