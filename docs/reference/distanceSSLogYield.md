# Measure distance between current and previous state in terms of yield.

Calculates the proportional difference between getYield() outputs of
current and previous state. This function can be used in
projectToSteady() to decide when sufficient convergence to steady state
has been achieved.

## Usage

``` r
distanceSSLogYield(params, current, previous, criterion = "SSE")
```

## Arguments

- params:

  MizerParams

- current:

  A named list with entries `n`, `n_pp` and `n_other` describing the
  current state

- previous:

  A named list with entries `n`, `n_pp` and `n_other` describing the
  previous state

- criterion:

  TODO: document

## Value

proportional difference between current and previous state
