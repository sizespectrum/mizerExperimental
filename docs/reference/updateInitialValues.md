# Update the initial values

Recalculates the steady-state abundances in a fixed background given by
the current abundances, keeping the abundances fixed in the smallest
size class for each species. Then readjusts the `erepro` values.

## Usage

``` r
updateInitialValues(params)
```

## Arguments

- params:

  A MizerParams object

## Value

The MizerParams object with updated `initial_n` and `initial_n_pp`
slots.
