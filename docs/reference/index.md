# Package index

## Plotting functions

- [`plotBiomassFlux()`](https://sizespectrum.org/mizerExperimental/reference/plotBiomassFlux.md)
  : Plot biomass flux
- [`plotBiomassRelative()`](https://sizespectrum.org/mizerExperimental/reference/plotBiomassRelative.md)
  [`plotlyBiomassRelative()`](https://sizespectrum.org/mizerExperimental/reference/plotBiomassRelative.md)
  **\[deprecated\]** : Plot change in biomass over time
- [`plotBiomassVsSpecies()`](https://sizespectrum.org/mizerExperimental/reference/plotBiomassVsSpecies.md)
  : Plot the biomass against species
- [`plotDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md)
  [`plotlyDeath()`](https://sizespectrum.org/mizerExperimental/reference/plotDeath.md)
  : Plot the sources of external, predation and fishing mortality per
  species and size
- [`plotEnergyBudget()`](https://sizespectrum.org/mizerExperimental/reference/plotEnergyBudget.md)
  [`plotlyEnergyBudget()`](https://sizespectrum.org/mizerExperimental/reference/plotEnergyBudget.md)
  : Plot the energy budget of each species through size.
- [`plotNumberVsSpecies()`](https://sizespectrum.org/mizerExperimental/reference/plotNumberVsSpecies.md)
  : Plot the number against species
- [`plotResourceLevel()`](https://sizespectrum.org/mizerExperimental/reference/plotResourceLevel.md)
  : Plot the proportion of the resource abundance compared to the
  resource carrying capacity
- [`plotResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md)
  [`plotlyResourcePred()`](https://sizespectrum.org/mizerExperimental/reference/plotResourcePred.md)
  : Plot the mortality applied on the resource spectrum(s)
- [`plotYieldRelative()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldRelative.md)
  [`plotlyYieldRelative()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldRelative.md)
  **\[deprecated\]** : Plot change in yield over time
- [`plotYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsF.md)
  : Plot a yield-vs-F curve
- [`plotYieldVsSize()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSize.md)
  [`plotlyYieldVsSize()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSize.md)
  : Plot the size distribution of the catch
- [`plotYieldVsSpecies()`](https://sizespectrum.org/mizerExperimental/reference/plotYieldVsSpecies.md)
  : Plot the yield against species
- [`biomass_callback()`](https://sizespectrum.org/mizerExperimental/reference/biomass_callback.md)
  : Callback function for plotting biomass in real-time

## Helper functions for model tuning

- [`matchYield()`](https://sizespectrum.org/mizerExperimental/reference/matchYield.md)
  : Match observed yields
- [`adjustBackgroundSpecies()`](https://sizespectrum.org/mizerExperimental/reference/adjustBackgroundSpecies.md)
  : Retunes abundance of background species.
- [`scaleAbundance()`](https://sizespectrum.org/mizerExperimental/reference/scaleAbundance.md)
  : Rescale Abundance
- [`scaleDownBackground()`](https://sizespectrum.org/mizerExperimental/reference/scaleDownBackground.md)
  : Scale background down by a factor
- [`updateInitialValues()`](https://sizespectrum.org/mizerExperimental/reference/updateInitialValues.md)
  : Update the initial values
- [`pruneSpecies()`](https://sizespectrum.org/mizerExperimental/reference/pruneSpecies.md)
  : Removes species with abundance below a threshold
- [`getYieldVsF()`](https://sizespectrum.org/mizerExperimental/reference/getYieldVsF.md)
  : Calculate a yield-vs-F curve
- [`alignResource()`](https://sizespectrum.org/mizerExperimental/reference/alignResource.md)
  : Rescale resource to be in line with fish community

## Helper functions for `plotYieldVsF()`

- [`distanceSSLogYield()`](https://sizespectrum.org/mizerExperimental/reference/distanceSSLogYield.md)
  : Measure distance between current and previous state in terms of
  yield.
- [`yieldCalculator()`](https://sizespectrum.org/mizerExperimental/reference/yieldCalculator.md)
  : Calculates yield from steady sim
