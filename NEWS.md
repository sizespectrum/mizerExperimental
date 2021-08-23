# mizerExperimental 2.2.1

* Much polishing of `tuneParams()` and some added documentation.
* First stab at a `validSim()` function that will be useful when simulations
  produce non-finite values.
* Graduated the updated `setBevertonHolt()` and the new `getReproductionLevel()`
  to mizer package.
* Removed `dislayFrames()`, `getBiomassFrame()` and `getSSBFrame()` because
  these are superseeded by the new `plotDataFrame()` in mizer.
* Removed the vignette explaining size-spectrum dynamics in single-species 
  example because this has graduated to mizer.
* Improvements to argument checks in `getYieldVsF()`

# mizerExperimental 2.2.0

The version number will now always be that of the latest mizer package that
this version of mizerExperimental requires.

* Graduated `animateSpectra()`, `addSpecies()`, `removeSpecies()` and
  `renameSpecies()` to mizer package.
* `newSheldonParams()` has graduated to mizer package under the new name
  `newSingleSpeciesParams()`.
* Improvements to `tuneParams()`, but still work in progress.


# mizerExperimental 0.1.2

* Graduated `projectToSteady()`, `constantEggRDI()`, `customFunction()`
  and `compareParams()` to mizer package.
* Added a `NEWS.md` file to track changes to the package.
* Vignette explaining size-spectrum dynamics in single-species example.
* `removeSpecies()` work also with 3d pred kernel.
* `removeSpecies()` to handles `gear_params` correctly
