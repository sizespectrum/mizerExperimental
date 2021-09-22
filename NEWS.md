# mizerExperimental 2.3.0

* New `tuneGrowth()` shiny gadget to help tune growth curves.

## Changes in `tuneParams()`

* Important change: The updated species abundance is now always calculated in
the unchanged background.
* Using `params` instead of `p` as argument name.
* Many more help popups.
* Main panel utilises all available space.
* Add buttons and keyboard shortcuts for cycling through species.
* Move tabs and controls into individual files and add roxygen comments.
* New `match` argument that determines whether biomasses or yields or neither
should be matched at each press of the `steady` button.
* Remove the ability to upload params objects.
* New `growthControl` controls.
* New `biomassTab` tab.
* Controls can now also update species sliders.
* By default preserves `erepro`.
* Scaling of background now happens by clicking on the slider without need for
a 'Go' button.
* Double-clicking on a growth curve plot now toggles between the panel view and
the individual species view.
* Do not overrride ggplot2 theme, just adjust font size.
* Rename `cutoff_size` to `biomass_cutoff` everywhere.
* Many more changes.

## Other changes

* New `plotBiomassVsSpecies()` and `plotYieldVsSpecies`.
* Egg density slider is now updated by run to steady.
* Improved handling of missing values in `plotBiomassVsSpecies()`.
* New `scaleDownBackground()` and `removeBackgroundspecies()`.

## Developments moved to core mizer
* New `mizer::plotBiomassObservedVsModel()`
* New `mizer::calibrateBiomass()`, `mizer::matchBiomasses()`, `mizer::calibrateYield()`,
`mizer::matchYields()`, `mizer::scaleModel()`.


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
