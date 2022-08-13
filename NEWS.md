# mizerExperimental 2.3.1

## New plotting functions

* `plotDeath()`
* `plotEnergyBudget()`
* `plotResourceLevel()`
* `plotResourcePred()`
* `plotYieldVsSize()`
* Also `plotYieldVsF()` has been improved.

## Other new functions

* `singleSpeciesSteady()`
* `alignResource()`

## Changes in `tuneParams()`
* Allow tabs to set their title, see issue #37
* `resourceControl()` now works also when the resource dynamics are constant.
* Reproduction plot now calculates the reproduction success correctly also for
  non-zero reproduction level.
* Clean-up of `spectraTab` code. Now button does not trigger already when it is
  displayed but only when it is pressed.
* Call `setBevertonHolt()` only for the focus species.
* The feature whereby one can call `tuneParams()` without a parameter to recover
  it from log files after a crash is now working again.
* The catch tab now has the same functionality for calibration and matching as
  the biomass tab.
* When changing predation kernel parameters, the search volume is also 
  rescaled to keep the same encounter rate for larvae.
* Now one can also match abundances to observed numbers instead of observed
  biomasses.
* Extracted more of the plotting code into plot functions.
* Corrected the plotting of observed catch.
* Corrected the y-axis scale in density plots.
* New `abundanceTab()` showing both biomasses and numbers.
* Double-clicking on a species on the biomass graph now changes its biomass to
  the clicked value rather than the observed value.
* Separated spectra and biomass tabs.
* An observed biomass of 0 (not possible) is now converted to NA
* Using the same theme as the mizer website
* Beginnings of an example tab that can be used as a well-documented base for
  new custom tabs. Needs to be expanded to get better documentation.

## Other changes
* Allow mizer extensions to overwrite the `steady()` function.



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

* New `plotBiomassVsSpecies()` and `plotYieldVsSpecies()`.
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
