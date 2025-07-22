# toxpiR 1.4.0

## New Features
#### Major update for allowing user defined confidence intervals for slices. 
* `TxpSlice` now contains slots `txpLowerNames`, `txpLowerFuncs`, `txpUpperNames`, `txpUpperFuncs` 
* `TxpSlice` now allows `txpValueNames` to be NULL as long as at least one other names slot is provided 
* `TxpModel` now has accessor functions `txpLowerNames()` and `txpUpperNames()` for viewing defined slice metrics in confidence intervals 
* `txpCalculateScores` now aggregates individual slices by averaging instead of summing 
This change mathematically does not effect slice scores of existing models without bounds, 
while also allowing confidence level scores within a slice to be accurately compared when they 
have differing number of features in their definition 
* `txpCalculateScores` now treats missing data as follows:
  * Missing data in lower bounds after aggregation and re-scaling are set to 0 
  * Missing data in main slices after aggregation and rescaling are set to the corresponding lower bound score if it exists, otherwise it is set to 0 as was done previously.
  * Missing data in the upper bound prior to aggregation but post metric transformation is set to the maximum in its column to prevent upper confidence interval deflation. 
Missing data after aggregation and rescaling is set to 1.
  * Confidence interval levels provided in one slice but not another are treated as a missing column of data per missing level.
  * Confidence interval levels not provided anywhere in the model are not considered missing and skip calculations, being assigned NULL.
* `TxpResult` now contains slots `txpScoreLows`, `txpScoreUps` for overall confidence level scores, 
`txpSliceLows`, `txpSliceUps` for slice confidence level scores, and `txpRankLows`, `txpRankUps` for confidence level rankings
* `sort()` now has an optional parameter `level` specifying whether to sort TxpResult objects by "low", "main", or "up" confidence levels
* `plot(package = "gg")` now provides functionality for plotting confidence intervals and has new and altered parameters:
  * `showLower` - a boolean for showing dotted lower confidence interval arcs if they exist in the model that defaults to TRUE
  * `showMain` - a boolean for showing color filled main slice scores if they exist in the model that defaults to TRUE
  * `showUpper` - a boolean for showing dashed upper confidence interval arcs if they exist in the model that defaults to TRUE
  * `sliceBoundColor` - a text string containing a hex code or r recognized color for coloring confidence interval arcs that defaults to "black". 
When the plot does not contain any main slice scores, the arcs will automatically be colored by `fills` to allow slice differentiation
  * `sliceLineColor` - default has been altered to "white" and is now drawn as a solid line instead of dashed
  * `showScore` - now shows overall scores for all confidence levels provided
#### Addition of csv files for toxpiR model export similar to Toxpi GUI model exports via `txpImportCSV()` and `txpExportCSV()`
* `txpImportCSV()` - function for importing both toxpiR and Java GUI exported data models. Only requires the path to the file
* `txpExportCSV()` - function for exporting models and their corresponding data to csv formats. Has several parameters:
  * `model` - ToxPi model object created using `TxpModel()` 
  * `input` - dataframe containing input data for ToxPi model
  * `id.var` - Character scalar or integer, representing column name or index in `input` that stores unique sample id. Defaults to NULL.
  * `fills` - Vector containing slice colors as either hex codes or R recognized colors. Defaults to a color scheme matching the ToxPi GUI.
  * `format` - Text string specifying whether to format the file for toxpiR or the Java GUI. Options are "toxpiR" and "gui". Default is "toxpiR". 
Restrictions exist on models capable of being exported for use with the Java GUI.
  * `fileName` - Text string to name the output csv file. Default is "txpModel.csv".
* `txpImportGUI()` - deprecated in favor of `txpImportCSV()`
* `txpExportGUI()` - deprecated in favor of `txpExportCSV(format = "gui")`

## Bux Fixes
* Fixed non-numeric warning referencing wrong columns when importing gui files
* Fixed error when plotting `TxpResult` objects when `txpIDs` is NULL 
* Fixed failing gui file imports for models where slices all contain the same number of features and the number of features per slice is greater than 1

## Improvements
* Added naming for `txpWeights` slot in `TxpModel` equivalent to names of `txpSlices` slot.
* Assigned `txpIDs` as row indices when creating `TxpResult` objects if `id.var` is set to NULL
* Required `txpIDs` in `TxpResult` objects to ensure consistency when analyzing and visualizing results
* Added more verbose warnings and errors
* Provided an error instead of warning when transformations result in non-finite data for an entire column
* Added slots `negativeHandling` and `rankTies` to txpModel for more consistent sharing and replication of results
* Deprecated txpResultParam() in favor of providing these as slots in `TxpModel`
* Allowed parameters `rank.ties.method` and `negative.value.handling` for `txpCalculateScores()` 
to overwrite their corresponding slots in `TxpModel`
* Altered input duplication warning to only occur at model initialization or when assigning `TxpSliceList` 
to avoid unnecessary warnings when altering other parts of the model
* Added example dataset, model, and results for confidence level methods
* Added functionality for changing number of metrics in a slice when changing `txpValueNames`. Same for lower and upper.
* Added more testing
* Updated readME for specifying biocManager installs

# toxpiR 1.3.0

* Added 'txpMissing' slot to TxpResult; this stores information regarding the 
  amount of missing data in the dataset per slice
* Added ggplot capabilities for plotting with several new aesthetics
* Updated vignettes
* Transferred maintainer to Jonathon F Fleming


# toxpiR 1.2.0

* Now require R>=4.0 due to reports of installation issues
* Added 'txpValueNames' method for TxpModel
* Added 'txpExportGui' function
* Added 'TxpResultParam' object and 'txpResultParam' slot to TxpResult; this
  stores the parameters controlling the calculation of the scores, e.g. the 
  rank ties method
* Modified the log transformation functions within txpImportGui to match the
  negative value handling behavior in the GUI
* Updated vignettes