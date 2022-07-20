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