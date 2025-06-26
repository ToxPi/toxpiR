##----------------------------------------------------------------------------##
## txpImportGui tests
##----------------------------------------------------------------------------##

test_that("We can import GUI outputs", {
  warnings <- testthat::capture_warnings({
    l <- txpImportGui(file.path("guiFiles", "gui_output_data.csv"))
  })
  expect_true(any(grepl("deprecated", warnings)))
  expect_true(any(grepl("columns are duplicated", warnings)))
  
  expect_type(l, "list")
  expect_s4_class(l$model, "TxpModel")
  expect_named(l$model, c("Slice1", "Slice2", "Slice3", "Slice4"))
  expect_s3_class(l$input, "data.frame")
  expect_named(l$input, 
               c("row", "SID", "CASRN", "Name", "metric1", 
                 "metric2", "metric4", "metric3"))
  expect_warning(expect_error(txpImportGui(file.path("guiFiles", "gui_bad_input.csv"))), 
                 "deprecated")
  expect_warning(expect_error(txpImportGui(file.path("guiFiles", 
                                      "gui_output_missingFuncs.csv")), 
               "hitcall\\(x\\), function\\(x\\), f\\(x\\), hello\\(x\\)"),
               "deprecated")
  expect_warning(
    expect_warning(
      expect_error({
        txpImportGui(file.path("guiFiles", "gui_output_nonNumeric.csv"))
      }, "metric1, metric3"), 
      "columns are duplicated"),
    "deprecated")
  # expect_silent({
  #   dl <- txpImportGui(file.path("guiFiles", "gui_distributions.csv"))
  #   expect_warning(txpCalculateScores(dl$model, dl$input), "NaNs produced")
  # })
})
