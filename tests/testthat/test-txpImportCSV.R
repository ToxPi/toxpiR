##----------------------------------------------------------------------------##
## txpImportCSV tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## toxpiR csv tests

test_that("We can upload toxpiR format csv files accurately", {
  expect_silent({
    csv <- txpImportCSV(file.path("csvFiles", "txp_example_input_CI.csv"))
    res <- txpCalculateScores(csv$model, csv$input, id.var = 1)
    res2 <- txpCalculateScores(txp_example_model_CI, txp_example_input_CI, id.var = 1)
  })
  expect_identical(suppressWarnings(as.data.frame(res)), suppressWarnings(as.data.frame(res2)))
  
  csv <- txpImportCSV(file.path("csvFiles", "csv_gui_funcs.csv"))
  
  expect_silent({ #model with only bounds for a slice
    data_exported <- tempfile()
    test_model <- txp_example_model_CI
    expect_warning({
      txpValueNames(txpSlices(test_model)$ExampleA) <- NULL
    }, "Setting <txpTransFuncs> to NULL")
    txpExportCSV(
      fileName = data_exported,
      input = txp_example_input_CI,
      model = test_model,
      id.var = NULL)
    data_imported <- txpImportCSV(data_exported)
  })
})

##----------------------------------------------------------------------------##
## gui csv tests

test_that("We can upload gui format csv files accurately", {
  #valid file with duplicated metrics
  expect_warning({
    l <- txpImportCSV(file.path("guiFiles", "gui_output_data.csv"))
  }, "duplicated")
  expect_type(l, "list")
  expect_s4_class(l$model, "TxpModel")
  expect_named(l$model, c("Slice1", "Slice2", "Slice3", "Slice4"))
  expect_s3_class(l$input, "data.frame")
  expect_named(l$input, 
               c("row", "SID", "CASRN", "Name", "metric1", 
                 "metric2", "metric4", "metric3"))
})

##----------------------------------------------------------------------------##
## invalid file tests

test_that("Incorrect files return error message", {
  expect_error(txpImportCSV(file.path("guiFiles", "gui_bad_input.csv")))
  expect_error(txpImportCSV(file.path("guiFiles", 
                                      "gui_output_missingFuncs.csv")), 
               "hitcall\\(x\\), function\\(x\\), f\\(x\\), hello\\(x\\)")
  expect_error({
    txpImportCSV(file.path("csvFiles", "bad_csv_nonnumeric.csv"))
  }, "Slice3_U2")
  expect_warning(
    expect_error({
      txpImportCSV(file.path("guiFiles", "gui_output_nonNumeric.csv"))
    }, "metric2, metric4"), 
    "columns are duplicated")
  
  expect_error({ #numeric indicator
    data_exported <- tempfile()
    csv <- as.data.frame(c(1,2))
    write.csv(csv, file = data_exported)
    txpImportCSV(data_exported)
  })
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_empty_slice.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_indicator.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_slice_func.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_indicator.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_color.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_weight.csv")))
  expect_error(txpImportCSV(file.path("csvFiles", "gui_csv_empty_rank.csv")))
})