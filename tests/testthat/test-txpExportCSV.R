##----------------------------------------------------------------------------##
## txpExportCSV tests
##----------------------------------------------------------------------------##

## toxpiR csv tests
test_that("We can export toxpiR format csv files accurately", {
  #valid model exports
  expect_silent({
    data_exported <- tempfile()
    csv <- txpImportCSV(file.path("csvFiles", "txp_example_input_CI.csv"))
    txpTransFuncsUpper(txpSlices(csv$model)$ExampleA) <- NULL
    txpExportCSV(
      fileName = data_exported,
      input = csv$input,
      model = csv$model,
      id.var = NULL,
      fills = csv$fills,
      format = "toxpiR")
    data_imported <- txpImportCSV(data_exported)
    res <- txpCalculateScores(data_imported$model, data_imported$input, id.var = 1)
    res2 <- txpCalculateScores(csv$model, csv$input, id.var = 1)
  })
  expect_identical(as.data.frame(res), as.data.frame(res2))

  expect_silent({
    gui <- suppressWarnings(txpImportCSV(file.path("guiFiles", "gui_output_data.csv")))
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = NULL,
      fills = gui$fills,
      format = "toxpiR")
    data_imported <-suppressWarnings(txpImportCSV(data_exported))
    res <- txpCalculateScores(data_imported$model, data_imported$input, id.var = 1)
    res2 <- txpCalculateScores(gui$model, gui$input, id.var = 1)
  })
  expect_identical(suppressWarnings(as.data.frame(res)), suppressWarnings(as.data.frame(res2)))
  
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = csv$input,
      model = csv$model,
      id.var = NULL,
      fills = NULL)
  })
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = csv$input,
      model = csv$model,
      id.var = NULL,
      fills = "red")
  })
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = NULL,
      fills = rep("red", 10))
  })
})

##----------------------------------------------------------------------------##
## gui csv tests
test_that("We can export gui format csv files accurately", {
  #valid model exports
  expect_silent(data_exported <- tempfile())
  expect_warning({
    gui <- txpImportCSV(file.path("guiFiles", "gui_output_data.csv"))
  }, "columns are duplicated")
  expect_silent({
    suppressWarnings(txpTransFuncs(txpSlices(gui$model)$Slice1) <- NULL)
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui")
  })
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = NULL,
      format = "gui")
  })
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = "red",
      format = "gui")
  })
  expect_silent({
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = rep("red", 10),
      format = "gui")
  })
  
  #warning model exports
  expect_warning({
    test_model <- gui$model
    txpTransFuncs(test_model)[[1]] <- function(x) log10(x)
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui"
    )
  }, "Model contains slice-level")
  
  expect_warning({
    expect_warning({
      test_model <- gui$model
      txpTransFuncs(txpSlices(test_model)[[1]])[[1]] <- function(x) -x
    }, "columns are duplicated")
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui"
    )
  }, "contains both missing and negative")
  
  warnings <- testthat::capture_warnings({
    test_input <- gui$input
    test_input[, 6] <- -test_input[, 6]
    txpExportCSV(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui"
    )
  })
  expect_true(any(grepl("Slice1", warnings)))
  expect_true(any(grepl("Slice3", warnings)))
  expect_true(any(grepl("Slice4", warnings)))

  #invalid model exports
  expect_error({
    test_model <- gui$model
    txpWeights(test_model)[1] <- 0.5
    txpExportCSV(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui"
    )
  }, "integer weights")
  
  expect_error({
    negativeHandling(gui$model) <- "keep"
    txpExportCSV(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui")
  }, "negativeHandling")
})

##----------------------------------------------------------------------------##
## invalid function tests

test_that("Incorrect functions/files return error message", {
  expect_silent(data_exported <- tempfile())
  expect_error({
    txpExportCSV(
      fileName = 1,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = "gui")
  }, "fileName")
  expect_error({
    txpExportCSV(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = c("gui", "toxpiR"))
  }, "format")
  expect_error({
    txpExportCSV(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills,
      format = "INVALID")
  }, "Invalid format")
  expect_error({
    csv <- txpImportCSV(file.path("csvFiles", "txp_example_input_CI.csv"))
    txpExportCSV(
      fileName = data_exported,
      input = csv$input,
      model = csv$model,
      id.var = NULL,
      fills = csv$fills,
      format = "gui")
  }, "confidence intervals")
  expect_error({
    txpExportCSV(
      fileName = data_exported,
      input = csv$input,
      model = csv$model,
      id.var = 'INVALID',
      fills = csv$fills)
  }, "id.var")
})