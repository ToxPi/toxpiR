##----------------------------------------------------------------------------##
## txpExportGui tests
##----------------------------------------------------------------------------##

test_that("We can export GUI-ready files", {
  
  # Note1
  # Modifying transformation functions in the model causes the "columns are duplicated"
  # message to be repeated, I'm not sure if that is the desired response
  
  # Output file for txpExportGui()
  data_exported <- tempfile()
  # Load data and model
  warnings <- testthat::capture_warnings({
    gui <- txpImportGui(file.path("guiFiles", "gui_output_data.csv"))
  })
  
  expect_true(any(grepl("deprecated", warnings)))
  expect_true(any(grepl("columns are duplicated", warnings)))

  # Deprecated warning expected for original imported file
  expect_warning({
    txpExportGui(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills
    )
  }, "deprecated")
  # Non-integer weights
  test_model <- gui$model
  expect_silent({
    # See Note1 above
    txpWeights(test_model)[1] <- 0.5
  })
  expect_warning(
    expect_error({
      txpExportGui(
        fileName = data_exported,
        input = gui$input,
        model = test_model,
        id.var = 'Name',
        fills = gui$fills
      )
    }), "deprecated")
  # Slice-level transformation function
  test_model <- gui$model
  expect_silent({
    # See Note1 above
    txpTransFuncs(test_model)[[1]] <- function(x) log10(x)
  })
  expect_warning(
    expect_warning({
      txpExportGui(
        fileName = data_exported,
        input = gui$input,
        model = test_model,
        id.var = 'Name',
        fills = gui$fills
      )
    }, "Model contains slice-level"),
    "deprecated")
  # Input-level transformation function that creates negative values
  test_model <- gui$model
  expect_warning({ 
    txpTransFuncs(txpSlices(test_model)[[1]])[[1]] <- function(x) -x
  }, "duplicated")
  expect_warning(
    expect_warning({
      txpExportGui(
        fileName = data_exported,
        input = gui$input,
        model = test_model,
        id.var = 'Name',
        fills = gui$fills
      )
    }, "contains both missing and negative"),
    "deprecated")
  
  # Negative input values, expect 3 warnings for the 3 affected slices
  test_input <- gui$input
  test_input[, 6] <- -test_input[, 6]
  warnings <- testthat::capture_warnings({
    txpExportGui(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills
    )
  })

  expect_true(any(grepl("Slice1", warnings)))
  expect_true(any(grepl("Slice3", warnings)))
  expect_true(any(grepl("Slice4", warnings)))
  expect_true(any(grepl("deprecated", warnings)))

  # Invalid negativeHandling, expect an error since GUI only allows 'missing'
  suppressWarnings(negativeHandling(gui$model) <- "keep")
  expect_warning(expect_error(
    txpExportGui(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills
    )
  ), "deprecated")
})
