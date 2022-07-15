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
  expect_warning({
    gui <- txpImportGui(file.path("guiFiles", "gui_output_data.csv"))
  })
  # No warnings/errors expected for original imported file
  expect_silent({
    txpExportGui(
      fileName = data_exported,
      input = gui$input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills
    )
  })
  # Non-integer weights
  test_model <- gui$model
  expect_warning({
    # See Note1 above
    txpWeights(test_model)[1] <- 0.5
  })
  expect_error({
    txpExportGui(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills
    )
  })
  # Slice-level transformation function
  test_model <- gui$model
  expect_warning({
    # See Note1 above
    txpTransFuncs(test_model)[[1]] <- function(x) log10(x)
  })
  expect_warning({
    txpExportGui(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills
    )
  })
  # Input-level transformation function that creates negative values
  test_model <- gui$model
  expect_warning({
    # See Note1 above
    txpTransFuncs(txpSlices(test_model)[[1]])[[1]] <- function(x) -x
  })
  expect_warning({
    txpExportGui(
      fileName = data_exported,
      input = gui$input,
      model = test_model,
      id.var = 'Name',
      fills = gui$fills
    )
  })
  # Negative input values, expect 3 warnings for the 3 affected slices
  test_input <- gui$input
  test_input[, 6] <- -test_input[, 6]
  expect_warning(expect_warning(expect_warning({
    txpExportGui(
      fileName = data_exported,
      input = test_input,
      model = gui$model,
      id.var = 'Name',
      fills = gui$fills
    )
  })))
})
