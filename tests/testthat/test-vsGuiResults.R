##----------------------------------------------------------------------------##
## txpExportGui tests
##----------------------------------------------------------------------------##

test_that("We reproduce GUI results", {
  
  # Get expected results from GUI output
  resExpected <- read.csv(file.path("guiFiles", "gui_test_results.csv"), check.names = FALSE)
  resExpected <- resExpected[order(resExpected$Name), -c(2:5)]
  names(resExpected) <- c('score', sapply(strsplit(names(resExpected)[-1], '!'), '[', 1))

  ##------------------------------##
  # Using txpImportGui
  ##------------------------------##
  
  # Load data and model
  expect_warning({
    gui1 <- txpImportGui(file.path("guiFiles", "gui_test_data.csv"))
  })
  
  # Compute scores
  expect_silent({
    res1 <- txpCalculateScores(gui1$model, gui1$input)
  })
  
  # Compare ToxPi Scores
  expect_equal(
    txpScores(res1),
    resExpected$score,
    ignore_attr = TRUE
  )
  
  # Compare Slice Scores
  expect_equal(
    as.data.frame(txpSliceScores(res1, adjusted = FALSE)),
    resExpected[,-1],
    ignore_attr = TRUE
  )
  
  ##------------------------------##
  # Using output from txpExportGui
  ##------------------------------##
  
  # Export model, suppress expected warnings about containing negative values
  data_exported <- tempfile()
  expect_silent({
    suppressWarnings(
      txpExportGui(
        fileName = data_exported,
        input = gui1$input,
        model = gui1$model,
        id.var = 'Name',
        fills = gui1$fills
      )
    )
  })
  
  # Load data and model
  expect_silent({
    gui2 <- txpImportGui(data_exported)
  })
  
  # Compute scores
  expect_silent({
    res2 <- txpCalculateScores(gui2$model, gui2$input)
  })
  
  # Compare ToxPi Scores
  expect_equal(
    txpScores(res2),
    resExpected$score,
    ignore_attr = TRUE
  )
  
  # Compare Slice Scores
  expect_equal(
    as.data.frame(txpSliceScores(res2, adjusted = FALSE)),
    resExpected[,-1],
    ignore_attr = TRUE
  )
  
  ##------------------------------##
  # Manually created model
  ##------------------------------##
  
  # Create model
  input <- read.csv(file.path("guiFiles", "gui_test_data.csv"), skip = 40, check.names = FALSE, stringsAsFactors = FALSE)

  slices <- TxpSliceList()
  nFn <- 10
  for ( i in 1:nFn ) {
    fn <- switch(
      i,
      function(x) x,                                        # 1: linear(x)
      function(x) as.integer(x != 0),                       # 2: hit count
      function(x) ifelse(x <= 0, NA, -log10(x)),            # 3: -log10(x)
      function(x) ifelse(x <= 0, NA, -log10(x) + log10(max(x, na.rm = TRUE))), # 4: -log10(x)+log10(max(x))
      function(x) ifelse(x <= 0, NA, -log10(x) + 6),        # 5: -log10(x)+6
      function(x) ifelse(x <= 0, NA, -log(x)),              # 6: -ln(x)
      function(x) ifelse(x <= 0, NA, log10(x)),             # 7: log10(x)
      function(x) sqrt(x),                                  # 8: sqrt(x)
      function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE), # 9:  zscore(x)
      function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)), # 10: uniform(x)
      function(x) x                                         # default
    )
    addSlices <- TxpSliceList(
      b = TxpSlice(c("y1a", "y1b"), TxpTransFuncList(fn, fn)),
      a = TxpSlice("y1a", TxpTransFuncList(fn)),
      d = TxpSlice(c("y2a", "y2b"), TxpTransFuncList(fn, fn)),
      c = TxpSlice("y2a", TxpTransFuncList(fn))
    )
    names(addSlices) <- paste0('Slice', i, c('_1ab', '_1a', '_2ab', '_2a'))
    slices <- c(slices, addSlices)
  }
  
  expect_warning({
    model <- TxpModel(
      txpSlices = slices,
      txpWeights = rep(c(2,1,2,1), nFn)
    )
  })
  
  # Compute scores
  expect_silent({
    res3 <- txpCalculateScores(model, input, negative.value.handling = 'missing')
  })
  
  # Compare ToxPi Scores
  expect_equal(
    txpScores(res3),
    resExpected$score,
    ignore_attr = TRUE
  )
  
  # Compare Slice Scores
  expect_equal(
    as.data.frame(txpSliceScores(res3, adjusted = FALSE)),
    resExpected[,-1],
    ignore_attr = TRUE
  )
  
})
