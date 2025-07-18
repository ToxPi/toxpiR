##----------------------------------------------------------------------------##
## TxpResult/txpCalculateScores tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## txpCalculateScores

test_that("We can create TxpResult objects through txpCalculateScores", {
  data(txp_example_input, package = "toxpiR")
  data(txp_example_model, package = "toxpiR")
  expect_s4_class(res <- txpCalculateScores(model = txp_example_model, 
                                            input = txp_example_input, 
                                            id.var = "name"),
                  "TxpResult")
  expect_warning({txpCalculateScores(model = txp_example_model,
                                     input = txp_example_input,
                                     id.var = "name",
                                     rank.ties.method = "average")},
                 "overwriting rankTies")
  expect_warning({txpCalculateScores(model = txp_example_model,
                                     input = txp_example_input,
                                     id.var = "name",
                                     negative.value.handling = "keep")},
                 "overwriting negativeHandling")             
  inf_example <- txp_example_input
  inf_example["chem4", "metric1"] <- Inf
  expect_warning(inf_res <- txpCalculateScores(model = txp_example_model, 
                                                input = inf_example, 
                                                id.var = "name"))
  expect_s4_class(inf_res, "TxpResult")
  txpValueNames(txpSlices(txp_example_model)[[2]]) <- "notInput"
  expect_error(txpCalculateScores(model = txp_example_model, 
                                  input = txp_example_input))
  txp_example_input$notInput <- "hello"
  expect_error(txpCalculateScores(model = txp_example_model, 
                                  input = txp_example_input))
  
  expect_silent({
    slcLst <- TxpSliceList(S1 = TxpSlice(txpLowerNames = "metric1", txpUpperNames = "metric2"), 
                           S2 = TxpSlice(txpLowerNames = "metric3", txpUpperNames = "metric4"))
    md <- TxpModel(slcLst)
    txpCalculateScores(md, txp_example_input, id.var = "name")
  })
  expect_silent({
    slcLst <- TxpSliceList(S1 = TxpSlice(txpLowerNames = "Invalid", txpUpperNames = "Invalid1"), 
                           S2 = TxpSlice(txpLowerNames = "metric3", txpUpperNames = "metric4"))
    md <- TxpModel(slcLst)
  })
  expect_error({txpCalculateScores(md, txp_example_input, id.var = "name")}, "Invalid, Invalid1")
})

##----------------------------------------------------------------------------##
## Accessors

test_that("TxpResult accessors return expected slots", {
  data(txp_example_input, package = "toxpiR")
  data(txp_example_model, package = "toxpiR")
  expect_s4_class(res <- txpCalculateScores(model = txp_example_model, 
                                            input = txp_example_input, 
                                            id.var = "name"),
                  "TxpResult")
  expect_s4_class(txpModel(res), "TxpModel")
  expect_type(txpScores(res), "double")
  expect_type(txpIDs(res), "character")
  expect_equal(txpIDs(res), sprintf("chem%02d", 1:10))
  expect_type(txpSliceScores(res), "double")
  expect_true(is.matrix(txpSliceScores(res)))
  expect_equal(dim(txpSliceScores(res)), c(10, 4))
  expect_equal(rowSums(txpSliceScores(res, adjusted = TRUE)), txpScores(res))
  expect_equal(apply(txpSliceScores(res, adjusted = FALSE), 2, max), 
               c(s1 = 1, s2 = 1, s3 = 1, s4 = 1))
  expect_named(txpRanks(res), names(res))
  expect_equal(unname(txpRanks(sort(res))), 1:10)
  expect_equal(unname(txpRanks(sort(res, decreasing = FALSE))), 10:1)
  expect_s4_class(txpSlices(res), "TxpSliceList")
  expect_length(txpSlices(res), 4)
  expect_named(txpScores(res), names(res))
  expect_equal(unname(round(txpScores(res), 6)),
               c(0.863316, 0.414845, 0.347997, 0.164044, 0.425231, 
                 0.585716, 0.000000, 0.719512, 0.771979, 0.470999))
  expect_equal(txpTransFuncs(res, level = "model"),
               txpTransFuncs(txpModel(res)))
  expect_equal(txpTransFuncs(res, level = "slices"),
               txpTransFuncs(txpSlices(txpModel(res))))
  expect_equal(txpTransFuncs(res, level = "slices", simplify = TRUE),
               txpTransFuncs(txpSlices(txpModel(res)), simplify = TRUE))
  expect_equal(txpValueNames(res), txpValueNames(txpSlices(txpModel(res))))
  expect_equal(txpValueNames(res, simplify = TRUE), 
               txpValueNames(txpSlices(txpModel(res)), simplify = TRUE))
  expect_type(txpMissing(res), "double")
  expect_equal(length(txpMissing(res)), length(txpSlices(res)))
  expect_true(all(txpMissing(res) >=0 & txpMissing(res) <=1))
  expect_equal(txpMissing(res), c(s1 = 0.1,s2 =0.1,s3 =0.125,s4 =0.1))
})

##----------------------------------------------------------------------------##
## Replacement

test_that("We can replace TxpResult names/txpIDs", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
    oldNms <- names(res)
    newNms <- as.character(sprintf("new%02d", 1:10))
  })
  expect_named({names(res) <- newNms; res}, newNms)
  expect_named({txpIDs(res) <- oldNms; res}, oldNms)
  expect_named({txpIDs(res)[1] <- "hello"; res[1]}, "hello")
  expect_named({names(res)[8:9] <- newNms[8:9]; res[8:9] }, newNms[8:9])
  expect_error(names(res) <- letters)
})

##----------------------------------------------------------------------------##
## Subsetting

test_that("TxpResult accessors return expected slots", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
  })
  expect_s4_class(res[1], "TxpResult")
  expect_length(res[1], 1)
  expect_named(res[1], "chem01")
  expect_s4_class(res[c(rep(TRUE, 4), rep(FALSE, 6))], "TxpResult")
  expect_length(res[c(rep(TRUE, 4), rep(FALSE, 6))], 4)
  expect_named(res[c(rep(TRUE, 4), rep(FALSE, 6))], sprintf("chem%02d", 1:4))
  expect_s4_class(res[c("chem04", "chem08")], "TxpResult")
  expect_length(res[c("chem04", "chem08")], 2)
  expect_named(res[c("chem04", "chem08")], c("chem04", "chem08"))
  expect_error(res[25])
  expect_warning(expect_length(res[c(TRUE, FALSE)], 5))
  expect_length(res["notAName"], 0)
  #expect_silent(names(res) <- NULL)
  expect_equal(length(res["hello"]), 0)
})

##----------------------------------------------------------------------------##
## Coercion

test_that("We can coerce TxpResult to data.frame", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
  })
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_equal(dim(as.data.frame(res)), c(10, 7))
  expect_named(as.data.frame(res), 
               c("id", "score", "rank", sprintf("s%d", 1:4)))
  expect_named(as.data.frame(res, 
                             id.name = "a", 
                             score.name = "b", 
                             rank.name = "c"), 
               c("a", "b", "c", sprintf("s%d", 1:4)))
  #txpIDs(res) <- NULL
  #expect_warning(woID <- as.data.frame(res))
  #expect_s3_class(woID, "data.frame")
  #expect_named(woID, c("score", "rank", sprintf("s%d", 1:4)))
})

##----------------------------------------------------------------------------##
## Show

test_that("TxpResult show method displays correct information", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
  })
  expect_output(print(res), "TxpResult of length 10")
  expect_output(print(res), "chem01 chem02 ... chem09 chem10")
})

##----------------------------------------------------------------------------##
## Calculation

test_that("TxpResult calculations are accurate", {
  expect_silent({
    data <- txpImportCSV(file.path("csvFiles", "csv_test_data.csv"))
    expect_warning({
      res <- txpCalculateScores(model = data$model, 
                                input = data$input)
    }, "id.var")
    res_df <- as.data.frame(res)
    res_df2 <- read.csv(file.path("csvFiles", "csv_test_results.csv"))
    res_df2$id <- as.character(res_df2$id)
    rownames(res_df2) <- as.character(rownames(res_df2))
    
    # Sort columns and normalize row names
    res_df   <- res_df[, sort(names(res_df))]
    res_df2  <- res_df2[, sort(names(res_df2))]
  })
  expect_equal(res_df, res_df2)
  expect_equal(txpMissing(res), c(ExampleA = 0.083333333,	
                                  ExampleB = 0.50,	
                                  ExampleC = 0.75))
})

##----------------------------------------------------------------------------##
## Sorting

test_that("TxpResult objects can be sorted", {
  expect_equal(unname(txpRanks(sort(txp_example_results_CI))), 1:10)
  expect_equal(unname(txpRankLows(sort(txp_example_results_CI, level = "low"))), 1:10)
  expect_equal(unname(txpRankUps(sort(txp_example_results_CI, level = "up"))), 1:10)
  expect_equal(unname(txpRankLows(sort(txp_example_results_CI, level = "low", decreasing = FALSE))), 10:1)
  expect_failure(expect_equal(unname(txpRanks(sort(txp_example_results_CI, level = "low"))), 1:10))
  expect_error({txpRanks(sort(txp_example_results_CI, level = "INVALID"))}, "Invalid level parameter")
})

##----------------------------------------------------------------------------##
## Invalid Calculations
test_that("Improper txpCalculateScores throw proper error", {
  expect_error({
    data <- txpImportCSV(file.path("csvFiles", "csv_test_data.csv"))
    txpCalculateScores(data$model, data$input, id.var = "INVALID")
  }, "identifier column")
  expect_error({
    data <- txpImportCSV(file.path("csvFiles", "csv_test_data.csv"))
    txpCalculateScores(data$model, data$input, id.var = "Slice1_L1")
  }, "non-unique values")
  expect_error({
    data <- txpImportCSV(file.path("csvFiles", "csv_test_data.csv"))
    txpCalculateScores(data$model, data$input, id.var = 0)
  }, "Invalid column index")
  expect_silent({
    mod <- txp_example_model_CI
    txpTransFuncs(mod)$ExampleA <- function(x) log(-x)
  })
  expect_error({
    suppressWarnings(txpCalculateScores(mod, txp_example_input_CI, id.var = 1))
  }, "ExampleA, ExampleA_low, ExampleA_up")
})

##----------------------------------------------------------------------------##
## Plot -- TxpResult, missing

test_that("We can make and edit ToxPi diagrams", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
    names <- txpIDs(res)
    #txpIDs(res) <- NULL
  })
  #expect_warning(plot(res))
  expect_silent({
    txpIDs(res) <- names
    plot(res)
  })
  expect_silent(grid.edit("pie-1", fills = NULL))
  grid.edit("pie-10::slice1", gp = gpar(fill = "#7DBC3D"))
  expect_silent(plot(res, package = "gg"))
  expect_silent(plot(res, package = "gg",fills = c("red","blue","green","magenta")))
  expect_silent(plot(res, package = "gg",showScore = FALSE))
  expect_silent(plot(res, package = "gg",ncol = 2))
  expect_silent(plot(res, package = "gg",bgcolor = "white"))
  expect_silent(plot(res, package = "gg",sliceBorderColor = NULL))
  expect_silent(plot(res, package = "gg",sliceValueColor = "#FF00FF",))
  expect_silent(plot(res, package = "gg",sliceLineColor = "#FF00FF"))
  expect_silent(plot(res, package = "gg",showMissing = FALSE))
  expect_silent(plot(res, package = "gg",showCenter = FALSE))
  expect_silent({
    res <- txp_example_results_CI
    plot(txp_example_results_CI, package = "gg")
  })
  expect_silent(plot(txp_example_results_CI, package = "gg", showLower = FALSE))
  expect_silent(plot(txp_example_results_CI, package = "gg", showMain = FALSE))
  expect_silent(plot(txp_example_results_CI, package = "gg", showUpper = FALSE))
  expect_silent(plot(txp_example_results_CI, package = "gg", sliceBoundColor = "magenta"))
  expect_warning({plot(txp_example_results_CI, package = "gg", sliceBorderColor = "black")},
                 "<sliceBorderColor> == <sliceBoundColor>")
  expect_warning({plot(txp_example_results_CI, package = "gg", borderColor = "black")},
                 "<borderColor> == <sliceBoundColor>")
})

##----------------------------------------------------------------------------##
## Plot -- TxpResult, numeric

test_that("We can make ToxPi rank plot ", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
  })
  expect_silent(plot(res, txpRanks(res)))
  expect_silent(plot(res, txpRanks(res), labels = 1:10))
})


