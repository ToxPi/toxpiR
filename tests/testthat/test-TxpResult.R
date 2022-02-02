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
  expect_equal(txpRanks(sort(res)), 1:10)
  expect_equal(txpRanks(sort(res, decreasing = FALSE)), 10:1)
  expect_s4_class(txpSlices(res), "TxpSliceList")
  expect_length(txpSlices(res), 4)
  expect_equal(round(txpScores(res), 6),
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
  expect_silent(names(res) <- NULL)
  expect_error(res["hello"])
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
  txpIDs(res) <- NULL
  expect_warning(woID <- as.data.frame(res))
  expect_s3_class(woID, "data.frame")
  expect_named(woID, c("score", "rank", sprintf("s%d", 1:4)))
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
## Plot -- TxpResult, missing

test_that("We can make ToxPi diagrams", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    data(txp_example_model, package = "toxpiR")
    res <- txpCalculateScores(model = txp_example_model, 
                              input = txp_example_input, 
                              id.var = "name")
  })
  expect_silent(plot(res))
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


