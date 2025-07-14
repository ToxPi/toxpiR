##----------------------------------------------------------------------------##
## TxpModel tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpModel objects", {
  expect_silent({
    slcLst <- list(S1 = TxpSlice("input1"), S2 = TxpSlice("input2"))
    txpSlcLst <- TxpSliceList(S1 = TxpSlice("input1"), S2 = TxpSlice("input2"))
    fxnLst <- list(f1 = function(x) x, f2 = function(x) x^2)
    txpFxnLst <- TxpTransFuncList(f1 = function(x) x, f2 = function(x) x^2)
    wrnLst <- TxpSliceList(S1 = TxpSlice("input1"), S2 = TxpSlice("input1"))
  })
  expect_s4_class(TxpModel(slcLst), "TxpModel")
  expect_s4_class(TxpModel(txpSlcLst), "TxpModel")
  expect_s4_class(TxpModel(txpSlices = slcLst, txpTransFuncs = fxnLst), 
                  "TxpModel")
  expect_s4_class(TxpModel(txpSlices = slcLst, txpTransFuncs = txpFxnLst), 
                  "TxpModel")
  expect_s4_class(TxpModel(txpSlices = slcLst, txpTransFuncs = txpFxnLst, negativeHandling = "missing", rankTies = "first"), 
                  "TxpModel")
  expect_error(TxpModel(txpSlices = txpSlcLst, txpWeights = 1))
  expect_error(TxpModel(txpSlices = txpSlcLst, txpWeights = "1"))
  expect_error(TxpModel(txpSlices = txpSlcLst, txpTransFuncs = txpFxnLst[1]))
  expect_error(TxpModel(txpSlices = txpSlcLst, negativeHandling = "error"))
  expect_error(TxpModel(txpSlices = txpSlcLst, rankTies = "error"))
  expect_warning(TxpModel(wrnLst))
})

##----------------------------------------------------------------------------##
## Accessors

test_that("TxpModel accessors return expected slots", {
  expect_silent({
    sl <- TxpSliceList(S1 = TxpSlice("input1", txpLowerNames = "input3"), 
                       S2 = TxpSlice("input2"))
    md <- TxpModel(sl)
  })
  expect_s4_class(txpSlices(md), "TxpSliceList")
  expect_equal(unname(txpWeights(md)), rep(1, 2))
  expect_equal(unname(txpWeights(md, adjust = TRUE)), rep(0.5, 2))
  expect_s4_class(txpTransFuncs(md), "TxpTransFuncList")
  expect_equal(negativeHandling(md), "keep")
  expect_equal(rankTies(md), "average")
  expect_equal(txpValueNames(md), list(S1 = "input1", S2 = "input2"))
  expect_equal(txpValueNames(md, simplify = TRUE), 
               c(S1 = "input1", S2 = "input2"))
  expect_equal(txpLowerNames(md), list(S1 = "input3", S2 = NULL))
  expect_equal(txpLowerNames(md, simplify = TRUE), 
               c(S1 = "input3", S2 = NULL))
  expect_equal(txpUpperNames(md), NULL)
  expect_equal(txpUpperNames(md, simplify = TRUE), 
               c(S1 = NULL, S2 = NULL))
  expect_named(md, c("S1", "S2"))
  expect_length(md, 2)
})

##----------------------------------------------------------------------------##
## Replace

test_that("We can replace TxpModel slots", {
  expect_silent({
    sl1 <- TxpSliceList(S1 = TxpSlice("input1"), S2 = TxpSlice("input2"))
    sl2 <- TxpSliceList(S1 = TxpSlice("input1"), S3 = TxpSlice("input3"))
    md <- TxpModel(sl1)
    fl <- TxpTransFuncList(f1 = function(x) x, f2 = function(x) sqrt(x))
  })
  expect_s4_class(txpSlices(md) <- sl2, "TxpSliceList")
  expect_named(txpSlices(md), c("S1", "S3"))
  expect_error(txpSlices(md) <- c("A", "B"))
  expect_error(txpSlices(md) <- c(sl1, sl2[2]))
  expect_silent(txpWeights(md) <- 1:2)
  expect_equal(unname(txpWeights(md)), 1:2)
  expect_named(txpWeights(md), c("S1", "S3"))
  expect_silent(txpTransFuncs(md) <- fl)
  expect_named(txpTransFuncs(md), c("f1", "f2"))
  expect_silent(txpTransFuncs(md) <- as.list(fl)[2:1])
  expect_named(txpTransFuncs(md), c("f2", "f1"))
  expect_silent(txpTransFuncs(md) <- NULL)
  expect_equal(txpTransFuncs(md), TxpTransFuncList(NULL, NULL))
  md <- TxpModel(c(sl1, sl2[2]))
  names(md) <- c("A", "B", "C")
  expect_named(md, c("A", "B", "C"))
  expect_error(names(md) <- "hello")
  names(md)[2] <- "hello"
  expect_named(md, c("A", "hello", "C"))
  names(md)[2:3] <- c("B", "hello")
  expect_named(md, c("A", "B", "hello"))
  expect_silent(negativeHandling(md) <- "missing")
  expect_equal(negativeHandling(md), "missing")
  expect_silent(rankTies(md) <- "first")
  expect_equal(rankTies(md), "first")
})


##----------------------------------------------------------------------------##
## Show

test_that("TxpModel show method displays correct information", {
  mdl <- TxpModel(txpSlices = TxpSliceList(S1 = TxpSlice("inpt1"), 
                                           S2 = TxpSlice("input2")),
                  txpWeights = 1:2,
                  txpTransFuncs = list(f1 = function(x) x, NULL))
  expect_output(print(mdl), "txpSlices\\(2\\)")
  expect_output(print(mdl), "S1 S2")
  expect_output(print(mdl), "txpWeights\\(2\\)")
  expect_output(print(mdl), "1 2")
  expect_output(print(mdl), "txpTransFuncs\\(2\\)")
  expect_output(print(mdl), "f1 NULL")
  expect_output(print(mdl), "negativeHandling")
  expect_output(print(mdl), "keep")
  expect_output(print(mdl), "rankTies")
  expect_output(print(mdl), "average")
})

##----------------------------------------------------------------------------##
## Merge

test_that("We can merge two TxpModel objects", {
  expect_silent({
    m1 <- TxpModel(txpSlices = c(S1 = TxpSlice("inpt1"), 
                                 S2 = TxpSlice("inpt2")),
                   txpWeights = 1:2,
                   txpTransFuncs = list(NULL, linear = function(x) x))
    m2 <- TxpModel(txpSlices = c(S3 = TxpSlice("inpt3"), 
                                 S4 = TxpSlice("inpt4")),
                   txpWeights = 2:1,
                   txpTransFuncs = list(linear = function(x) x, 
                                        sqrt = function(x) sqrt(x)))
    m3 <- TxpModel(c(S1 = TxpSlice("inpt4")))
    m4 <- TxpModel(c(S4 = TxpSlice("inpt1")), txpWeights = 3)
    m5 <- TxpModel(txpSlices = c(S3 = TxpSlice("inpt3"), 
                                 S4 = TxpSlice("inpt4")),
                   negativeHandling = "missing")
    m6 <- TxpModel(txpSlices = c(S3 = TxpSlice("inpt3"), 
                                 S4 = TxpSlice("inpt4")),
                   rankTies = "first")
  })
  expect_s4_class(mrg1 <- merge(m1, m2), "TxpModel")
  expect_length(mrg1, 4)
  expect_named(mrg1, c("S1", "S2", "S3", "S4"))
  expect_equal(names(txpTransFuncs(mrg1)), c("", "linear", "linear", "sqrt"))
  expect_equal(txpTransFuncs(mrg1)[[4]](100), 10)
  expect_equal(txpTransFuncs(mrg1)[[2]](100), 100)
  expect_error(txpTransFuncs(mrg1)[[1]](100))
  expect_error(merge(m1, m3))
  expect_error(merge(m1, m5))
  expect_error(merge(m1, m6))
  expect_warning(merge(m1, m4))
})

