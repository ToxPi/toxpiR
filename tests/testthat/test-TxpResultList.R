##----------------------------------------------------------------------------##
## TxpResultList tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization/txpCalculateScores

test_that("We can create TxpResultList objects through txpCalculateScores", {
  data(txp_example_input, package = "toxpiR")
  sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                     s2 = TxpSlice(sprintf("metric%d", 3:7)))
  md1 <- TxpModel(sl, txpWeights = c(2, 1))
  md2 <- TxpModel(sl)
  md3 <- TxpModel(sl, 
                  txpTransFuncs = list(f1 = TxpTransFunc(), 
                                       f2 = TxpTransFunc()))
  ml <- TxpModelList(md1, md2, md3)
  expect_s4_class(res <- txpCalculateScores(model = ml, 
                                            input = txp_example_input, 
                                            id.var = "name"),
                  "TxpResultList")
  expect_s4_class(txpCalculateScores(model = as.list(ml),
                                     input = txp_example_input,
                                     id.var = "name"),
                  "TxpResultList")
  expect_equal(txpModel(res[[1]]), md1)
  expect_equal(txpModel(res[[2]]), md2)
  expect_equal(txpModel(res[[3]]), md3)
  expect_error(TxpResultList(NULL))
  expect_error(TxpResultList(NULL, res[[1]]))
  expect_error(TxpResultList(1))
  expect_error(txpCalculateScores(model = c(as.list(ml), "hello"),
                                  input = txp_example_input,
                                  id.var = "name"))
})

##----------------------------------------------------------------------------##
## Show

test_that("TxpResultList show method displays correct information", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                       s2 = TxpSlice(sprintf("metric%d", 3:7)))
    md1 <- TxpModel(sl, txpWeights = c(2, 1))
    md2 <- TxpModel(sl)
    md3 <- TxpModel(sl, 
                    txpTransFuncs = list(f1 = TxpTransFunc(), 
                                         f2 = TxpTransFunc()))
    ml <- TxpModelList(md1, m2 = md2, md3)
    l <- txpCalculateScores(model = ml, 
                            input = txp_example_input, 
                            id.var = "name")
  })
  expect_output(print(l), "TxpResultList of length 3")
  expect_output(print(l), "'' m2 ''")
  expect_silent(names(l) <- NULL)
  expect_output(print(l), "'' '' ''")
})

##----------------------------------------------------------------------------##
## Concatenation

test_that("We can concatenate TxpResultList objects", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                       s2 = TxpSlice(sprintf("metric%d", 3:7)))
    md1 <- TxpModel(sl, txpWeights = c(2, 1))
    md2 <- TxpModel(sl)
    md3 <- TxpModel(sl, 
                    txpTransFuncs = list(f1 = TxpTransFunc(), 
                                         f2 = TxpTransFunc()))
    ml <- TxpModelList(m1 = md1, md2, m3 = md3)
    l <- txpCalculateScores(model = ml, 
                            input = txp_example_input, 
                            id.var = "name")
  })
  expect_s4_class(cl <- c(l, rev(l), l), "TxpResultList")
  expect_length(cl, 9)
  expect_named(cl, c('m1', '', 'm3', 'm3', '', 'm1', 'm1', '', 'm3'))
})

##----------------------------------------------------------------------------##
## Duplicated

test_that("We can detect duplicate TxpResult objects in TxpResultList", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                       s2 = TxpSlice(sprintf("metric%d", 3:7)))
    md1 <- TxpModel(sl, txpWeights = c(2, 1))
    md2 <- TxpModel(sl)
    ml1 <- TxpModelList(m1 = md1, m2 = md1)
    ml2 <- TxpModelList(m1 = md1, m2 = md2)
    l1 <- txpCalculateScores(model = ml1, 
                             input = txp_example_input, 
                             id.var = "name")
    l2 <- txpCalculateScores(model = ml2, 
                             input = txp_example_input, 
                             id.var = "name")
  })
  expect_true(any(duplicated(l1)))
  expect_false(any(duplicated(l2)))
})

##----------------------------------------------------------------------------##
## Coercion

test_that("We can coerce to TxpResultList objects", {
  expect_silent({
    data(txp_example_input, package = "toxpiR")
    sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                       s2 = TxpSlice(sprintf("metric%d", 3:7)))
    md1 <- TxpModel(sl, txpWeights = c(2, 1))
    md2 <- TxpModel(sl)
    md3 <- TxpModel(sl, 
                    txpTransFuncs = list(f1 = TxpTransFunc(), 
                                         f2 = TxpTransFunc()))
    ml <- TxpModelList(m1 = md1, md2, m3 = md3)
    l <- lapply(ml, txpCalculateScores, 
                input = txp_example_input, 
                id.var = "name")
  })
  expect_s4_class(as.TxpResultList(l), "TxpResultList")
  expect_s4_class(as.TxpResultList(l[[1]]), "TxpResultList")
})

