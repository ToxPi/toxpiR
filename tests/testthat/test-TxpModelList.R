##----------------------------------------------------------------------------##
## TxpModelList tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpModelList objects", {
  expect_silent(mdl <- TxpModel(TxpSliceList(s1 = TxpSlice("hello"))))
  expect_s4_class(TxpModelList(mdl, mdl), "TxpModelList")
  expect_s4_class(TxpModelList(m1 = mdl, m2 = mdl), "TxpModelList")
  expect_error(TxpModelList(mdl, NULL))
  expect_error(TxpModelList(NULL))
  expect_error(TxpModelList(3))
  expect_length(TxpModelList(mdl, mdl, mdl), 3)
  expect_named(TxpModelList(mdl, m = mdl, mdl), c('', 'm', ''))
})

##----------------------------------------------------------------------------##
## Show

test_that("TxpModelList show method displays correct information", {
  expect_silent({
    mdl <- TxpModel(TxpSliceList(s1 = TxpSlice("hello")))
    l <- TxpModelList(m1 = mdl, mdl, m3 = mdl)
  })
  expect_output(print(l), "TxpModelList of length 3")
  expect_output(print(l), "m1 '' m3")
  expect_silent(names(l) <- NULL)
  expect_output(print(l), "'' '' ''")
})

##----------------------------------------------------------------------------##
## Concatenation

test_that("We can concatenate TxpModelList objects", {
  expect_silent({
    mdl <- TxpModel(TxpSliceList(s1 = TxpSlice("hello")))
    l <- TxpModelList(m1 = mdl, mdl, m3 = mdl)
  })
  expect_s4_class(cl <- c(l, rev(l), l), "TxpModelList")
  expect_length(cl, 9)
  expect_named(cl, c('m1', '', 'm3', 'm3', '', 'm1', 'm1', '', 'm3'))
})

##----------------------------------------------------------------------------##
## Coercion

test_that("We can coerce to TxpModelList objects", {
  expect_silent({
    mdl <- TxpModel(TxpSliceList(s1 = TxpSlice("hello")))
    l <- list(m1 = mdl, mdl, m3 = mdl)
  })
  expect_s4_class(as.TxpModelList(l), "TxpModelList")
  expect_s4_class(as.TxpModelList(l[[1]]), "TxpModelList")
})

##----------------------------------------------------------------------------##
## Duplicated

test_that("We can detect duplicate TxpModel objects in TxpModelList", {
  m1 <- TxpModel(c(S1 = TxpSlice("inpt1")))
  m2 <- TxpModel(c(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2")))
  m3 <- TxpModel(c(S1 = TxpSlice("inpt1")), 2)
  expect_false(any(duplicated(TxpModelList(m1 = m1, m2 = m2, m3 = m3))))
  expect_true(any(duplicated(TxpModelList(m1 = m1, m2 = m1))))
})
