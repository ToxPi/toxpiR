##----------------------------------------------------------------------------##
## TxpSlice tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpSlice objects", {
  vnames <- c("input1", "input2", "input3")
  tfuncs <- TxpTransFuncList(f1 = TxpTransFunc(), f2 = NULL, f3 = NULL)
  expect_s4_class(TxpSlice(txpValueNames = vnames), "TxpSlice")
  expect_s4_class(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs), 
                  "TxpSlice")
  expect_warning(recycle <- TxpSlice(c("a", "b", "c"), function(x) x))
  expect_s4_class(recycle, "TxpSlice")
  expect_length(txpTransFuncs(recycle), 3)
  expect_error(TxpSlice())
  expect_error(TxpSlice(NA))
  expect_error(TxpSlice(NULL))
  expect_error(TxpSlice(c("x", "x")))
  expect_error(TxpSlice(vnames, tfuncs[1:2]))
})

##----------------------------------------------------------------------------##
## Accessors

test_that("TxpSlice accessors return expected slots", {
  sl <- TxpSlice(txpValueNames = c("input1", "input2", "input3"),
                 txpTransFuncs = TxpTransFuncList(f1 = TxpTransFunc(), 
                                                  f2 = NULL, 
                                                  f3 = NULL))
  expect_s4_class(txpTransFuncs(sl), "TxpTransFuncList")
  expect_equal(txpValueNames(sl), c("input1", "input2", "input3"))
})

##----------------------------------------------------------------------------##
## Replace

test_that("We can replace TxpSlice slots", {
  sl <- TxpSlice("input1")
  expect_s4_class({txpValueNames(sl) <- "input2"; sl}, "TxpSlice")
  expect_equal(txpValueNames(sl), "input2")
  expect_error(txpValueNames(sl) <- 3)
  expect_error(txpValueNames(sl) <- c("a", "b"))
  expect_s4_class({txpTransFuncs(sl) <- function(x) x; sl}, "TxpSlice")
  expect_s4_class({names(txpTransFuncs(sl)) <- "linear"; sl}, "TxpSlice")
  expect_equal(txpTransFuncs(sl)[[1]](10), 10)
  expect_named(txpTransFuncs(sl), "linear")
})


##----------------------------------------------------------------------------##
## Show

test_that("TxpSlice shows correct information", {
  sl <- TxpSlice(c("input1", "input2"), list(f1 = function(x) x, NULL))
  expect_output(print(sl), "txpValueNames\\(2\\)")
  expect_output(print(sl), "input1 input2")
  expect_output(print(sl), "txpTransFuncs\\(2\\)")
  expect_output(print(sl), "f1 NULL")
})

##----------------------------------------------------------------------------##
## Length

test_that("TxpSlice length returns correct length", {
  expect_equal(length(TxpSlice(letters)), 26)
  expect_equal(length(TxpSlice(letters[1:5])), 5)
})

##----------------------------------------------------------------------------##
## Merge

test_that("We can merge two TxpSlice objects", {
  s1 <- TxpSlice(c("input1", "input2"), list(NULL, linear = function(x) x))
  s2 <- TxpSlice(c("input3", "input4"), list(NULL, linear = function(x) x))
  expect_s4_class(smrg <- merge(s1, s2), "TxpSlice")
  expect_equal(txpValueNames(smrg), c("input1", "input2", "input3", "input4"))
  expect_named(txpTransFuncs(smrg), c("", "linear", "", "linear"))
})
