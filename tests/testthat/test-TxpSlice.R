##----------------------------------------------------------------------------##
## TxpSlice tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpSlice objects", {
  vnames <- c("input1", "input2", "input3")
  lnames <- c("input4", "input5", "input6")
  unames <- c("input7", "input8", "input9")
  invalnames <- c("x", "x", "y")
  tfuncs <- TxpTransFuncList(f1 = TxpTransFunc(), f2 = NULL, f3 = NULL)
  expect_s4_class(TxpSlice(txpValueNames = vnames), "TxpSlice")
  expect_s4_class(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs,
                           txpLowerNames = lnames, txpLowerFuncs = tfuncs,
                           txpUpperNames = unames, txpUpperFuncs = tfuncs), 
                  "TxpSlice")
  expect_warning(recycle <- TxpSlice(c("a", "b", "c"), function(x) x))
  expect_s4_class(recycle, "TxpSlice")
  expect_length(txpTransFuncs(recycle), 3)
  expect_error(TxpSlice())
  expect_error(TxpSlice(NA))
  expect_error(TxpSlice(NULL), 
               "At least one of txpValueNames")
  expect_error(TxpSlice(invalnames))
  expect_error(TxpSlice(vnames, tfuncs[1:2]))
  expect_error(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs,
                        txpLowerNames = invalnames, txpLowerFuncs = tfuncs), 
               "TxpSlice")
  expect_error(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs,
                        txpLowerNames = invalnames, txpLowerFuncs = tfuncs[1:2]), 
               "TxpSlice")
  expect_error(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs,
                        txpUpperNames = invalnames, txpUpperFuncs = tfuncs[1:2]), 
               "TxpSlice")
  expect_error(TxpSlice(txpValueNames = vnames, txpTransFuncs = tfuncs,
                        txpUpperNames = vnames, txpUpperFuncs = tfuncs[1:2]), 
               "TxpSlice")
  
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
  expect_warning(txpValueNames(sl) <- c("a", "b"), "Length of new <txpValueNames> greater than old length. Assuming extra have txpTransFuncs NULL. Please check txpTransFuncs<TxpSlice>.")
  expect_warning(txpValueNames(sl) <- "Length of new <txpValueNames> less than old length. Removing excess txpTransFuncs. Please check txpTransFuncs<TxpSlice>.")
  expect_error(txpValueNames(sl) <- NULL)
  expect_s4_class({txpTransFuncs(sl) <- function(x) x; sl}, "TxpSlice")
  expect_s4_class({names(txpTransFuncs(sl)) <- "linear"; sl}, "TxpSlice")
  expect_equal(txpTransFuncs(sl)[[1]](10), 10)
  expect_named(txpTransFuncs(sl), "linear")
  expect_warning(txpLowerNames(sl) <- c("input3", "input4"), "Length of new <txpLowerNames> greater than old length. Assuming extra have txpLowerFuncs NULL. Please check txpLowerFuncs<TxpSlice>.")
  expect_warning(txpLowerNames(sl) <- "input3", "Length of new <txpLowerNames> less than old length. Removing excess txpLowerFuncs. Please check txpLowerFuncs<TxpSlice>.")
  expect_s4_class({txpLowerFuncs(sl) <- function(x) x; sl}, "TxpSlice")
  expect_s4_class({names(txpLowerFuncs(sl)) <- "linear"; sl}, "TxpSlice")
  expect_equal(txpLowerFuncs(sl)[[1]](10), 10)
  expect_named(txpLowerFuncs(sl), "linear")
  expect_warning(txpLowerNames(sl) <- NULL, "Setting <txpLowerFuncs> to NULL to match <txpLowerNames>")
  expect_warning(txpUpperNames(sl) <- c("input3", "input4"), "Length of new <txpUpperNames> greater than old length. Assuming extra have txpUpperFuncs NULL. Please check txpUpperFuncs<TxpSlice>.")
  expect_warning(txpUpperNames(sl) <- "input3", "Length of new <txpUpperNames> less than old length. Removing excess txpUpperFuncs. Please check txpUpperFuncs<TxpSlice>.")
  expect_s4_class({txpUpperFuncs(sl) <- function(x) x; sl}, "TxpSlice")
  expect_s4_class({names(txpUpperFuncs(sl)) <- "linear"; sl}, "TxpSlice")
  expect_equal(txpUpperFuncs(sl)[[1]](10), 10)
  expect_named(txpUpperFuncs(sl), "linear")
  expect_warning(txpUpperNames(sl) <- NULL, "Setting <txpUpperFuncs> to NULL to match <txpUpperNames>")
  expect_warning(txpUpperNames(sl) <- c("input3"), "Length of new <txpUpperNames> greater than old length. Assuming extra have txpUpperFuncs NULL. Please check txpUpperFuncs<TxpSlice>.")
  expect_warning(txpValueNames(sl) <- NULL, "Setting <txpTransFuncs> to NULL to match <txpValueNames>")
})


##----------------------------------------------------------------------------##
## Show

test_that("TxpSlice shows correct information", {
  sl <- TxpSlice(c("input1", "input2"), list(f1 = function(x) x, NULL))
  expect_output(print(sl), "txpValueNames\\(2\\)")
  expect_output(print(sl), "input1 input2")
  expect_output(print(sl), "txpTransFuncs\\(2\\)")
  expect_output(print(sl), "f1 NULL")
  
  sl <- TxpSlice(txpLowerNames = "input3", txpUpperNames = "input4")
  expect_output(print(sl), "txpValueNames\\(0\\)")
  expect_output(print(sl), "NULL")
  expect_output(print(sl), "txpTransFuncs\\(0\\)")
  expect_output(print(sl), "NULL")
  expect_output(print(sl), "txpLowerNames\\(1\\)")
  expect_output(print(sl), "input3")
  expect_output(print(sl), "txpLowerFuncs\\(1\\)")
  expect_output(print(sl), "NULL")
  expect_output(print(sl), "txpUpperNames\\(1\\)")
  expect_output(print(sl), "input4")
  expect_output(print(sl), "txpUpperFuncs\\(1\\)")
  expect_output(print(sl), "NULL")
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
