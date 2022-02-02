##----------------------------------------------------------------------------##
## TxpSliceList tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpSliceList objects", {
  expect_s4_class(TxpSliceList(), "TxpSliceList")
  expect_s4_class(TxpSliceList(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2")),
                  "TxpSliceList")
  expect_error(TxpSliceList(TxpSlice("inpt1")))
  expect_error(TxpSliceList(S1 = TxpSlice("inpt1"), S1 = TxpSlice("inpt2")))
  expect_error(TxpSliceList(NULL))
  expect_error(TxpSliceList("a"))
})

test_that("We can coerce list to TxpSliceList", {
  l <- list(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2"))
  expect_s4_class(as.TxpSliceList(l), "TxpSliceList")
})

##----------------------------------------------------------------------------##
## Accessors

test_that("We can access TxpSlice slots from TxpSliceList", {
  sl <- TxpSliceList(S1 = TxpSlice("inpt1"), S2 = TxpSlice(c("inpt2", "inpt3")))
  expect_type(txpValueNames(sl), "list")
  expect_length(txpValueNames(sl), 2)
  expect_type(txpValueNames(sl, simplify = TRUE), "character")
  expect_length(txpValueNames(sl, simplify = TRUE), 3)
  expect_type(txpTransFuncs(sl), "list")
  expect_length(txpTransFuncs(sl), 2)
  expect_s4_class(txpTransFuncs(sl, simplify = TRUE), "TxpTransFuncList")
  expect_length(txpTransFuncs(sl, simplify = TRUE), 3)
})

##----------------------------------------------------------------------------##
## Duplicated

test_that("We can detect duplicate TxpSlice objects in TxpSliceList", {
  s1 <- TxpSlice("inpt1")
  s2 <- TxpSlice("inpt1")
  s3 <- TxpSlice("inpt1", txpTransFuncs = function(x) x^2)
  expect_true(any(duplicated(TxpSliceList(s1 = s2, s2 = s2))))
  expect_false(any(duplicated(TxpSliceList(s1 = s1, s3 = s3))))
})

##----------------------------------------------------------------------------##
## Replacement

test_that("We can replace TxpSliceList objects", {
  expect_silent({
    s <- TxpSlice("inpt1")
    l <- TxpSliceList(s1 = s, s2 = s, s3 = s)
  })
  expect_s4_class({l[[1]] <- TxpSlice("hello"); l}, "TxpSliceList")
  expect_equal(txpValueNames(l[[1]]), "hello")
  expect_error(l[[2]] <- "a")
  expect_error(l[2] <- list(NULL))
  expect_error(l[2:3] <- list(TxpSlice("inpt2"), TxpSlice("inpt3")))
  expect_s4_class({
    l[2:3] <- list(s4 = TxpSlice("inpt2"), s5 = TxpSlice("inpt3"))
    l
  }, "TxpSliceList")
  expect_named(l, c("s1", "s2", "s3"))
  expect_equal(txpValueNames(l, simplify = TRUE), 
               c(s1 = "hello", s2 = "inpt2", s3 = "inpt3"))
  expect_named({names(l) <- c("a", "b", "c"); l}, c("a", "b", "c"))
  expect_named({names(l)[1] <- "hello"; l}, c("hello", "b", "c"))
  expect_named({names(l)[1:2] <- c("a", "hello"); l}, c("a", "hello", "c"))
  expect_length({l[2] <- NULL; l}, 2)
  expect_equal(txpValueNames(l, simplify = TRUE), c(a = "hello", c = "inpt3"))
})
