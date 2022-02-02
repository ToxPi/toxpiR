##----------------------------------------------------------------------------##
## TxpTransFuncList tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpTransFuncList objects", {
  expect_s4_class(TxpTransFuncList(TxpTransFunc()), "TxpTransFuncList")
  expect_s4_class(TxpTransFuncList(f = TxpTransFunc()), "TxpTransFuncList")
  expect_s4_class(TxpTransFuncList(function(x) x), "TxpTransFuncList")
  expect_s4_class(TxpTransFuncList(NULL), "TxpTransFuncList")
  f <- function(x) x
  expect_s4_class(l <- TxpTransFuncList(NULL, NULL, f1 = f, f, NULL), 
                  "TxpTransFuncList")
  expect_length(l, 5)
  expect_named(l, c('', '', 'f1', '', ''))
  expect_error(TxpTransFuncList(NULL, "a"))
  expect_error(TxpTransFuncList(function(x) "a"))
  expect_silent(l <- as.list(l))
  expect_s4_class(as.TxpTransFuncList(l), "TxpTransFuncList")
  expect_s4_class(as(l, "TxpTransFuncList"), "TxpTransFuncList")
  expect_s4_class(as.TxpTransFuncList(function(x) x), "TxpTransFuncList")
  expect_s4_class(as(function(x) x, "TxpTransFuncList"), "TxpTransFuncList")
})

##----------------------------------------------------------------------------##
## Show

test_that("TxpTransFuncList show method displays correct information", {
  expect_silent(f <- function(x) x)
  expect_silent(l <- TxpTransFuncList(NULL, NULL, f1 = f, f, NULL))
  expect_output(print(l), "TxpTransFuncList of length 5")
  expect_output(print(l), "NULL NULL f1 '' NULL")
  expect_silent(names(l) <- NULL)
  expect_output(print(l), "NULL NULL '' '' NULL")
})

##----------------------------------------------------------------------------##
## Concatenation

test_that("We can concatenate TxpTransFuncList objects", {
  expect_silent({
    f <- TxpTransFunc()
    l <- TxpTransFuncList(f = f, f, NULL)
  })
  expect_s4_class(cl <- c(l, rev(l), l), "TxpTransFuncList")
  expect_length(cl, 9)
  expect_named(cl, c('f', '', '', '', '', 'f', 'f', '', ''))
})

##----------------------------------------------------------------------------##
## Replacement

test_that("We can replace TxpTransFuncList objects", {
  expect_silent({
    f <- TxpTransFunc()
    l <- TxpTransFuncList(f = f, f, NULL)
  })
  expect_s4_class({l[[2]] <- function(x) x^2; l}, "TxpTransFuncList")
  expect_equal(l[[2]](10), 100)
  expect_error(l[[2]] <- "a")
  expect_error(l[[2]] <- function(x) x + "hello")
  expect_s4_class({l[2:3] <- list(function(x) x, function(x) sqrt(x)); l}, 
                  "TxpTransFuncList")
  expect_equal(l[[2]](10), 10)
  expect_equal(l[[3]](100), 10)
  expect_length({l[2] <- list(NULL); l}, 3)
  expect_output(print(l), "f NULL ''")
  expect_length({l[[1]] <- NULL; l}, 2)
  expect_output(print(l), "NULL ''")
})


