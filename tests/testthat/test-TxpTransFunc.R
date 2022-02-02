##----------------------------------------------------------------------------##
## TxpTransFunc tests
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Initialization

test_that("We can create TxpTransFunc objects", {
  fx <- function(x) x + 1
  expect_s4_class(tf <- TxpTransFunc(fx), "TxpTransFunc")
  expect_s4_class(as(fx, "TxpTransFunc"), "TxpTransFunc")
  expect_condition(body(tf) == "x + 1", regexp = NA)
  expect_equal(formalArgs(tf), "x")
  expect_equal(tf(1:10), 1:10 + 1)
  expect_error(TxpTransFunc(function(x) "hello"))
  expect_error(TxpTransFunc(function(x) x + "a"))
  expect_error(TxpTransFunc(1))
})

test_that("TxpTransFunc can handle primitives", {
  expect_warning(f1 <- TxpTransFunc(sqrt))
  expect_equal(f1(1:10), sqrt(1:10))
  expect_warning(f2 <- as(sqrt, "TxpTransFunc"))
  expect_equal(f2(1:10), sqrt(1:10))
})
