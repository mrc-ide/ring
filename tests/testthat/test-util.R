context("util")

test_that("assertions work", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")

  expect_error(assert_scalar_logical(1), "must be logical")
  expect_error(assert_scalar_logical(NA), "must not be NA")
  expect_error(assert_scalar_logical(c(TRUE, TRUE)), "must be a scalar")

  expect_error(assert_function(1.5), "must be a function")
  expect_error(assert_function(list()), "must be a function")
})
