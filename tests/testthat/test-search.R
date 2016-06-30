context("search")

test_that("empty", {
  b <- ring_buffer_bytes_typed(10, double(1))
  expect_equal(test_search_linear(b$ptr, 0), -1L)
})

test_that("trivial", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(0.5)
  expect_equal(test_search_linear(b$ptr, 0.0), -1L)
  expect_equal(test_search_linear(b$ptr, 0.5), 0L) # check exact
  expect_equal(test_search_linear(b$ptr, 1.0), 0L)
})

## I suspect that once this is working, then everything will work
## pretty happily.
test_that("two", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(c(.3, .6))
  expect_equal(test_search_linear(b$ptr, 0.0), -1L)
  expect_equal(test_search_linear(b$ptr, 0.2), -1L)
  expect_equal(test_search_linear(b$ptr, 0.3),  0L)
  expect_equal(test_search_linear(b$ptr, 0.4),  0L)
  expect_equal(test_search_linear(b$ptr, 0.6),  1L)
  expect_equal(test_search_linear(b$ptr, 0.7),  1L)
})

## A little more of the same, just to check for any further corner cases.
test_that("three", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(c(.3, .6, .9))
  expect_equal(test_search_linear(b$ptr, 0.0), -1L)
  expect_equal(test_search_linear(b$ptr, 0.2), -1L)
  expect_equal(test_search_linear(b$ptr, 0.3),  0L)
  expect_equal(test_search_linear(b$ptr, 0.4),  0L)
  expect_equal(test_search_linear(b$ptr, 0.6),  1L)
  expect_equal(test_search_linear(b$ptr, 0.7),  1L)
  expect_equal(test_search_linear(b$ptr, 0.9),  2L)
  expect_equal(test_search_linear(b$ptr, 1.0),  2L)
})

test_that("nontrivial, unwrapped", {
  b <- ring_buffer_bytes_typed(10, double(1))
  set.seed(1)
  x <- sort(runif(8))
  b$push(x)

  expect_equal(test_search_linear(b$ptr, 0.0), -1L)
  expect_equal(test_search_linear(b$ptr, 1.0), length(x) - 1L)

  ## This is rife with off-by-one issues here.  I think that the 4th
  ## case here (somewhere between the first two values) should come
  ## out with an offset of zero but I see an offset of one.  That's
  ## not good and (probably) indicates that I have one of the early
  ## offsets incorrect.  However, in general, I might be off by one.
  y <- runif(10)
  i <- vapply(y, test_search_linear, integer(1), buffer=b$ptr)
  expect_equal(i, findInterval(y, x) - 1L)
})

test_that("nontrivial, wrapped", {
  set.seed(10)
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(rep(1, 6))
  b$take(6)
  x <- sort(runif(8))
  b$push(x)

  y <- runif(10)
  i <- vapply(y, test_search_linear, integer(1), buffer=b$ptr)
  j <- findInterval(y, x) - 1L
  expect_equal(i, j)

  ## Test all the intervals:
  z <- c(0, x) + diff(c(0, x, 1)) / 2
  i <- vapply(z, test_search_linear, integer(1), buffer=b$ptr)
  j <- findInterval(z, x) - 1L
  expect_equal(i, j)

  ## Test all the values:
  i <- vapply(x, test_search_linear, integer(1), buffer=b$ptr)
  j <- findInterval(x, x) - 1L
  expect_equal(i, j)
})
