context("search")

test_that("empty", {
  b <- ring_buffer_bytes_typed(10, double(1))
  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0, type), -1L)
  }
})

test_that("trivial", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(0.5)

  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0.0, type), -1L)
    expect_equal(test_search(b$.ptr, 0.5, type), 0L) # check exact
    expect_equal(test_search(b$.ptr, 1.0, type), 0L)
  }
})

## I suspect that once this is working, then everything will work
## pretty happily.
test_that("two", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(c(.3, .6))

  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0.0, type), -1L)
    expect_equal(test_search(b$.ptr, 0.2, type), -1L)
    expect_equal(test_search(b$.ptr, 0.3, type),  0L)
    expect_equal(test_search(b$.ptr, 0.4, type),  0L)
    expect_equal(test_search(b$.ptr, 0.6, type),  1L)
    expect_equal(test_search(b$.ptr, 0.7, type),  1L)
  }
})

## A little more of the same, just to check for any further corner cases.
test_that("three", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(c(.3, .6, .9))

  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0.0, type), -1L)
    expect_equal(test_search(b$.ptr, 0.2, type), -1L)
    expect_equal(test_search(b$.ptr, 0.3, type),  0L)
    expect_equal(test_search(b$.ptr, 0.4, type),  0L)
    expect_equal(test_search(b$.ptr, 0.6, type),  1L)
    expect_equal(test_search(b$.ptr, 0.7, type),  1L)
    ## These two are still incorrect, and I don't see why, as this
    ## works ok with more than 3 elements?  It's probably a bigger
    ## issue though.
    expect_equal(test_search(b$.ptr, 0.9, type),  2L)
    expect_equal(test_search(b$.ptr, 1.0, type),  2L)
  }
})

## The bisection case has a corner case with 3 elements so driving one
## more past it.
test_that("four", {
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(c(.2, .4, .6, .8))

  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0.0, type), -1L)
    expect_equal(test_search(b$.ptr, 0.1, type), -1L)
    expect_equal(test_search(b$.ptr, 0.2, type),  0L)
    expect_equal(test_search(b$.ptr, 0.3, type),  0L)
    expect_equal(test_search(b$.ptr, 0.4, type),  1L)
    expect_equal(test_search(b$.ptr, 0.5, type),  1L)
    expect_equal(test_search(b$.ptr, 0.6, type),  2L)
    expect_equal(test_search(b$.ptr, 0.7, type),  2L)
    expect_equal(test_search(b$.ptr, 0.8, type),  3L)
    expect_equal(test_search(b$.ptr, 0.9, type),  3L)
  }
})

test_that("nontrivial, unwrapped", {
  b <- ring_buffer_bytes_typed(10, double(1))
  set.seed(1)
  x <- sort(runif(8))
  b$push(x)

  for (type in search_types) {
    expect_equal(test_search(b$.ptr, 0.0, type), -1L)
    expect_equal(test_search(b$.ptr, 1.0, type), length(x) - 1L)

    ## This is rife with off-by-one issues here.  I think that the 4th
    ## case here (somewhere between the first two values) should come
    ## out with an offset of zero but I see an offset of one.  That's
    ## not good and (probably) indicates that I have one of the early
    ## offsets incorrect.  However, in general, I might be off by one.
    y <- runif(10)
    i <- vapply(y, test_search, integer(1), buffer=b$.ptr, type=type)
    expect_equal(i, findInterval(y, x) - 1L)
  }
})

test_that("nontrivial, wrapped", {
  set.seed(10)
  b <- ring_buffer_bytes_typed(10, double(1))
  b$push(rep(1, 6))
  b$take(6)
  x <- sort(runif(8))
  b$push(x)

  y <- runif(10)
  z <- c(0, x) + diff(c(0, x, 1)) / 2

  type <- "bisect"

  for (type in search_types) {
    i <- vapply(y, test_search, integer(1), buffer=b$.ptr, type=type)
    j <- findInterval(y, x) - 1L
    expect_equal(i, j)

    ## Test all the intervals:
    i <- vapply(z, test_search, integer(1), buffer=b$.ptr, type=type)
    j <- findInterval(z, x) - 1L
    expect_equal(i, j)

    ## Test all the values:
    i <- vapply(x, test_search, integer(1), buffer=b$.ptr, type=type)
    j <- findInterval(x, x) - 1L
    expect_equal(i, j)
  }
})
