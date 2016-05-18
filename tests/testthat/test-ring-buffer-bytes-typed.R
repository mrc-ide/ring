context("ring_buffer_bytes_typed (typed)")

test_that("basic", {
  size <- 100
  n <- 7
  buf <- ring_buffer_bytes_typed(size, integer(n))

  expect_equal(buf$size(), size)
  expect_equal(buf$stride(), sizes[["integer"]] * n)

  x1 <- as.integer(sample(100, n))

  buf$push(x1)
  expect_equal(buf$used(), 1)
  expect_equal(buf$used(TRUE), n * sizes[["integer"]])

  expect_identical(buf$tail_data(), x1)
  expect_identical(buf$tail_offset_data(0), x1)

  expect_equal(buf$head_pos(), 1)
  expect_equal(buf$head_pos(TRUE), sizes[["integer"]] * n)

  x2 <- matrix(as.integer(sample(100, n * 3)), 3, byrow=FALSE)
  buf$push(x2)

  expect_identical(buf$read(4), c(x1, x2))

  p <- buf$head_pos(TRUE)
  expect_error(buf$push(1L:2L), "Incorrect size data")
  expect_identical(buf$head_pos(TRUE), p)
})
