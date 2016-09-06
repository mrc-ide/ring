## These are tests that are not part of the upstream ringbuf library
## but are added for the R version.  I'll switch the organisation
## around later, perhaps.

context("ring_buffer_bytes (r)")

test_that("tail offset", {
  bytes <- as.raw(0:255)
  buf <- ring_buffer_bytes(length(bytes))
  expect_equal(buf$push(bytes), length(bytes))
  expect_true(buf$full())

  n <- 20

  cmp <- as.raw(seq_len(n) - 1L)
  expect_equal(buf$read(n), cmp)
  tmp <- vapply(seq_len(n) - 1L,
                function(x) buf$tail_offset(x), raw(1))
  expect_equal(tmp, cmp)
  expect_equal(buf$take(n), cmp)

  cmp <- as.raw(as.integer(cmp) + n)
  expect_equal(buf$read(n), cmp)

  tmp <- vapply(seq_len(n) - 1L, buf$tail_offset, raw(1))
  expect_equal(tmp, cmp)

  expect_error(buf$tail_offset(300), "Buffer underflow")

  expect_equal(buf$tail_offset(255 - n), as.raw(255))
  expect_error(buf$tail_offset(255 - n + 1),
               "Buffer underflow")

  ## Add a bunch more bytes in so that we wrap the tail:
  cmp <- as.raw(rev(seq_len(n) - 1L))
  buf$push(cmp)
  expect_true(buf$full())
})

## This one duplicates the simple checks used in the environment based
## ring buffer:
test_that("head offset (1)", {
  n <- 10
  buf <- ring_buffer_bytes(n)
  m <- 4
  data <- as.raw(1:m)
  buf$push(data)

  expect_equal(buf$head_offset(0), data[m])
  expect_equal(buf$head_offset(1), data[m - 1])
  expect_equal(buf$head_offset(m - 1), data[1])
  expect_error(buf$head_offset(m), "Buffer underflow")
})

## This one is more involved.
test_that("head offset", {
  bytes <- as.raw(0:255)
  buf <- ring_buffer_bytes(length(bytes))
  expect_equal(buf$push(bytes), length(bytes))
  expect_true(buf$full())

  n <- 20

  cmp <- rev(bytes)[seq_len(n)]
  ## expect_equal(buf$read_head(n), cmp)
  tmp <- vapply(seq_len(n) - 1L,
                function(x) buf$head_offset(x), raw(1))
  expect_equal(tmp, cmp)

  buf$take(n)

  expect_error(buf$head_offset(300), "Buffer underflow")
  expect_equal(buf$head_offset(255 - n), bytes[n + 1])
  expect_error(buf$head_offset(255 - n + 1),
               "Buffer underflow")

  ## Add a bunch more bytes in so that we wrap the tail:
  cmp <- as.raw(rev(seq_len(n) - 1L))
  buf$push(cmp)
  expect_true(buf$full())

  tmp1 <- vapply(seq_len(256) - 1L,
                 function(x) buf$head_offset(x), raw(1))
  tmp2 <- vapply(seq_len(256) - 1L,
                 function(x) buf$tail_offset(x), raw(1))
  expect_equal(tmp1, rev(tmp2))
})

test_that("impossible sizes", {
  expect_error(ring_buffer_bytes(0),
               "Can't create ring buffer with size 0")
  expect_error(ring_buffer_bytes(10, 0),
               "Can't create ring buffer with stride 0")
})

test_that("input validation", {
  ## Truncation:
  expect_identical(ring_buffer_bytes(pi)$size(), 3L)
  expect_identical(ring_buffer_bytes(10, pi)$stride(), 3L)

  expect_error(ring_buffer_bytes(-1),
               "Expected a nonnegative value")
  expect_error(ring_buffer_bytes(-1L),
               "Expected a nonnegative value")
  expect_error(ring_buffer_bytes(Inf),
               "Expected a nonnegative value")
  expect_error(ring_buffer_bytes(NA_real_),
               "Expected a nonnegative value")
  expect_error(ring_buffer_bytes(NA_integer_),
               "Expected a nonnegative value")
})

## unusual direction:
test_that("take_head", {
  bytes <- as.raw(0:255)
  n <- length(bytes)
  buf <- ring_buffer_bytes(n)
  expect_equal(buf$push(bytes), n)
  expect_true(buf$full())

  expect_equal(buf$read_head(0), raw(0))
  expect_equal(buf$read_head(1), tail(bytes, 1))
  expect_equal(buf$read_head(2), rev(tail(bytes, 2)))
  expect_equal(buf$read_head(n), rev(bytes))
  expect_error(buf$read_head(n + 1L), "Buffer underflow")

  m <- 15
  buf$take(m)
  b2 <- sample(bytes, m)
  buf$push(b2)
  expect_equal(buf$read_head(0), raw(0))
  expect_equal(buf$read_head(1), tail(b2, 1))
  expect_equal(buf$read_head(m), rev(b2))
  cmp <- c(rev(b2), rev(bytes[-seq_len(m)]))
  expect_equal(buf$read_head(n), cmp)

  expect_equal(buf$take_head(0), raw(0))
  expect_equal(buf$read_head(n), cmp)

  expect_equal(buf$take_head(1), cmp[1])
  expect_equal(buf$read_head(n - 1), cmp[-1])

  expect_equal(buf$take_head(m - 1), cmp[2:m])
  expect_equal(buf$read_head(n - m), cmp[-seq_len(m)])
})

test_that("head() behaviour", {
  b <- ring_buffer_bytes(10)
  expect_error(b$head(), "empty")
  expect_error(b$tail(), "empty")
  b$push(as.raw(1:4))
  expect_equal(b$tail(), as.raw(1))
  expect_equal(b$tail_offset(0), as.raw(1))
  ## In contrast with the C API this returns the *most recently added
  ## element*, not the memory that will be written to next.
  expect_equal(b$head(), as.raw(4))
  expect_equal(b$head_offset(0), as.raw(4))
})

test_that("duplicate", {
  n <- 10
  buf <- ring_buffer_bytes(10)
  buf$push(1:12)
  buf$take(3)

  expect_equal(buf$head_pos(), 1) # NOTE: different to env!
  expect_equal(buf$tail_pos(), 5)
  expect_equal(buf$used(), 7)
  expect_equal(buf$size(), n)
  expect_equal(buf$read(buf$used()), as.raw(6:12))

  cpy <- buf$duplicate()

  ## Source unchanged:
  for (x in list(buf, cpy)) {
    expect_equal(x$head_pos(), 1)
    expect_equal(x$tail_pos(), 5)
    expect_equal(x$used(), 7)
    expect_equal(x$size(), n)
    expect_equal(x$read(x$used()), as.raw(6:12))
  }

  ## But we can move the two buffers independently.
  expect_equal(cpy$take(2), as.raw(6:7))
  cpy$push(13)

  expect_equal(buf$head_pos(), 1)
  expect_equal(buf$tail_pos(), 5)
  expect_equal(buf$used(), 7)
  expect_equal(buf$size(), n)
  expect_equal(buf$read(buf$used()), as.raw(6:12))

  expect_equal(cpy$head_pos(), 2)
  expect_equal(cpy$tail_pos(), 7)
  expect_equal(cpy$used(), 6)
  expect_equal(cpy$size(), n)
  expect_equal(cpy$read(cpy$used()), as.raw(8:13))
})
