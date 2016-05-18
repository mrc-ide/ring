## These are tests that are not part of the upstream ringbuf library
## but are added for the R version.  I'll switch the organisation
## around later, perhaps.

context("ring_buffer_bytes (r)")

test_that("tail offset", {
  bytes <- as.raw(0:255)
  buf <- ring_buffer_create(length(bytes))
  ring_buffer_memcpy_into(buf, bytes)
  expect_true(ring_buffer_full(buf))

  n <- 20

  cmp <- as.raw(seq_len(n) - 1L)
  expect_equal(ring_buffer_tail_read(buf, n), cmp)
  tmp <- vapply(seq_len(n) - 1L,
                function(x) ring_buffer_tail_offset(buf, x), raw(1))
  expect_equal(tmp, cmp)
  expect_equal(ring_buffer_memcpy_from(buf, n), cmp)

  cmp <- as.raw(as.integer(cmp) + n)
  expect_equal(ring_buffer_tail_read(buf, n), cmp)

  tmp <- vapply(seq_len(n) - 1L,
                function(x) ring_buffer_tail_offset(buf, x), raw(1))
  expect_equal(tmp, cmp)

  expect_error(ring_buffer_tail_offset(buf, 300),
               "Buffer underflow")

  expect_equal(ring_buffer_tail_offset(buf, 255 - n), as.raw(255))
  expect_error(ring_buffer_tail_offset(buf, 255 - n + 1),
               "Buffer underflow")

  ## Add a bunch more bytes in so that we wrap the tail:
  cmp <- as.raw(rev(seq_len(n) - 1L))
  ring_buffer_memcpy_into(buf, cmp)
  expect_true(ring_buffer_full(buf))
})

test_that("impossible sizes", {
  expect_error(ring_buffer_bytes(0),
               "Can't create ring buffer with size 0")
  expect_error(ring_buffer_bytes(10, 0),
               "Can't create ring buffer with stride 0")
})
