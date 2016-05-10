## These are tests that are not part of the upstream ringbuf library
## but are added for the R version.  I'll switch the organisation
## around later, perhaps.

context("circle (r)")

## buf2 <- circle_buffer_clone(buf)


test_that("tail offset", {
  bytes <- as.raw(0:255)
  buf <- circle_buffer_create(length(bytes))
  circle_buffer_memcpy_into(buf, bytes)
  expect_true(circle_buffer_full(buf))

  n <- 20

  cmp <- as.raw(seq_len(n) - 1L)
  expect_equal(circle_buffer_tail_read(buf, n), cmp)
  tmp <- vapply(seq_len(n) - 1L,
                function(x) circle_buffer_tail_offset(buf, x), raw(1))
  expect_equal(tmp, cmp)
  expect_equal(circle_buffer_memcpy_from(buf, n), cmp)

  cmp <- as.raw(as.integer(cmp) + n)
  expect_equal(circle_buffer_tail_read(buf, n), cmp)

  tmp <- vapply(seq_len(n) - 1L,
                function(x) circle_buffer_tail_offset(buf, x), raw(1))
  expect_equal(tmp, cmp)

  expect_error(circle_buffer_tail_offset(buf, 300),
               "Buffer underflow")

  expect_equal(circle_buffer_tail_offset(buf, 255 - n), as.raw(255))
  expect_error(circle_buffer_tail_offset(buf, 255 - n + 1),
               "Buffer underflow")
})
