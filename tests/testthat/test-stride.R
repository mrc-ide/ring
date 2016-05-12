context("stride")

test_that("empty", {
  buf <- circle_buffer_create(100, 5)

  expect_equal(circle_buffer_stride(buf), 5)

  expect_equal(circle_buffer_size(buf, TRUE), 500L)
  expect_equal(circle_buffer_size(buf, FALSE), 100L)
  expect_equal(circle_buffer_size(buf), 100L)

  expect_equal(circle_buffer_used(buf, TRUE), 0L)
  expect_equal(circle_buffer_used(buf, FALSE), 0L)
  expect_equal(circle_buffer_used(buf), 0L)

  expect_equal(circle_buffer_free(buf, TRUE), 500L)
  expect_equal(circle_buffer_free(buf, FALSE), 100L)
  expect_equal(circle_buffer_free(buf), 100L)

  expect_equal(circle_buffer_data(buf), raw(500))
  expect_equal(circle_buffer_bytes_data(buf), 501L)

  expect_equal(circle_buffer_head_pos(buf, TRUE), 0L)
  expect_equal(circle_buffer_head_pos(buf, FALSE), 0L)
  expect_equal(circle_buffer_head_pos(buf), 0L)

  expect_equal(circle_buffer_tail_pos(buf, TRUE), 0L)
  expect_equal(circle_buffer_tail_pos(buf, FALSE), 0L)
  expect_equal(circle_buffer_tail_pos(buf), 0L)

  expect_true(circle_buffer_empty(buf))
  expect_false(circle_buffer_full(buf))
})

test_that("memset", {
  size <- 100L
  stride <- 5L
  buf <- circle_buffer_create(100, 5)

  ## First, set a few entries to something nonzero:
  n <- 3L
  expect_equal(circle_buffer_memset(buf, as.raw(1), n), n * stride)

  ## Lots of checking of the state of the buffer:
  expect_false(circle_buffer_empty(buf))
  expect_false(circle_buffer_full(buf))

  expect_equal(circle_buffer_size(buf, TRUE), size * stride)
  expect_equal(circle_buffer_size(buf, FALSE), size)
  expect_equal(circle_buffer_size(buf), size)

  expect_equal(circle_buffer_used(buf, TRUE), n * stride)
  expect_equal(circle_buffer_used(buf, FALSE), n)
  expect_equal(circle_buffer_used(buf), n)

  expect_equal(circle_buffer_free(buf, TRUE), size * stride - n * stride)
  expect_equal(circle_buffer_free(buf, FALSE), size - n)
  expect_equal(circle_buffer_free(buf), size - n)

  expect_equal(circle_buffer_data(buf),
               pad(as.raw(rep(1, n * stride)), size * stride))
  expect_equal(circle_buffer_bytes_data(buf), size * stride + 1)

  expect_equal(circle_buffer_head_pos(buf, TRUE), n * stride)
  expect_equal(circle_buffer_head_pos(buf, FALSE), n)
  expect_equal(circle_buffer_head_pos(buf), n)

  expect_equal(circle_buffer_tail_pos(buf, TRUE), 0L)
  expect_equal(circle_buffer_tail_pos(buf, FALSE), 0L)
  expect_equal(circle_buffer_tail_pos(buf), 0L)

  ## Read a bit of the buffer and make sure that we're OK here.
  expect_equal(circle_buffer_tail_read(buf, 0), raw())
  expect_equal(circle_buffer_tail_read(buf, 1), as.raw(rep(1, stride)))
  expect_equal(circle_buffer_tail_read(buf, n), as.raw(rep(1, n * stride)))
  expect_error(circle_buffer_tail_read(buf, n + 1),
               "Buffer underflow")

  ## And check the tail offset works as expected
  expect_equal(circle_buffer_tail_offset(buf, 0), as.raw(rep(1, stride)))
  expect_equal(circle_buffer_tail_offset(buf, n - 1), as.raw(rep(1, stride)))
  expect_error(circle_buffer_tail_offset(buf, n),
               "Buffer underflow")

  ## Then, destructive modification: read a set of bytes:
  expect_equal(circle_buffer_memcpy_from(buf, 0), raw())
  expect_equal(circle_buffer_memcpy_from(buf, 1), as.raw(rep(1, stride)))

  expect_false(circle_buffer_empty(buf))
  expect_false(circle_buffer_full(buf))

  expect_equal(circle_buffer_head_pos(buf, TRUE), n * stride)
  expect_equal(circle_buffer_head_pos(buf, FALSE), n)
  expect_equal(circle_buffer_head_pos(buf), n)

  expect_equal(circle_buffer_tail_pos(buf, TRUE), stride)
  expect_equal(circle_buffer_tail_pos(buf, FALSE), 1L)
  expect_equal(circle_buffer_tail_pos(buf), 1L)

  expect_equal(circle_buffer_used(buf, TRUE), (n - 1) * stride)
  expect_equal(circle_buffer_used(buf, FALSE), n - 1)

  expect_equal(circle_buffer_free(buf, TRUE), (size - n + 1) * stride)
  expect_equal(circle_buffer_free(buf, FALSE), size - n + 1)

  expect_equal(circle_buffer_tail_offset(buf, 0), as.raw(rep(1, stride)))
  expect_equal(circle_buffer_tail_offset(buf, n - 2), as.raw(rep(1, stride)))
  expect_error(circle_buffer_tail_offset(buf, n - 1),
               "Buffer underflow")

  ## Read the rest:
  expect_equal(circle_buffer_memcpy_from(buf, n - 1),
               as.raw(rep(1, (n - 1) * stride)))

  expect_true(circle_buffer_empty(buf))
  expect_false(circle_buffer_full(buf))

  expect_equal(circle_buffer_head_pos(buf, TRUE), n * stride)
  expect_equal(circle_buffer_head_pos(buf, FALSE), n)
  expect_equal(circle_buffer_tail_pos(buf, TRUE), n * stride)
  expect_equal(circle_buffer_tail_pos(buf, FALSE), n)
  expect_equal(circle_buffer_used(buf, TRUE), 0)
  expect_equal(circle_buffer_used(buf, FALSE), 0)
  expect_equal(circle_buffer_free(buf, TRUE), size * stride)
  expect_equal(circle_buffer_free(buf, FALSE), size)
})
