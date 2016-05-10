context("stride")

test_that("empty", {
  buf <- circle_buffer_create(100, 5)

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
})
