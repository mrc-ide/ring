## TODO: a 'flush' argument that gets all and resets would be useful.

## TODO: a "read" modifier to head and tail that reads information.
## This is used by the C interface and needs only R level support.
## But the wrapping still needs dealing with so that does require C
## support.  Hold off for now.

## NOTE: There are a lot of tests here.  These are derived from the
## c-ringbuf/ringbuf-test.c file and exercise everything fairly
## carefully.  Unfortunately, having run through all of this we'll
## also have to do basically the same for the with-stride buffer too,
## along with any other corner cases that involves (which are likely
## to be many).

## NOTE: More tests will come when this holds something other than raw
## bytes.  So if we want a ring buffer of doubles or serialised R
## objects or whatever, then that will need some addiional testing and
## support.

context("circle")

test_that("initial conditions", {
  size <- 100L

  buf <- circle_buffer_create(size, 1L)
  expect_is(buf, "circle_buffer")

  expect_equal(circle_buffer_size(buf), size + 1L)
  expect_equal(circle_buffer_capacity(buf), size)
  expect_equal(circle_buffer_bytes_free(buf), 100L)
  expect_equal(circle_buffer_bytes_used(buf), 0L)
  expect_false(circle_buffer_full(buf))
  expect_true(circle_buffer_empty(buf))

  expect_equal(circle_buffer_head_pos(buf), 0L)
  expect_equal(circle_buffer_tail_pos(buf), 0L)

  expect_error(circle_buffer_head(buf),
               "Buffer is empty")
  expect_error(circle_buffer_tail(buf),
               "Buffer is empty")
  expect_null(circle_buffer_reset(buf))

  expect_error(circle_buffer_memcpy_from(buf, 10L),
               "Buffer underflow")

  rm(buf)
  gc() # no crash, ideally
})

test_that("null pointer safe", {
  size <- 100L

  buf <- circle_buffer_create(size, 1L)
  path <- tempfile()
  saveRDS(buf, path)
  buf2 <- readRDS(path)

  expect_error(circle_buffer_full(buf2),
               "circle_buffer already freed")
})

test_that("reset", {
  size <- 24L
  for (write in c(8L, size + 1L)) {
    buf <- circle_buffer_create(size, 1L)
    expect_equal(circle_buffer_memset(buf, as.raw(1), write), write)
    expect_null(circle_buffer_reset(buf))
    expect_equal(circle_buffer_size(buf), size + 1L)
    expect_equal(circle_buffer_capacity(buf), size)

    expect_equal(circle_buffer_bytes_free(buf),
                 circle_buffer_capacity(buf))
    expect_equal(circle_buffer_bytes_used(buf), 0L)

    expect_equal(circle_buffer_head_pos(buf), 0L)
    expect_equal(circle_buffer_tail_pos(buf), 0L)
  }
})

test_that("memset with zero count", {
  size <- 24L
  buf <- circle_buffer_create(size)

  expect_equal(circle_buffer_memset(buf, 1, 0), 0)
  expect_equal(circle_buffer_capacity(buf), size)
  expect_equal(circle_buffer_bytes_free(buf), circle_buffer_capacity(buf))
  expect_equal(circle_buffer_bytes_used(buf), 0)
  expect_false(circle_buffer_full(buf))
  expect_true(circle_buffer_empty(buf))
  expect_equal(circle_buffer_tail_pos(buf), circle_buffer_head_pos(buf))
})

test_that("memset a few bytes", {
  size <- 4096L

  buf <- circle_buffer_create(size)
  circle_buffer_reset(buf)
  expect_equal(circle_buffer_memset(buf, 57, 7), 7)
  expect_equal(circle_buffer_capacity(buf), size)
  expect_equal(circle_buffer_bytes_free(buf), circle_buffer_capacity(buf) - 7)
  expect_equal(circle_buffer_bytes_used(buf), 7)
  expect_false(circle_buffer_full(buf))
  expect_false(circle_buffer_empty(buf))

  expect_equal(circle_buffer_data(buf),
               pad(rep(as.raw(57), 7), size))
})

test_that("memset full capacity", {
  size <- 4096L
  buf <- circle_buffer_create(size)

  expect_equal(circle_buffer_memset(buf, 57, size), size)
  expect_equal(circle_buffer_capacity(buf), size)
  expect_equal(circle_buffer_bytes_free(buf), 0)
  expect_equal(circle_buffer_bytes_used(buf), circle_buffer_capacity(buf))
  expect_true(circle_buffer_full(buf))
  expect_false(circle_buffer_empty(buf))
  expect_equal(circle_buffer_data(buf), as.raw(rep(57, size)))
})

test_that("memset, twice", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)

  circle_buffer_reset(rb1)
  expect_equal(circle_buffer_memset(rb1, 57, 7), 7)
  expect_equal(circle_buffer_memset(rb1, 57, 15), 15)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_used(rb1), 7 + 15)
  expect_equal(circle_buffer_bytes_free(rb1),
               circle_buffer_capacity(rb1) - (7 + 15))
  expect_false(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))

  expect_equal(circle_buffer_data(rb1),
               pad(rep(as.raw(57), 7 + 15), size))
})

test_that("memset, twice, to full capacity", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)

  circle_buffer_reset(rb1)
  expect_equal(circle_buffer_memset(rb1, 57, size - 1L), size - 1L)
  expect_equal(circle_buffer_memset(rb1, 57, 1L), 1L)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0)
  expect_equal(circle_buffer_bytes_used(rb1), size)
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))
  expect_equal(circle_buffer_data(rb1), rep(as.raw(57), size))
})

test_that("circle_buffer_memset, overflow by 1 byte", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_reset(rb1)

  expect_equal(circle_buffer_memset(rb1, 57, size + 1L), size + 1L)

  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0)
  expect_equal(circle_buffer_bytes_used(rb1), circle_buffer_capacity(rb1))
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))
  ## head should point to the beginning of the buffer
  expect_equal(circle_buffer_head_pos(rb1), 0)
  ## tail should have bumped forward by 1 byte
  expect_equal(circle_buffer_tail_pos(rb1), 1L)
  expect_equal(circle_buffer_data(rb1), rep(as.raw(57), size))
})

test_that("circle_buffer_memset, twice (overflow by 1 byte on 2nd copy)", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_reset(rb1)

  expect_equal(circle_buffer_memset(rb1, 57, size), size)
  expect_equal(circle_buffer_memset(rb1, 57, 1L), 1L)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0)
  expect_equal(circle_buffer_bytes_used(rb1), circle_buffer_capacity(rb1))
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))
  ## head should point to the beginning of the buffer
  expect_equal(circle_buffer_head_pos(rb1), 0)
  ## tail should have bumped forward by 1 byte
  expect_equal(circle_buffer_tail_pos(rb1), 1L)

  expect_equal(circle_buffer_data(rb1), repr(57, size))
})

test_that("circle_buffer_memset, twice with oveflow", {
  ## circle_buffer_memset, attempt to overflow by 2 bytes, but
  ## circle_buffer_memset will stop at 1 byte overflow (length
  ## clamping, see circle_buffer_memset documentation).
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_reset(rb1)

  expect_equal(circle_buffer_memset(rb1, 57, size + 2), size + 1L)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0)
  expect_equal(circle_buffer_bytes_used(rb1), circle_buffer_capacity(rb1))
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))
  expect_equal(circle_buffer_head_pos(rb1), 0L)
  expect_equal(circle_buffer_tail_pos(rb1), 1L)
  expect_equal(circle_buffer_data(rb1), repr(57, size))
})

test_that("circle_buffer_memset, twice, overflowing both times", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_reset(rb1)

  expect_equal(circle_buffer_memset(rb1, 57, size + 1L), size + 1L)
  expect_equal(circle_buffer_memset(rb1, 58, size + 1L), size + 1L)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0)
  expect_equal(circle_buffer_bytes_used(rb1), circle_buffer_capacity(rb1))
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))
  expect_equal(circle_buffer_head_pos(rb1), 0L)
  expect_equal(circle_buffer_tail_pos(rb1), 1L)
  expect_equal(circle_buffer_data(rb1), repr(58, size))
})

test_that("circle_buffer_memcpy_into with zero count", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_memset(rb1, as.raw(1), circle_buffer_size(rb1))
  circle_buffer_reset(rb1)

  tmp <- fill_buffer("abcdefghijk", (size + 1) * 2)

  expect_equal(circle_buffer_memcpy_into(rb1, raw()),
               circle_buffer_head_pos(rb1))
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), circle_buffer_capacity(rb1))
  expect_equal(circle_buffer_bytes_used(rb1), 0)
  expect_false(circle_buffer_full(rb1))
  expect_true(circle_buffer_empty(rb1))
  expect_equal(circle_buffer_tail_pos(rb1), circle_buffer_head_pos(rb1))

  expect_equal(circle_buffer_data(rb1), repr(1, size))
})

test_that("circle_buffer_memcpy_into a few bytes of data", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_memset(rb1, 1, circle_buffer_size(rb1))
  circle_buffer_reset(rb1)

  bytes <- charToRaw("abcdefghijk")

  expect_equal(circle_buffer_memcpy_into(rb1, bytes),
               circle_buffer_head_pos(rb1))

  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1),
               circle_buffer_capacity(rb1) - length(bytes))
  expect_equal(circle_buffer_bytes_used(rb1), length(bytes))
  expect_false(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))

  expect_equal(circle_buffer_data(rb1), pad(bytes, size, 1))
})

test_that("circle_buffer_memcpy_into full capacity", {
  size <- 4096L
  rb1 <- circle_buffer_create(size)
  circle_buffer_memset(rb1, 1, circle_buffer_size(rb1))
  circle_buffer_reset(rb1)

  bytes <- fill_buffer("abcdefghijk", (size + 1) * 2)

  expect_equal(circle_buffer_memcpy_into(rb1, bytes), 0L)
  expect_equal(circle_buffer_head_pos(rb1), 0L)
  expect_equal(circle_buffer_capacity(rb1), size)
  expect_equal(circle_buffer_bytes_free(rb1), 0L)
  expect_equal(circle_buffer_bytes_used(rb1), circle_buffer_capacity(rb1))
  expect_true(circle_buffer_full(rb1))
  expect_false(circle_buffer_empty(rb1))

  expect_equal(circle_buffer_tail_read(rb1, size), tail(bytes, size))

  ## NOTE: I'm a bit confused about this, though, the data doesn't
  ## seem quite what I'd expect for the buffer to hold.
  res <- circle_buffer_data(rb1)
  i <- circle_buffer_tail_pos(rb1)
  expect_equal(i, 1L)
  j <- seq_len(i)
  ## expect_equal(c(res[-i], res[i]),
  ##              circle_buffer_tail_read(rb1, size))
})
