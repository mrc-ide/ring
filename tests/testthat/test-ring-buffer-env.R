context("ring_buffer_env")

test_that("empty", {
  n <- 10
  buf <- ring_buffer_env(10)
  expect_equal(buf$size(), n)
  expect_identical(buf$size(), as.integer(n))

  expect_equal(buf$used(), 0L)
  expect_equal(buf$free(), 10L)
  expect_true(buf$empty())
  expect_false(buf$full())
  expect_equal(buf$head_pos(), 0L)
  expect_equal(buf$tail_pos(), 0L)
})

test_that("push", {
  n <- 10
  buf <- ring_buffer_env(10)
  m <- 4
  buf$push(1:m)
  expect_equal(buf$used(), m)
  expect_equal(buf$free(), n - m)

  expect_false(buf$empty())
  expect_false(buf$full())
  expect_equal(buf$head_pos(), m)
  expect_equal(buf$tail_pos(), 0L)

  expect_equal(buf$tail_data(), 1)
  expect_equal(buf$head_data(), m)
})

test_that("read", {
  n <- 10
  buf <- ring_buffer_env(10)
  m <- 4
  buf$push(1:m)

  expect_equal(buf$read(0), list())
  expect_equal(buf$read(m), as.list(1:m))
  expect_error(buf$read(m + 1), "Buffer underflow")
})

test_that("tail_offset_data", {
  n <- 10
  buf <- ring_buffer_env(10)
  m <- 4
  buf$push(1:m)

  expect_equal(buf$tail_offset_data(0), 1L)
  expect_equal(buf$tail_offset_data(1), 2L)
  expect_equal(buf$tail_offset_data(m - 1), m)
  expect_error(buf$tail_offset_data(m), "Buffer underflow")
})

test_that("head_offset_data", {
  n <- 10
  buf <- ring_buffer_env(10)
  m <- 4
  buf$push(1:m)

  expect_equal(buf$head_offset_data(0), m)
  expect_equal(buf$head_offset_data(1), m - 1)
  expect_equal(buf$head_offset_data(m - 1), 1)
  expect_error(buf$head_offset_data(m), "Buffer underflow")
})

test_that("take", {
  n <- 10
  buf <- ring_buffer_env(10)
  m <- 4
  buf$push(1:m)

  expect_equal(buf$take(0), list())
  expect_equal(buf$used(), m)

  expect_equal(buf$take(1), list(1))
  expect_equal(buf$used(), m - 1)

  expect_equal(buf$head_pos(), m)
  expect_equal(buf$tail_pos(), 1L)
  expect_equal(buf$tail_data(), 2)

  expect_equal(buf$take(m - 1), as.list(2:m))
  expect_equal(buf$head_pos(), m)
  expect_equal(buf$tail_pos(), m)
  expect_true(buf$empty())
  expect_false(buf$full())
})

test_that("fill buffer, then overflow", {
  n <- 10
  buf <- ring_buffer_env(10)
  buf$push(1:n)
  expect_true(buf$full())
  expect_equal(buf$used(), n)
  expect_identical(buf$head, buf$tail)

  expect_equal(buf$head_pos(), 0L) # wrapped...
  expect_equal(buf$tail_pos(), 0L) # hasn't moved yet

  ## All the data is here:
  expect_equal(buf$read(n), as.list(seq_len(n)))

  ## Add one more, causing an overflow:
  buf$push(n + 1)
  expect_true(buf$full())
  expect_equal(buf$used(), n)
  expect_identical(buf$head, buf$tail)

  expect_equal(buf$head_pos(), 1L)
  expect_equal(buf$tail_pos(), 1L)

  expect_equal(buf$read(n), as.list(seq_len(n) + 1L))
})

test_that("duplicate", {
  n <- 10
  buf <- ring_buffer_env(10)
  buf$push(1:12)
  buf$take(3)

  expect_equal(buf$head_pos(), 2)
  expect_equal(buf$tail_pos(), 5)
  expect_equal(buf$used(), 7)
  expect_equal(buf$size(), n)
  expect_equal(buf$read(buf$used()), as.list(6:12))

  cpy <- buf$duplicate()

  ## Source unchanged:
  for (x in list(buf, cpy)) {
    expect_equal(x$head_pos(), 2)
    expect_equal(x$tail_pos(), 5)
    expect_equal(x$used(), 7)
    expect_equal(x$size(), n)
    expect_equal(x$read(x$used()), as.list(6:12))
  }

  ## But we can move the two buffers independently.
  expect_equal(cpy$take(2), as.list(6:7))
  cpy$push(13)

  expect_equal(buf$head_pos(), 2)
  expect_equal(buf$tail_pos(), 5)
  expect_equal(buf$used(), 7)
  expect_equal(buf$size(), n)
  expect_equal(buf$read(buf$used()), as.list(6:12))

  expect_equal(cpy$head_pos(), 3)
  expect_equal(cpy$tail_pos(), 7)
  expect_equal(cpy$used(), 6)
  expect_equal(cpy$size(), n)
  expect_equal(cpy$read(cpy$used()), as.list(8:13))
})

test_that("copy zero", {
  n1 <- 20
  n2 <- 10
  buf1 <- ring_buffer_env(n1)
  buf2 <- ring_buffer_env(n2)

  buf1$push(1:n1)
  buf1$copy(buf2, 0L)

  expect_equal(buf1$head_pos(), 0)
  expect_equal(buf2$head_pos(), 0)
  expect_equal(buf1$tail_pos(), 0)
  expect_equal(buf2$tail_pos(), 0)
  expect_equal(buf1$used(), n1)
  expect_equal(buf2$used(), 0)
})

test_that("copy some", {
  n1 <- 20
  n2 <- 10
  buf1 <- ring_buffer_env(n1)
  buf2 <- ring_buffer_env(n2)

  buf1$push(1:n1)
  buf1$copy(buf2, 5L)

  expect_equal(buf1$head_pos(), 0)
  expect_equal(buf2$head_pos(), 5)
  expect_equal(buf1$tail_pos(), 5)
  expect_equal(buf2$tail_pos(), 0)
  expect_equal(buf1$used(), n1 - 5)
  expect_equal(buf2$used(), 5)
})

## Because we do things that create circular references, I want to
## check that R will delete everything appropriately.
test_that("destruction", {
  buf <- ring_buffer_env(10)
  buf$push(1:10)

  deleted <- integer(0)
  finaliser <- function(obj) {
    deleted <<- c(deleted, obj$data)
  }
  local({
    head <- buf$head
    for (i in seq_len(buf$size())) {
      reg.finalizer(head, finaliser)
      head <- head$.next
    }
  })
  rm(buf)
  gc()

  expect_equal(length(deleted), 10L)
  expect_equal(sort(deleted), 1:10)
})
