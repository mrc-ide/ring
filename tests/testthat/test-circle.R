## TODO: a 'flush' argument that gets all and resets would be useful.
context("circle")

test_that("initial conditions", {
  size <- 100L
  stride <- 1L

  ptr <- .Call(Ccircle_buffer_create, size, stride)
  expect_is(ptr, "externalptr")

  expect_equal(.Call(Ccircle_buffer_size, ptr), size + 1L)
  expect_equal(.Call(Ccircle_buffer_capacity, ptr), size)
  expect_equal(.Call(Ccircle_buffer_bytes_free, ptr), 100L)
  expect_equal(.Call(Ccircle_buffer_bytes_used, ptr), 0L)
  expect_false(.Call(Ccircle_buffer_full, ptr))
  expect_true(.Call(Ccircle_buffer_empty, ptr))

  expect_error(.Call(Ccircle_buffer_head, ptr),
               "Buffer is empty")
  expect_error(.Call(Ccircle_buffer_tail, ptr),
               "Buffer is empty")
  .Call(Ccircle_buffer_reset, ptr)

  expect_error(.Call(Ccircle_buffer_memcpy_from, ptr, 10L),
               "Buffer underflow")

  rm(ptr)
  gc() # no crash, ideally
})

test_that("null pointer safe", {
  size <- 100L
  stride <- 1L
  ptr <- .Call(Ccircle_buffer_create, size, stride)
  path <- tempfile()
  saveRDS(ptr, path)
  ptr2 <- readRDS(path)

  expect_error(.Call(Ccircle_buffer_full, ptr2),
               "circle_buffer already freed")
})

test_that("reset", {
  size <- 24L
  ptr <- .Call(Ccircle_buffer_create, size, 1L)
  expect_equal(.Call(Ccircle_buffer_memset, ptr, as.raw(1), 8L), 8L)
  expect_null(.Call(Ccircle_buffer_reset, ptr))
  expect_equal(.Call(Ccircle_buffer_size, ptr), size + 1L)
  expect_equal(.Call(Ccircle_buffer_capacity, ptr), size)

  expect_equal(.Call(Ccircle_buffer_bytes_free, ptr),
               .Call(Ccircle_buffer_capacity, ptr))
  expect_equal(.Call(Ccircle_buffer_bytes_used, ptr), 0L)
  ## TODO: could do head and tail checks here, but they're really
  ## doing a check for *position* so add an interface for that
  ## perhaps (as an offset against the base).
})

data <- as.raw(1:50)
.Call(Ccircle_buffer_memcpy_into, ptr, data)

.Call(Ccircle_buffer_full, ptr)
.Call(Ccircle_buffer_empty, ptr)

## OK, head is just not working somehow.  It _always_ returns 0 here.
## I don't think that tail is working correctly either.
try(.Call(Ccircle_buffer_head, ptr))
try(.Call(Ccircle_buffer_tail, ptr))

.Call(Ccircle_buffer_bytes_free, ptr)
.Call(Ccircle_buffer_bytes_used, ptr)

tmp <- .Call(Ccircle_buffer_memcpy_from, ptr, 5L)
