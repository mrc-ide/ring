ring_buffer_create <- function(size, stride=1L) {
  .Call(Cring_buffer_create, size, stride)
}

ring_buffer_reset <- function(ptr) {
  .Call(Cring_buffer_reset, ptr)
}

ring_buffer_clone <- function(ptr) {
  .Call(Cring_buffer_clone, ptr)
}

ring_buffer_size <- function(ptr, bytes=FALSE) {
  .Call(Cring_buffer_size, ptr, as.logical(bytes))
}
ring_buffer_bytes_data <- function(ptr) {
  .Call(Cring_buffer_bytes_data, ptr)
}
ring_buffer_stride <- function(ptr) {
  .Call(Cring_buffer_stride, ptr)
}

ring_buffer_free <- function(ptr, bytes=FALSE) {
  .Call(Cring_buffer_free, ptr, bytes)
}
ring_buffer_used <- function(ptr, bytes=FALSE) {
  .Call(Cring_buffer_used, ptr, bytes)
}

ring_buffer_empty <- function(ptr) {
  .Call(Cring_buffer_empty, ptr)
}
ring_buffer_full <- function(ptr) {
  .Call(Cring_buffer_full, ptr)
}

ring_buffer_head_pos <- function(ptr, bytes=FALSE) {
  .Call(Cring_buffer_head_pos, ptr, bytes)
}
ring_buffer_tail_pos <- function(ptr, bytes=FALSE) {
  .Call(Cring_buffer_tail_pos, ptr, bytes)
}

ring_buffer_head_data <- function(ptr) {
  .Call(Cring_buffer_head, ptr)
}
ring_buffer_tail_data <- function(ptr) {
  .Call(Cring_buffer_tail, ptr)
}
ring_buffer_buffer_data <- function(ptr) {
  .Call(Cring_buffer_data, ptr)
}

## TODO: this is exposed in the R6 class as 'set' taking arguments
## 'data' and 'n'.  The behaviour on stride > 0 classes needs to be
## tweaked because we should allow 'c' to be a *pattern* of bytes.
## Expect changes -- I'll tweak this to allow 'c' to be either length
## 1 or length stride (no behaviour change on stride 1 buffers), and
## route through a separate function in the case of length(c) > 1.
ring_buffer_memset <- function(ptr, c, len) {
  invisible(.Call(Cring_buffer_memset, ptr, as.raw(c), len))
}

## TODO: This is exposed as 'push' with argument 'data'.  Check what
## the treatment of src that doesn't quite match with stride is; where
## is the error there?
ring_buffer_memcpy_into <- function(ptr, src) {
  invisible(.Call(Cring_buffer_memcpy_into, ptr, as.raw(src)))
}

## TODO: This is exposed as 'take' with argument 'n'
ring_buffer_memcpy_from <- function(ptr, count) {
  .Call(Cring_buffer_memcpy_from, ptr, count)
}

## TODO: This is exposed as 'read' with argument 'n'
ring_buffer_tail_read <- function(ptr, count) {
  .Call(Cring_buffer_tail_read, ptr, count)
}

## TODO: This is exposed with argument 'n'
ring_buffer_copy <- function(src, dest, count) {
  invisible(.Call(Cring_buffer_copy, src, dest, count))
}

ring_buffer_tail_offset <- function(ptr, offset) {
  .Call(Cring_buffer_tail_offset, ptr, offset)
}
