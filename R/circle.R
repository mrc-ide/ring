## TODO: All the as.integer functions are really 'assert and convert
## scalar integer'.  We *do* need to validate this somewhere because
## we just blindly dereference that pointer in the C code which may
## crash R.
circle_buffer_create <- function(size, stride=1L) {
  .Call(Ccircle_buffer_create, as.integer(size), as.integer(stride))
}

circle_buffer_clone <- function(ptr) {
  .Call(Ccircle_buffer_clone, ptr)
}

circle_buffer_bytes_data <- function(ptr) {
  .Call(Ccircle_buffer_bytes_data, ptr)
}
circle_buffer_size <- function(ptr, bytes=FALSE) {
  .Call(Ccircle_buffer_size, ptr, as.logical(bytes))
}
circle_buffer_stride <- function(ptr) {
  .Call(Ccircle_buffer_stride, ptr)
}

circle_buffer_full <- function(ptr) {
  .Call(Ccircle_buffer_full, ptr)
}
circle_buffer_empty <- function(ptr) {
  .Call(Ccircle_buffer_empty, ptr)
}

circle_buffer_head <- function(ptr) {
  .Call(Ccircle_buffer_head, ptr)
}
circle_buffer_tail <- function(ptr) {
  .Call(Ccircle_buffer_tail, ptr)
}
circle_buffer_data <- function(ptr) {
  .Call(Ccircle_buffer_data, ptr)
}

circle_buffer_head_pos <- function(ptr, bytes=FALSE) {
  .Call(Ccircle_buffer_head_pos, ptr, bytes)
}
circle_buffer_tail_pos <- function(ptr, bytes=FALSE) {
  .Call(Ccircle_buffer_tail_pos, ptr, bytes)
}

circle_buffer_free <- function(ptr, bytes=FALSE) {
  .Call(Ccircle_buffer_free, ptr, bytes)
}
circle_buffer_used <- function(ptr, bytes=FALSE) {
  .Call(Ccircle_buffer_used, ptr, bytes)
}

circle_buffer_reset <- function(ptr) {
  .Call(Ccircle_buffer_reset, ptr)
}

circle_buffer_memset <- function(ptr, c, len=ptr$size) {
  .Call(Ccircle_buffer_memset, ptr, as.raw(c), as.integer(len))
}

circle_buffer_memcpy_into <- function(ptr, src) {
  invisible(.Call(Ccircle_buffer_memcpy_into, ptr, as.raw(src)))
}

circle_buffer_memcpy_from <- function(ptr, count) {
  .Call(Ccircle_buffer_memcpy_from, ptr, as.integer(count))
}

circle_buffer_tail_read <- function(ptr, count) {
  .Call(Ccircle_buffer_tail_read, ptr, as.integer(count))
}

circle_buffer_copy <- function(src, dest, count) {
  invisible(.Call(Ccircle_buffer_copy, src, dest, as.integer(count)))
}

circle_buffer_tail_offset <- function(ptr, offset) {
  .Call(Ccircle_buffer_tail_offset, ptr, as.integer(offset))
}
