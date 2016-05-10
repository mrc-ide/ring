## TODO: All the as.integer functions are really 'assert and convert
## scalar integer'.  We *do* need to validate this somewhere because
## we just blindly dereference that pointer in the C code which may
## crash R.
circle_buffer_create <- function(size, stride=1L) {
  .Call(Ccircle_buffer_create, as.integer(size), as.integer(stride))
}

circle_buffer_clone <- function(buffer) {
  .Call(Ccircle_buffer_clone, buffer[[1L]])
}

circle_buffer_bytes_data <- function(obj) {
  .Call(Ccircle_buffer_bytes_data, obj[[1L]])
}
circle_buffer_size <- function(obj, bytes=FALSE) {
  .Call(Ccircle_buffer_size, obj[[1L]], as.logical(bytes))
}

circle_buffer_full <- function(obj) {
  .Call(Ccircle_buffer_full, obj[[1L]])
}
circle_buffer_empty <- function(obj) {
  .Call(Ccircle_buffer_empty, obj[[1L]])
}

circle_buffer_head <- function(obj) {
  .Call(Ccircle_buffer_head, obj[[1L]])
}
circle_buffer_tail <- function(obj) {
  .Call(Ccircle_buffer_tail, obj[[1L]])
}
circle_buffer_data <- function(obj) {
  .Call(Ccircle_buffer_data, obj[[1L]])
}

circle_buffer_head_pos <- function(obj) {
  .Call(Ccircle_buffer_head_pos, obj[[1L]])
}
circle_buffer_tail_pos <- function(obj) {
  .Call(Ccircle_buffer_tail_pos, obj[[1L]])
}

circle_buffer_bytes_free <- function(obj) {
  .Call(Ccircle_buffer_bytes_free, obj[[1L]])
}
circle_buffer_bytes_used <- function(obj) {
  .Call(Ccircle_buffer_bytes_used, obj[[1L]])
}

circle_buffer_free <- function(obj) {
  .Call(Ccircle_buffer_free, obj[[1L]])
}
circle_buffer_used <- function(obj) {
  .Call(Ccircle_buffer_used, obj[[1L]])
}

circle_buffer_reset <- function(obj) {
  .Call(Ccircle_buffer_reset, obj[[1L]])
}

circle_buffer_memset <- function(obj, c, len=obj$size) {
  .Call(Ccircle_buffer_memset, obj[[1L]], as.raw(c), as.integer(len))
}

circle_buffer_memcpy_into <- function(obj, src) {
  invisible(.Call(Ccircle_buffer_memcpy_into, obj[[1L]], as.raw(src)))
}

circle_buffer_memcpy_from <- function(obj, count) {
  .Call(Ccircle_buffer_memcpy_from, obj[[1L]], as.integer(count))
}

circle_buffer_tail_read <- function(obj, count) {
  .Call(Ccircle_buffer_tail_read, obj[[1L]], as.integer(count))
}

circle_buffer_copy <- function(src, dest, count) {
  invisible(.Call(Ccircle_buffer_copy, src[[1L]], dest[[1L]],
                  as.integer(count)))
}

circle_buffer_tail_offset <- function(buffer, offset) {
  .Call(Ccircle_buffer_tail_offset, buffer[[1L]], as.integer(offset))
}
