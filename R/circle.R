circle_buffer_create <- function(size, stride=1L) {
  ptr <- .Call(Ccircle_buffer_create, as.integer(size), as.integer(stride))
  ret <- list(ptr=ptr, size=size, stride=stride)
  class(ret) <- "circle_buffer"
  ret
}

## TODO: These should change name because size does not reflect the
## name above...
circle_buffer_size <- function(obj) {
  .Call(Ccircle_buffer_size, obj[[1L]])
}
circle_buffer_capacity <- function(obj) {
  .Call(Ccircle_buffer_capacity, obj[[1L]])
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
  .Call(Ccircle_buffer_copy, src[[1L]], dest[[1L]], as.integer(count))
}
