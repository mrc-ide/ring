##' Construct a ring buffer where the buffer holds a stream of bytes.
##' Optionally the buffer can be "strided" so that the bytes naturally
##' fall into chunks of exactly the same size.  It is implemented in C
##' and will be fast, with the limitation that any data transfer
##' always involves copies.
##'
##' @title Byte array based ring buffer
##' @param size Size of the buffer, each entry of which will be
##'   \code{stride} bytes long.
##' @param stride Number of bytes per buffer entry.  Defaults to 1
##'   byte.
##' @export
ring_buffer_bytes <- function(size, stride=1L) {
  .R6_ring_buffer_bytes$new(size, stride)
}

##' @importFrom R6 R6Class
.R6_ring_buffer_bytes <- R6::R6Class(
  "ring_buffer_bytes",
  cloneable=FALSE,

  public=list(
    ptr=NULL,

    initialize=function(size, stride, ptr=NULL) {
      if (is.null(ptr)) {
        self$ptr <- ring_buffer_create(size, stride)
      } else {
        self$ptr <- ptr
      }
    },

    reset=function() ring_buffer_reset(self$ptr),

    duplicate=function() {
      ## NOTE: this is not implemented like the typical R6 clone
      ## method because we need a deep clone here.  Instead we create
      ## a clone of the data and return a brand new instance of the
      ## class.
      .R6_ring_buffer_bytes$new(ptr=ring_buffer_clone(self$ptr))
    },

    size=function(bytes=FALSE) ring_buffer_size(self$ptr, bytes),
    bytes_data=function() ring_buffer_bytes_data(self$ptr), # NOENV
    stride=function() ring_buffer_stride(self$ptr), # NOENV

    used=function(bytes=FALSE) ring_buffer_used(self$ptr, bytes),
    free=function(bytes=FALSE) ring_buffer_free(self$ptr, bytes),

    empty=function() ring_buffer_empty(self$ptr),
    full=function() ring_buffer_full(self$ptr),

    head_pos=function(bytes=FALSE) ring_buffer_head_pos(self$ptr, bytes),
    tail_pos=function(bytes=FALSE) ring_buffer_tail_pos(self$ptr, bytes),

    head_data=function() ring_buffer_head_data(self$ptr),
    tail_data=function() ring_buffer_tail_data(self$ptr),
    buffer_data=function() ring_buffer_buffer_data(self$ptr), # NOENV

    set=function(data, n) ring_buffer_memset(self$ptr, data, n),
    push=function(data) ring_buffer_memcpy_into(self$ptr, data),
    take=function(n) ring_buffer_memcpy_from(self$ptr, n),
    read=function(n) ring_buffer_tail_read(self$ptr, n),

    copy=function(dest, n) {
      if (!inherits(dest, "ring_buffer_bytes")) {
        stop("'dest' must be a 'ring_buffer_bytes'")
      }
      ring_buffer_copy(self$ptr, dest$ptr, n)
    },

    ## Nondestructive:
    head_offset_data=function(n) stop("head_offset_data not yet implemented"),
    tail_offset_data=function(n) ring_buffer_tail_offset(self$ptr, n),

    ## Unusual direction:
    take_head=function(n) stop("take_head not yet implemented"),
    read_head=function(n) stop("read_head not yet implemented")
  ))

## This is used in ring_buffer_typed but is not itself exported yet.
## The definition must follow .R6_ring_buffer_bytes, so either we use
## roxygen @import to set the collation or we have to leave it in this
## file.
.R6_ring_buffer_bytes_translate <- R6::R6Class(
  "ring_buffer_bytes_translate",
  cloneable=FALSE,
  inherit=.R6_ring_buffer_bytes,

  public=list(
    to=NULL,
    from=NULL,
    type=NULL,

    initialize=function(size, stride, to, from, type=NULL, ptr=NULL) {
      super$initialize(size, stride, ptr)
      self$to <- to
      self$from <- from
      self$type <- type
    },

    ## inherits: reset, size, bytes_data, stride, used, free,
    ##   head_pos, tail_pos, copy

    duplicate=function() {
      .R6_ring_buffer_bytes_typed$new(
        NULL, self$stride(), self$to, self$from, self$type, self$ptr)
    },

    head_data=function() self$from(super$head_data()),
    tail_data=function() self$from(super$tail_data()),
    set=function() super$push(self$to(data)),
    push=function(data) super$push(self$to(data)),
    take=function(n) self$from(super$take(n)),
    read=function(n) self$from(super$read(n)),
    head_offset_data=function(n) self$from(super$head_offset_data(n)),
    tail_offset_data=function(n) self$from(super$tail_offset_data(n)),
    take_head=function(n) self$from(super$take_head(n)),
    read_head=function(n) self$from(super$read_head(n))
  ))
