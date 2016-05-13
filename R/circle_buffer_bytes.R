circle_buffer_bytes <- function(size, stride=1L) {
  .R6_circle_buffer_bytes$new(size, stride)
}

##' @importFrom R6 R6Class
.R6_circle_buffer_bytes <- R6::R6Class(
  "circle_buffer_bytes",
  cloneable=FALSE,

  public=list(
    ptr=NULL,

    initialize=function(size, stride, ptr=NULL) {
      if (is.null(ptr)) {
        self$ptr <- circle_buffer_create(size, stride)
      } else {
        self$ptr <- ptr
      }
    },

    reset=function() circle_buffer_reset(self$ptr),

    duplicate=function() {
      ## NOTE: this is not implemented like the typical R6 clone
      ## method because we need a deep clone here.  Instead we create
      ## a clone of the data and return a brand new instance of the
      ## class.
      .R6_circle_buffer_bytes$new(ptr=circle_buffer_clone(self$ptr))
    },

    size=function(bytes=FALSE) circle_buffer_size(self$ptr, bytes),
    bytes_data=function() circle_buffer_bytes_data(self$ptr), # NOENV
    stride=function() circle_buffer_stride(self$ptr), # NOENV

    used=function(bytes=FALSE) circle_buffer_used(self$ptr, bytes),
    free=function(bytes=FALSE) circle_buffer_free(self$ptr, bytes),

    empty=function() circle_buffer_empty(self$ptr),
    full=function() circle_buffer_full(self$ptr),

    head_pos=function(bytes=FALSE) circle_buffer_head_pos(self$ptr, bytes),
    tail_pos=function(bytes=FALSE) circle_buffer_tail_pos(self$ptr, bytes),

    head_data=function() circle_buffer_head_data(self$ptr),
    tail_data=function() circle_buffer_tail_data(self$ptr),
    buffer_data=function() circle_buffer_buffer_data(self$ptr), # NOENV

    set=function(data, n) circle_buffer_memset(self$ptr, data, n),
    push=function(data) circle_buffer_memcpy_into(self$ptr, data),
    take=function(n) circle_buffer_memcpy_from(self$ptr, n),
    read=function(n) circle_buffer_tail_read(self$ptr, n),

    copy=function(dest, n) {
      if (!inherits(dest, "circle_buffer_bytes")) {
        stop("'dest' must be a 'circle_buffer_bytes'")
      }
      circle_buffer_copy(self$ptr, dest$ptr, n)
    },

    ## Nondestructive:
    head_offset=function(n) stop("head_offset not yet implemented"),
    tail_offset=function(n) stop("tail_offset not yet implemented"),
    head_offset_data=function(n) stop("head_offset_data not yet implemented"),
    tail_offset_data=function(n) circle_buffer_tail_offset(self$ptr, n),

    ## Unusual direction:
    take_head=function(n) stop("take_head not yet implemented"),
    read_head=function(n) stop("read_head not yet implemented")
  ))
