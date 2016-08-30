##' Construct a ring buffer where the buffer holds a stream of bytes.
##' Optionally the buffer can be "strided" so that the bytes naturally
##' fall into chunks of exactly the same size.  It is implemented in C
##' and will be fast, with the limitation that any data transfer
##' always involves copies.
##'
##' In contrast with \code{\link{ring_buffer_env}}, every element of
##' this buffer has the same size; this makes it less flexible in some
##' ways, but at the same time this can make the buffer easier to
##' think about.
##'
##' If you want to use this to store fixed-size arrays of integers,
##' numerics, etc, see \code{\link{ring_buffer_bytes_typed}} which
##' wraps this with fast conversion functions.
##'
##' @template ring_ref
##'
##' @title Byte array based ring buffer
##' @param size Size of the buffer, each entry of which will be
##'   \code{stride} bytes long.
##' @param stride Number of bytes per buffer entry.  Defaults to 1
##'   byte.  If you want to store anything other than a bytestream in
##'   the buffer, you will probably want more than one byte per
##'   element; for example, an integer takes 4 bytes and a double
##'   takes 8.
##' @export
##' @examples
##'
##' # Create a ring buffer of 100 bytes
##' b <- ring_buffer_bytes(100)
##'
##' # Get the length, number of used and number of free bytes:
##' b$size()
##' b$used()
##' b$free()
##'
##' # Nothing is used because we're empty:
##' b$empty()
##'
##' # To work with a bytes buffer you need to use R's raw vectors;
##' # here are 30 random bytes:
##' bytes <- as.raw(as.integer(sample(256, 30, TRUE) - 1L))
##' bytes
##'
##' # Push these onto the bytes buffer:
##' b$push(bytes)
##' b$used()
##'
##' # The head of the buffer points at the most recently added item
##' b$head()
##' bytes[[length(bytes)]]
##'
##' # ...and the tail at the oldest (first added in this case)
##' b$tail()
##' bytes[[1]]
##'
##' # Elements are taken from the tail; these will be the oldest items:
##' b$take(8)
##' bytes[1:8]
##' b$used()
##'
##' # To read from the buffer without removing elements, use read:
##' b$read(8)
##' bytes[9:16]
##'
##' # It is not possible to take or read more elements than are
##' # present in the buffer; it will throw an error:
##' \dontrun{
##' b$read(50) # error because there are only 22 bytes present
##' }
##'
##' # More elements can be pushed on:
##' b$push(as.raw(rep(0, 50)))
##' b$used()
##' b$read(b$used())
##'
##' # If many new elements are added, they will displace the old elements:
##' b$push(as.raw(1:75))
##' b$read(b$used())
ring_buffer_bytes <- function(size, stride=1L) {
  .R6_ring_buffer_bytes$new(size, stride)
}

##' @importFrom R6 R6Class
.R6_ring_buffer_bytes <- R6::R6Class(
  "ring_buffer_bytes",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,

    initialize=function(size, stride, ptr=NULL) {
      if (is.null(ptr)) {
        self$.ptr <- .Call(Cring_buffer_create, size, stride)
      } else {
        self$.ptr <- ptr
      }
    },

    reset=function() .Call(Cring_buffer_reset, self$.ptr),

    ## NOTE: duplicate is not implemented like the typical R6 clone
    ## method because we need a deep clone here but I don't want a
    ## private set of methods.  Instead we create a clone of the
    ## data and return a brand new instance of the class.
    duplicate=function() {
      .R6_ring_buffer_bytes$new(ptr=.Call(Cring_buffer_clone, self$.ptr))
    },

    size=function(bytes=FALSE) .Call(Cring_buffer_size, self$.ptr, bytes),
    bytes_data=function() .Call(Cring_buffer_bytes_data, self$.ptr),
    stride=function() .Call(Cring_buffer_stride, self$.ptr),

    used=function(bytes=FALSE) .Call(Cring_buffer_used, self$.ptr, bytes),
    free=function(bytes=FALSE) .Call(Cring_buffer_free, self$.ptr, bytes),

    empty=function() .Call(Cring_buffer_empty, self$.ptr),
    full=function() .Call(Cring_buffer_full, self$.ptr),

    head_pos=function(bytes=FALSE) {
      .Call(Cring_buffer_head_pos, self$.ptr, bytes)
    },
    tail_pos=function(bytes=FALSE) {
      .Call(Cring_buffer_tail_pos, self$.ptr, bytes)
    },

    head=function() .Call(Cring_buffer_head, self$.ptr),
    tail=function() .Call(Cring_buffer_tail, self$.ptr),
    data=function() .Call(Cring_buffer_data, self$.ptr),

    set=function(data, n) {
      invisible(.Call(Cring_buffer_set, self$.ptr, as.raw(data), n))
    },
    push=function(data) {
      invisible(.Call(Cring_buffer_push, self$.ptr, as.raw(data)))
    },
    take=function(n) .Call(Cring_buffer_take, self$.ptr, n),
    read=function(n) .Call(Cring_buffer_read, self$.ptr, n),

    copy=function(dest, n) {
      if (!inherits(dest, "ring_buffer_bytes")) {
        stop("'dest' must be a 'ring_buffer_bytes'")
      }
      .Call(Cring_buffer_copy, self$.ptr, dest$.ptr, n)
    },

    ## Nondestructive:
    head_offset=function(n) .Call(Cring_buffer_head_offset, self$.ptr, n),
    tail_offset=function(n) .Call(Cring_buffer_tail_offset, self$.ptr, n),

    ## Unusual direction:
    take_head=function(n) .Call(Cring_buffer_take_head, self$.ptr, n),
    read_head=function(n) .Call(Cring_buffer_read_head, self$.ptr, n)
  ))


##' This ring buffer is based on \code{\link{ring_buffer_bytes}} but
##' performs conversion to/from bytes to something useful as data is
##' stored/retrieved from the buffer.  This is the interface through
##' which \code{ring_buffer_bytes_typed} is implemented.
##'
##' The idea here is that manually working with raw vectors is
##' annoying, and if you are planning on using a bytes-based buffer
##' while working in R you may have a way of doing converstion from
##' and to R objects.  This interface lets you specify the functions
##' once and then will apply your conversion function in every case
##' where they are needed.
##'
##' @template ring_ref
##' @title Typed bytes ring buffer
##' @inheritParams ring_buffer_bytes
##' @param to Function to convert an R object to a set of exactly
##'   \code{stride} bytes.
##' @param from Function to convert a set of bytes to an R object.
##' @export
##' @author Rich FitzJohn
ring_buffer_bytes_translate <- function(size, stride, to, from) {
  .R6$ring_buffer_bytes_translate$new(size, stride, to, from)
}

## The definition below must follow .R6_ring_buffer_bytes, so either
## we use roxygen @import to set the collation or we have to leave it
## in this file (or jiggle the files around so they collate correctly
## in every language).
.R6_ring_buffer_bytes_translate <- R6::R6Class(
  "ring_buffer_bytes_translate",
  cloneable=FALSE,
  inherit=.R6_ring_buffer_bytes,

  public=list(
    to=NULL,
    from=NULL,
    type=NULL,

    initialize=function(size, stride, to, from, type=NULL, ptr=NULL) {
      assert_function(to)
      assert_function(from)
      super$initialize(size, stride, ptr)
      self$to <- to
      self$from <- from
      self$type <- type
    },

    ## inherits: reset, size, bytes_data, stride, used, free,
    ##   head_pos, tail_pos, copy

    duplicate=function() {
      .R6_ring_buffer_bytes_typed$new(
        NULL, self$stride(), self$to, self$from, self$type, self$.ptr)
    },

    head=function() self$from(super$head()),
    tail=function() self$from(super$tail()),
    set=function(data) super$push(self$to(data)),
    push=function(data) super$push(self$to(data)),
    take=function(n) self$from(super$take(n)),
    read=function(n) self$from(super$read(n)),
    head_offset=function(n) self$from(super$head_offset(n)),
    tail_offset=function(n) self$from(super$tail_offset(n)),
    take_head=function(n) self$from(super$take_head(n)),
    read_head=function(n) self$from(super$read_head(n))
  ))
