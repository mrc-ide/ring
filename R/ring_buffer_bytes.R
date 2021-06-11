##' Construct a ring buffer where the buffer holds a stream of bytes.
##' Optionally, the buffer can be "strided" so that the bytes
##' naturally fall into chunks of exactly the same size.  It is
##' implemented in C in the hope that it will be fast, with the
##' limitation that any data transfer to or from R will always involve
##' copies.
##'
##' In contrast with \code{\link{ring_buffer_env}}, every element of
##' this buffer has the same size; this makes it less flexible
##' (because you have to decide ahead of time what you will be
##' storing), but at the same time this can make using the buffer
##' easier to think about (because you decided ahead of time what you
##' are storing).
##'
##' If you want to use this to store fixed-size arrays of integers,
##' numerics, etc, see \code{\link{ring_buffer_bytes_typed}} which
##' wraps this with fast conversion functions.
##'
##' If the \code{on_overflow} action is \code{"grow"} and the buffer
##' overflows, then the size of the buffer will grow geometrically
##' (this is also the case if you manually \code{$grow()} the buffer
##' with \code{exact = FALSE}).  When used this way, let \code{n} is
##' the number of \emph{additional} elements that space is needed for;
##' \code{ring} then looks at the total needed capacity (used plus
##' \code{n} relative to \code{size()}).  \emph{If} the buffer needs
##' to be made larger to fit \code{n} elements in then it is grown by
##' a factor of phi (the golden ratio, approximately 1.6).  So if to
##' fit \code{n} elements in the buffer needs to be increased in size
##' by \code{m} then the smallest of \code{size * phi}, \code{size *
##' phi^2}, \code{size * phi^3}, ... will be used as the new size.
##'
##' In contrast, using the \code{grow()} method with \code{exact =
##' TRUE} will \emph{always} increase the size of the buffer so long
##' as \code{n} is positive.
##'
##' @template ring_ref
##'
##' @title Byte array based ring buffer
##' @param size Number of elements in the buffer, each of which will
##'   be \code{stride} bytes long.
##' @param stride Number of bytes per buffer element.  Defaults to 1
##'   byte.  If you want to store anything other than a bytestream in
##'   the buffer, you will probably want more than one byte per
##'   element; for example, on most R platforms an integer takes 4
##'   bytes and a double takes 8 (see \code{\link{.Machine}}, and also
##'   \code{\link{ring_buffer_bytes_typed}}).
##' @param on_overflow Behaviour on buffer overflow.  The default is
##'   to overwrite the oldest elements in the buffer
##'   (\code{"overwrite"}).  Alternative actions are \code{"error"}
##'   which will throw an error if a function tries to add more
##'   elements than there are space for, or \code{"grow"} which will
##'   grow the buffer to accept the new elements (this uses an
##'   approximately golden ratio approach; see details below).
##' @export
##' @examples
##' # Create a ring buffer of 100 bytes
##' b <- ring_buffer_bytes(100)
##'
##' # Get the length, number of used and number of free bytes:
##' b$size()
##' b$used()
##' b$free()
##'
##' # Nothing is used because we're empty:
##' b$is_empty()
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
ring_buffer_bytes <- function(size, stride = 1L, on_overflow = "overwrite") {
  R6_ring_buffer_bytes$new(size, stride, on_overflow)
}

##' @importFrom R6 R6Class
R6_ring_buffer_bytes <- R6::R6Class(
  "ring_buffer_bytes",
  cloneable = FALSE,
  public = list(
    .ptr = NULL,

    initialize = function(size, stride, on_overflow, ptr = NULL) {
      if (is.null(ptr)) {
        on_overflow <- check_on_overflow(on_overflow)
        self$.ptr <- .Call(Cring_buffer_create, size, stride, on_overflow)
      } else {
        self$.ptr <- ptr
      }
    },

    reset = function(clear = FALSE) {
      .Call(Cring_buffer_reset, self$.ptr, clear)
    },

    ## NOTE: duplicate is not implemented like the typical R6 clone
    ## method because we need a deep clone here but I don't want a
    ## private set of methods.  Instead we create a clone of the
    ## data and return a brand new instance of the class.
    duplicate = function() {
      R6_ring_buffer_bytes$new(ptr = .Call(Cring_buffer_duplicate, self$.ptr))
    },

    grow = function(n, exact = FALSE) {
      .Call(Cring_buffer_grow, self$.ptr, n, exact)
    },

    size = function(bytes = FALSE) .Call(Cring_buffer_size, self$.ptr, bytes),
    bytes_data = function() .Call(Cring_buffer_bytes_data, self$.ptr),
    stride = function() .Call(Cring_buffer_stride, self$.ptr),

    used = function(bytes = FALSE) .Call(Cring_buffer_used, self$.ptr, bytes),
    free = function(bytes = FALSE) .Call(Cring_buffer_free, self$.ptr, bytes),

    is_empty = function() .Call(Cring_buffer_is_empty, self$.ptr),
    is_full = function() .Call(Cring_buffer_is_full, self$.ptr),

    head_pos = function(bytes = FALSE) {
      .Call(Cring_buffer_head_pos, self$.ptr, bytes)
    },
    tail_pos = function(bytes = FALSE) {
      .Call(Cring_buffer_tail_pos, self$.ptr, bytes)
    },

    head = function() .Call(Cring_buffer_head, self$.ptr),
    tail = function() .Call(Cring_buffer_tail, self$.ptr),
    data = function() .Call(Cring_buffer_data, self$.ptr),

    set = function(data, n) {
      invisible(.Call(Cring_buffer_set, self$.ptr, data, n))
    },
    push = function(data) {
      invisible(.Call(Cring_buffer_push, self$.ptr, data))
    },
    take = function(n) .Call(Cring_buffer_take, self$.ptr, n),
    read = function(n) .Call(Cring_buffer_read, self$.ptr, n),

    copy = function(dest, n) {
      if (!inherits(dest, "ring_buffer_bytes")) {
        stop("'dest' must be a 'ring_buffer_bytes'")
      }
      .Call(Cring_buffer_copy, self$.ptr, dest$.ptr, n)
    },

    mirror = function(dest) {
      if (!inherits(dest, "ring_buffer_bytes")) {
        stop("'dest' must be a 'ring_buffer_bytes'")
      }
      invisible(.Call(Cring_buffer_mirror, self$.ptr, dest$.ptr))
    },

    ## Nondestructive:
    head_offset = function(n) .Call(Cring_buffer_head_offset, self$.ptr, n),
    tail_offset = function(n) .Call(Cring_buffer_tail_offset, self$.ptr, n),

    ## Unusual direction:
    take_head = function(n) .Call(Cring_buffer_take_head, self$.ptr, n),
    read_head = function(n) .Call(Cring_buffer_read_head, self$.ptr, n),

    ## Advanced
    head_set = function(data) {
      invisible(.Call(Cring_buffer_head_set, self$.ptr, data))
    },
    head_data = function() {
      .Call(Cring_buffer_head_data, self$.ptr)
    },
    head_advance = function() {
      invisible(.Call(Cring_buffer_head_advance, self$.ptr))
    }
  ))

##' This ring buffer is based on \code{\link{ring_buffer_bytes}} but
##' performs conversion to/from bytes to something useful as data is
##' stored/retrieved from the buffer.  This is the interface through
##' which \code{\link{ring_buffer_bytes_typed}} is implemented.
##'
##' The idea here is that manually working with raw vectors can get
##' tedious, and if you are planning on using a bytes-based buffer
##' while working in R you may have a way of doing conversion from
##' and to R objects.  This interface lets you specify the functions
##' once and then will apply your conversion function in every case
##' where they are needed.
##'
##' @template ring_ref
##' @title Translating bytes ring buffer
##' @inheritParams ring_buffer_bytes
##' @param to Function to convert an R object to a set of exactly
##'   \code{stride} bytes.  It must take one argument (being an R
##'   object) and return a raw vector of a length that is a multiple
##'   of \code{stride} (including zero).  It may throw an error if it
##'   is not possible to convert an object to a bytes vector.
##' @param from Function to convert a set of bytes to an R object.  It
##'   must take one argument (being a raw vector of a length that is a
##'   multiple of \code{stride}, including zero).  It should not throw
##'   an error as all data added to the buffer will have passed
##'   through \code{to} on the way in to the buffer.
##' @export
##' @author Rich FitzJohn
##' @examples
##' # The "typed" ring buffers do not allow for character vectors to
##' # be stored, because strings are generally hard and have unknown
##' # lengths.  But if you wanted to store strings that are *always*
##' # the same length, this is straightforward to do.
##'
##' # You can convert from string to bytes with charToRaw (or
##' # as.raw(utf8ToInt(x))):
##' bytes <- charToRaw("hello!")
##' bytes
##'
##' # And back again with rawToChar (or intToUtf8(as.integer(x)))
##' rawToChar(bytes)
##'
##' # So with these functions we can make a buffer for storing
##' # fixed-length strings:
##' b <- ring_buffer_bytes_translate(100, 8, charToRaw, rawToChar)
##'
##' # And with this we can store 8 character strings:
##' b$push("abcdefgh")
##' b$tail()
##'
##' # Other length strings cannot be added:
##' try(
##'   b$push("hello!")
##' ) # error
##'
##' # Because the 'from' and 'to' arguments can be arbitrary R
##' # functions we could tweak this to pad the character vector with
##' # null bytes, and strip these off on return:
##' char_to_raw <- function(x, max_len) {
##'   if (!(is.character(x) && length(x) == 1L)) {
##'     stop("Expected a single string")
##'   }
##'   n <- nchar(x)
##'   if (n > max_len) {
##'     stop("String is too long")
##'   }
##'   c(charToRaw(x), rep(raw(1), max_len - n))
##' }
##' char_from_raw <- function(x) {
##'   rawToChar(x[x != raw(1)])
##' }
##'
##' # Because max_len is the same thing as stride, wrap this all up a
##' # little:
##' char_buffer <- function(size, max_len) {
##'   to <- function(x) char_to_raw(x, max_len)
##'   ring_buffer_bytes_translate(size, max_len, to, char_from_raw)
##' }
##'
##' b <- char_buffer(100, 30) # 100 elements of up to 30 characters each
##' b$push("x")
##' b$tail()
##'
##' b$push("hello world!")
##' b$head()
##'
##' try(
##'   b$push("supercalafragalisticexpealadocious")
##' ) # error: string is too long
ring_buffer_bytes_translate <- function(size, stride, to, from,
                                        on_overflow = "overwrite") {
  R6_ring_buffer_bytes_translate$new(size, stride, to, from, on_overflow)
}

## The definition below must follow R6_ring_buffer_bytes, so either
## we use roxygen @import to set the collation or we have to leave it
## in this file (or jiggle the files around so they collate correctly
## in every language).
R6_ring_buffer_bytes_translate <- R6::R6Class(
  "ring_buffer_bytes_translate",
  cloneable = FALSE,
  inherit = R6_ring_buffer_bytes,

  public = list(
    .to = NULL,
    .from = NULL,
    .type = NULL,

    initialize = function(size, stride, to, from, on_overflow,
                          type = NULL, ptr = NULL) {
      assert_function(to)
      assert_function(from)
      super$initialize(size, stride, on_overflow, ptr)
      self$.to <- to
      self$.from <- from
      self$.type <- type
    },

    ## inherits: reset, size, bytes_data, stride, used, free,
    ##   head_pos, tail_pos, copy, mirror

    duplicate = function() {
      ptr_copy <- .Call(Cring_buffer_duplicate, self$.ptr)
      R6_ring_buffer_bytes_translate$new(
        NULL, NULL, self$.to, self$.from, NULL, self$.type, ptr_copy)
    },

    head = function() self$.from(super$head()),
    tail = function() self$.from(super$tail()),
    set = function(data, n) super$set(self$.to(data), n),
    push = function(data) super$push(self$.to(data)),
    take = function(n) self$.from(super$take(n)),
    read = function(n) self$.from(super$read(n)),
    head_offset = function(n) self$.from(super$head_offset(n)),
    tail_offset = function(n) self$.from(super$tail_offset(n)),
    take_head = function(n) self$.from(super$take_head(n)),
    read_head = function(n) self$.from(super$read_head(n)),
    head_set = function(data) super$head_set(self$.to(data)),
    head_data = function() self$.from(super$head_data())
  ))

## Must match the order in ring.h
OVERFLOW_ACTIONS <- c("overwrite", "grow", "error")
check_on_overflow <- function(on_overflow) {
  assert_scalar(on_overflow)
  assert_character(on_overflow)
  i <- match(on_overflow, OVERFLOW_ACTIONS)
  if (is.na(i)) {
    stop("Invalid value for 'on_overflow'; must be one of ",
         paste(OVERFLOW_ACTIONS, collapse = ", "))
  }
  as.integer(i - 1L)
}
