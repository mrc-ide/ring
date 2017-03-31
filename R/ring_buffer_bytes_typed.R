##' Create a ring buffer, backed by a \code{\link{ring_buffer_bytes}},
##' where each element corresponds to a fixed-size vector of one of
##' R's atomic numeric types (logical, integer, double, and complex).
##'
##' Note that a logical ring buffer and an integer ring buffer take
##' the same number of bytes because a logical vector is stored as an
##' integer (4 bytes per element) to deal with missing values; see
##' "writing R extensions".
##'
##' Note that it is not possible to store character vectors in a ring
##' buffer of this type because each element of a character vector can
##' be any number of bytes.
##'
##' @template ring_ref
##'
##' @title Typed bytes ring buffer
##' @param size The maximum number of elements the buffer can hold.
##'   Each element will be multiple bytes long.
##' @param what Either a vector on the style of \code{\link{vapply}}
##'   (e.g., \code{integer(4)} to indicate that each element of the
##'   buffer is a 4-element integer, or the \code{name} of a storage
##'   mode if \code{len} is also provided.
##' @param len If given, then the length of the storage.  If it is
##'   given, then if \code{length(what)} is zero, the storage mode of
##'   \code{what} is used as the type.  Otherwise \code{what} is
##'   interpreted as the \emph{name} of the storage mode (one of
##'   "logical", "integer", "double" or "complex".
##' @inheritParams ring_buffer_bytes
##' @export
##' @author Rich FitzJohn
##' @examples
##' # Create a ring buffer of 30 integers:
##' b <- ring_buffer_bytes_typed(30, integer(1))
##'
##' # Alternatively you can create the same buffer this way:
##' b <- ring_buffer_bytes_typed(30, "integer", 1)
##'
##' # The buffer is empty to start with
##' b$is_empty()
##'
##' # Note that the buffer has a stride of 4 (see ?ring_buffer_bytes)
##' b$stride()
##'
##' # Push some numbers into the buffer:
##' b$push(as.integer(1:10))
##'
##' # Report the number of elements used:
##' b$used()
##'
##' # Get the first added element:
##' b$tail()
##'
##' # The buffer behaves basically the same way now as
##' # "ring_buffer_env" but will typecheck all inputs:
##' \dontrun{
##'   b$push(pi) # error because not an integer
##'   b$push(1)  # error because not an integer (you must convert to int)
##' }
##'
##' # Recycling: the typed buffer operates by converting the input
##' # vector to a set of bytes and then pushing them onto the buffer;
##' # this works so long as the vector of bytes has the correct
##' # length.
##' b <- ring_buffer_bytes_typed(30, integer(3))
##'
##' # These both fail because 2 and 4 do not end up as multiples of 3:
##' \dontrun{
##'   b$push(c(1L, 2L))
##'   b$push(c(1L, 2L, 3L, 4L))
##' }
##'
##' # But this is fine:
##' b$push(seq_len(6))
##' b$tail()
##' b$tail_offset(1)
ring_buffer_bytes_typed <- function(size, what, len = NULL,
                                    on_overflow = "overwrite") {
  if (is.character(what) && length(what) == 1L) {
    type <- what
    len <- len %||% 0L # keeps error messages constant
    if (type %in% names(sizes)) {
    } else if (type == "numeric") {
      type <- "double"
    } else {
      stop("'what' must be one of ",
           paste(c(names(sizes), "numeric"), collapse = ", "))
    }
  } else {
    if (is.null(len)) {
      type <- storage.mode(what)
      len <- length(what)
    } else if (length(what) == 0L) {
      type <- storage.mode(what)
    } else {
      stop("Invalid value for 'what' given 'len' is provided")
    }
    if (!(type %in% names(sizes))) {
      stop("storage.mode(what) must be one of ",
           paste(names(sizes), collapse = ", "))
    }
  }
  if (len <= 0L) {
    stop("'len' must be greater than zero")
  }

  stride <- sizes[[type]] * len
  to <- convert_to[[type]]
  from <- convert_from[[type]]

  R6_ring_buffer_bytes_translate$new(size, stride, to, from, on_overflow,
                                      paste0("typed:", type))
}
