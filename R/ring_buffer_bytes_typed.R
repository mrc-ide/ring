##' Create a ring buffer, backed by a byte array, where each element
##' corresponds to a vector of one of R's atomic numeric types
##' (logical, integer, double, and complex).
##'
##' Note that a logical ring buffer and an integer ring buffer take
##' the same number of bytes because a logical vector is stored as an
##' integer (4 bytes per element) to deal with missing values.
##' @title Typed bytes buffer
##' @param size The maximum number of elements the buffer can hold.
##'   Each element will be multiple bytes long.
##' @param what Either a vector on the style of \code{vapply} (e.g.,
##'   \code{integer(4)} to indicate that each element of the buffer is
##'   a 4-element integer, or the \code{name} of a storage mode if
##'   \code{len} is also provided.
##' @param len If given, then the length of the storage.  If it is
##'   given, then if \code{length(what)} is zero, the storage mode of
##'   \code{what} is used as the type.  Otherwise \code{what} is
##'   interpreted as the \emph{name} of the storage mode (one of
##'   "logical", "integer", "double" or "complex".
##' @export
##' @author Rich FitzJohn
ring_buffer_bytes_typed <- function(size, what, len=NULL) {
  if (is.null(len)) {
    type <- storage.mode(what)
    len <- length(what)
  } else if (length(what) == 0L) {
    type <- storage.mode(what)
  } else if (length(what) == 1L && is.character(what)) {
    type <- what
  } else {
    stop("Invalid value for 'what' given 'len' is provided")
  }

  if (!(type %in% names(sizes))) {
    stop("storage.mode(what) must be one of ",
         paste(names(sizes), collapse=", "))
  }
  if (len <= 0L) {
    stop("'len' must be greater than zero")
  }

  stride <- sizes[[type]] * len
  to <- convert_to[[type]]
  from <- convert_from[[type]]

  .R6_ring_buffer_bytes_translate$new(size, stride, to, from,
                                      paste0("typed:", type))
}
