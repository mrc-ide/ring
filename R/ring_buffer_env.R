##' An environment based ring buffer.  In contrast with
##' \code{\link{ring_buffer_bytes}}, this ring buffer is truely
##' circular, implemented as a doubly linked list that loops back on
##' itself.  Each element of the ring buffer can hold an arbitrary R
##' object, and no checking is done to make sure that objects are
##' similar types; in this way they are most similar to a circular
##' version of an R \code{\link{list}}.
##'
##' When pushing objects onto the buffer, you must be careful about
##' the \code{iterate} argument.  By default if the object has a
##' \code{length()} greater than 1 then \code{$push()} will iterate
##' over the object (equivalent to \code{$push(data[[1]],
##' iterate=FALSE)}, \code{$push(data[[2]], iterate=FALSE)}, and so
##' on).
##'
##' For more information and usage examples, see the vignette
##' (\code{vignette("ring")}).
##'
##' On underflow (and overflow if \code{on_overflow = "error"})
##' \code{ring} will raise custom exceptions that can be caught
##' specially by \code{tryCatch}.  These will have class
##' \code{ring_underflow} (and \code{ring_overflow} for overflow).  This
##' is not supported in the bytes buffer yet.  See the examples for
##' usage.
##'
##' @title Environment-based ring buffer
##'
##' @param size The (maximum) number of entries the buffer can
##'   contain.
##'
##' @param on_overflow Behaviour on buffer overflow.  The default is
##'   to overwrite the oldest elements in the buffer
##'   (\code{"overwrite"}).  Alternative actions are \code{"error"}
##'   which will throw an error if a function tries to add more
##'   elements than there are space for, or \code{"grow"} which will
##'   grow the buffer to accept the new elements.
##'
##' @template ring_ref
##'
##' @export
##' @author Rich FitzJohn
##' @examples
##' buf <- ring_buffer_env(10)
##' buf$push(1:10)
##' buf$take(3)
##' buf$push(11:15)
##' buf$take(2)
##'
##' # The "on_overflow" argument by default allows for the buffer to
##' # overwrite on overflow.
##' buf <- ring_buffer_env(10)
##' buf$push(1:10)
##' unlist(buf$read(buf$used())) # 1:10
##' # Over-write the first 5
##' buf$push(11:15)
##' unlist(buf$read(buf$used())) # 6:15
##'
##' # Unlike ring_buffer_bytes, these ring buffers can hold any R
##' # object.  However, you must be careful about use of iterate!
##' buf$push(lm(mpg ~ cyl, mtcars), iterate = FALSE)
##' buf$take(1)
##'
##' # Alternatively, grow the buffer as overwriting happens
##' buf <- ring_buffer_env(10, "grow")
##' buf$push(1:10)
##' buf$push(11:15)
##' unlist(buf$read(buf$used())) # 1:15
##'
##' # Or throw an error on overflow
##' buf <- ring_buffer_env(10, "error")
##' buf$push(1:10)
##' try(buf$push(11:15))
##'
##' # The errors that are thrown on underflow / overflow are typed so
##' # can be caught by tryCatch:
##' tryCatch(buf$read(100),
##'          ring_underflow = function(e) message("nope"))
##' tryCatch(buf$push(100),
##'          ring_overflow = function(e) message("nope again"))
ring_buffer_env <- function(size, on_overflow = "overwrite") {
  C_assert_size(size, "size")
  match_value(on_overflow, OVERFLOW_ACTIONS)
  R6_ring_buffer_env$new(size, on_overflow)
}

## This creates a doubly-linked list with a pair of pointers
## (next/prev) pointing up and down the list.  It does not splice
## them.
double_linked_list_create <- function(size) {
  head <- prev <- NULL
  for (i in seq_len(size)) {
    x <- new.env(parent = emptyenv())
    if (is.null(prev)) {
      head <- x
    } else {
      prev$.next <- x
      x$.prev <- prev
    }
    prev <- x
  }
  list(head, prev)
}

## This turns a doubly-linked list into a ring buffer by splicing the
## ends of the list together.  The "first" element of the ring is set
## to hold size and used elements, which we used to distinguish
## between full/empty and to make capacity lookups O(1) not O(n).
ring_buffer_env_create <- function(size) {
  list <- double_linked_list_create(size)
  head <- list[[1L]]
  tail <- list[[2L]]

  tail$.next <- head
  head$.prev <- tail
  head$.size <- as.integer(size)
  head$.used <- 0L

  head
}

ring_buffer_env_duplicate <- function(buffer) {
  ret <- ring_buffer_env(buffer$size())

  ## To *truely* duplicate the buffer, we need to advance the pointers
  ## a little.
  tail <- ret$.tail
  for (i in seq_len(buffer$tail_pos())) {
    tail <- tail$.prev
  }
  ret$.head <- ret$.tail <- tail

  tail <- buffer$.tail
  for (i in seq_len(buffer$used())) {
    ret$push(tail$data, FALSE)
    tail <- tail$.next
  }

  ret$.check_overflow <- buffer$.check_overflow
  ret$.prevent_overflow <- buffer$.prevent_overflow

  ret
}

ring_buffer_env_grow <- function(buffer, n) {
  C_assert_size(n, "n")
  if (n == 0) {
    return(invisible(NULL))
  }
  list <- double_linked_list_create(n)
  front <- list[[1]]
  back <- list[[2]]

  h <- buffer$.head
  x <- h$.prev
  x$.next <- front
  front$.prev <- x
  h$.prev <- back
  back$.next <- h

  buffer$.buffer$.size <- buffer$.buffer$.size + n
  if (buffer$used() > 0) {
    buffer$.head <- front
  }
  invisible(NULL)
}

ring_buffer_env_mirror <- function(src, dest) {
  if (identical(dest$.buffer, src$.buffer)) {
    stop("Can't mirror a buffer into itself")
  }
  size <- src$size()
  if (dest$size() != size) {
    stop(sprintf("Can't mirror as buffers differ in their size (%d vs %d)",
                 size, dest$size()))
  }

  ## NOTE: Strictly only the data that is *used* need be copied.
  ## But that will require that we offset the position of the
  ## destination buffer so that the start point equals the tail of
  ## the source buffer.  And I do like the idea of a complete
  ## reset here.
  from <- src$.buffer
  to <- dest$.buffer
  for (idx in seq_len(size)) {
    to$data <- from$data
    to <- to$.next
    from <- from$.next
  }

  dest$.head <- ring_buffer_env_move_forward(dest$.buffer, src$head_pos())
  dest$.tail <- ring_buffer_env_move_forward(dest$.buffer, src$tail_pos())
  dest$.buffer$.used <- src$.buffer$.used
}

ring_buffer_reset <- function(buffer, clear) {
  buffer$.head <- buffer$.buffer
  buffer$.tail <- buffer$.buffer
  buffer$.buffer$.used <- 0L
  if (clear) {
    x <- buffer$.buffer
    for (i in seq_len(buffer$size())) {
      x$data <- NULL
      x <- x$.next
    }
  }
}

## NOTE: I've put lots of C_assert_size(n) calls in; implementing this
## in R takes about ~3us but the C version here takes ~.4us; the
## former is about the same as accessing the $size() method while the
## latter is about 3x the cost of `TRUE == FALSE`.  Having these here
## gives us nicer, and fairly consistent, error messages at a low
## overhead.
##' @importFrom R6 R6Class
R6_ring_buffer_env <- R6::R6Class(
  "ring_buffer_env",
  ## need to implement our own clone method as the default R6 one is
  ## not going to cut it, given everything inside the class is a
  ## reference.
  cloneable = FALSE,

  public = list(
    ## Making all data members begin with a period; while these still
    ## print with current R6 print semantics, they hopefully will be
    ## treated as private by users.
    .buffer = NULL,
    .head = NULL,
    .tail = NULL,
    .check_overflow = NULL,
    .prevent_overflow = NULL,

    initialize = function(size, on_overflow) {
      self$.buffer <- ring_buffer_env_create(size)
      self$.check_overflow <- on_overflow != "overwrite"
      self$.prevent_overflow <- on_overflow == "error"
      self$reset()
    },

    reset = function(clear = FALSE) {
      ring_buffer_reset(self, clear)
    },

    duplicate = function() {
      ring_buffer_env_duplicate(self)
    },

    grow = function(n) {
      C_assert_size(n, "n")
      ring_buffer_env_grow(self, n)
    },

    size = function() self$.buffer$.size,
    ## bytes_data
    ## stride
    used = function() self$.buffer$.used,
    free = function() self$size() - self$used(),

    is_empty = function() self$used() == 0L,
    is_full = function() self$used() == self$size(),

    ## Mostly debugging:
    head_pos = function() {
      ring_buffer_env_distance_forward(self$.buffer, self$.head)
    },
    tail_pos = function() {
      ring_buffer_env_distance_forward(self$.buffer, self$.tail)
    },

    head = function() {
      ring_buffer_env_check_underflow(self, 1L)
      self$.head$.prev$data
    },
    tail = function() {
      ring_buffer_env_check_underflow(self, 1L)
      self$.tail$data
    },

    ## Start getting strong divergence here:
    set = function(data, n) {
      C_assert_size(n, "n")
      ring_buffer_env_check_overflow(self, n)
      for (i in seq_len(min(n, self$size()))) {
        ring_buffer_env_write_to_head(self, data)
      }
    },

    push = function(data, iterate = TRUE) {
      ring_buffer_env_check_overflow(self, if (iterate) length(data) else 1L)
      if (iterate) {
        for (el in data) {
          ring_buffer_env_write_to_head(self, el)
        }
      } else {
        ring_buffer_env_write_to_head(self, data)
      }
      invisible(data)
    },

    take = function(n) {
      C_assert_size(n, "n")
      dat <- ring_buffer_env_read_from_tail(self, n)
      self$.tail <- dat[[2L]]
      self$.buffer$.used <- self$.buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read = function(n) {
      C_assert_size(n, "n")
      ring_buffer_env_read_from_tail(self, n)[[1L]]
    },

    copy = function(dest, n) {
      if (identical(dest$.buffer, self$.buffer)) {
        stop("Can't copy a buffer into itself")
      }
      C_assert_size(n, "n")
      ring_buffer_env_check_underflow(self, n)
      ring_buffer_env_check_overflow(dest, n)

      tail <- self$.tail
      for (i in seq_len(n)) {
        dest$push(tail$data)
        tail <- tail$.next
      }

      self$.tail <- tail
      self$.buffer$.used <- self$.buffer$.used - as.integer(n)
    },

    mirror = function(dest) {
      ring_buffer_env_mirror(self, dest)
    },

    head_offset = function(n) {
      C_assert_size(n, "n")
      ring_buffer_env_check_underflow(self, n + 1L)
      ring_buffer_env_move_backward(self$.head$.prev, n)$data
    },
    tail_offset = function(n) {
      C_assert_size(n, "n")
      ring_buffer_env_check_underflow(self, n + 1L)
      ring_buffer_env_move_forward(self$.tail, n)$data
    },

    ## This is the unusual direction...
    take_head = function(n) {
      C_assert_size(n, "n")
      dat <- ring_buffer_env_read_from_head(self, n)
      self$.head <- dat[[2L]]
      self$.buffer$.used <- self$.buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read_head = function(n) {
      C_assert_size(n, "n")
      ring_buffer_env_read_from_head(self, n)[[1L]]
    },

    ## advanced
    head_set = function(data) {
      self$.head$data <- data
    },
    head_data = function() {
      self$.head$data
    },
    head_advance = function() {
      ring_buffer_head_advance(self)
    }
  ))

ring_buffer_env_read_from_tail <- function(buf, n) {
  ring_buffer_env_check_underflow(buf, n)
  tail <- buf$.tail
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    ret[[i]] <- tail$data
    tail <- tail$.next
  }
  list(ret, tail)
}

ring_buffer_env_read_from_head <- function(buf, n) {
  ring_buffer_env_check_underflow(buf, n)
  head <- buf$.head

  ret <- vector("list", n)
  for (i in seq_len(n)) {
    head <- head$.prev
    ret[[i]] <- head$data
  }
  list(ret, head)
}

ring_buffer_env_write_to_head <- function(buf, data) {
  buf$.head$data <- data
  ring_buffer_head_advance(buf)
}

ring_buffer_head_advance <- function(buf) {
  n <- buf$.buffer$.used
  full <- n == buf$size()
  buf$.head <- buf$.head$.next
  if (full) {
    buf$.tail <- buf$.tail$.next
  } else {
    buf$.buffer$.used <- n + 1L
  }
}

ring_buffer_env_check_underflow <- function(obj, requested) {
  if (requested > obj$used()) {
    stop(ring_underflow(requested, obj$used()))
  }
}

ring_buffer_env_check_overflow <- function(obj, requested) {
  if (obj$.check_overflow) {
    nfree <- obj$free()
    if (requested > nfree) {
      if (obj$.prevent_overflow) {
        stop(ring_overflow(requested, nfree))
      } else {
        ring_buffer_env_grow(obj, requested - nfree)
      }
    }
  }
}

ring_buffer_env_distance_forward <- function(head, target) {
  i <- 0L
  while (!identical(target, head)) {
    i <- i + 1L
    head <- head$.next
  }
  i
}

ring_buffer_env_move_forward <- function(x, n) {
  for (i in seq_len(n)) {
    x <- x$.next
  }
  x
}

ring_buffer_env_move_backward <- function(x, n) {
  for (i in seq_len(n)) {
    x <- x$.prev
  }
  x
}

##' @export
as.list.ring_buffer_env <- function(x, ...) {
  ring_buffer_env_read_from_tail(x, x$used())[[1L]]
}

ring_underflow <- function(requested, used) {
  msg <- sprintf("Buffer underflow (requested %d elements but %d available)",
                 requested, used)
  structure(list(requested = requested, used = used, message = msg,
                 call = NULL),
            class = c("ring_underflow", "error", "condition"))
}

ring_overflow <- function(requested, free) {
  msg <- sprintf("Buffer overflow (requested %d elements but %d available)",
                 requested, free)
  structure(list(requested = requested, free = free, message = msg,
                 call = NULL),
            class = c("ring_overflow", "error", "condition"))
}
