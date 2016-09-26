##' An environment based ring buffer.  In contrast with
##' \code{\link{ring_buffer_bytes}}, this ring buffer is truely
##' circular, implemented as a doubly linked list that loops back on
##' itself.  Each element of the ring buffer can hold an arbitrary R
##' object, and no checking is done to make sure that objects are
##' similar types; in this way they are most similar to a circular
##' version of an R \code{\link{list}}.  In contrast with
##' \code{\link{ring_buffer_bytes}}, this ring buffer can optionally
##' prevent overflows (in addition to preventing underflows).
##'
##' For more information and usage examples, see the vignette
##' (\code{vignette{"ring"}}).
##'
##' @title Environment-based ring buffer
##'
##' @param size The (maximum) number of entries the buffer can
##'   contain.
##'
##' @param prevent_overflow Logial indicating if buffer overflow is
##'   not allowed.  If \code{FALSE} (the default) then the buffer will
##'   overflow silently (that is, the oldest data will be
##'   overwritten).  If \code{TRUE}, then on overflow an error will be
##'   raised.
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
ring_buffer_env <- function(size, on_overflow = "overwrite") {
  .R6_ring_buffer_env$new(size, on_overflow)
}

## TODO: implement growth, which is fairly easy to do here; we break
## the ring and splice in another doubly linked list.

## There is an interface issue here that I need to deal with once I
## have the basic logic working; is the point of the buffer to
## implement a FIFO queue, or something more general?  In which case
## what do we do about things like the special treatment of head and
## tail for the case of using this like a FIFO/buffer.  The other
## thing I wonder about is whether it's OK to expose the head/tail
## thing or if we should abstract that away...

## TODO: Throughout, it is possible that any error in the assignment
## (probably via an R issue, object not being found etc) leaves the
## size element corrupt.  I'd like to drop this entirely I think, in
## favour of an odometer based counter.  Once I get some decent tests
## implemented I can try and swap that out.  When done, the lookups
## for capacity will be all O(n) which is a bit bad but probably a
## reasonable price to pay for simpler and more robust code.

## This creates a doubly-linked list with a pair of pointers
## (next/prev) pointing up and down the list.  It does not splice
## them.
double_linked_list_create <- function(size) {
  head <- prev <- NULL
  for (i in seq_len(size)) {
    x <- new.env(parent=emptyenv())
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

##' @importFrom R6 R6Class
.R6_ring_buffer_env <- R6::R6Class(
  "ring_buffer_env",
  ## need to implement our own clone method as the default R6 one is
  ## not going to cut it, given everything inside the class is a
  ## reference.
  cloneable=FALSE,

  public=list(
    ## Making all data members begin with a period; while these still
    ## print with current R6 print semantics, they hopefully will be
    ## treated as private by users.
    .buffer=NULL,
    .head=NULL,
    .tail=NULL,
    .check_overflow=NULL,
    .prevent_overflow=NULL,

    initialize=function(size, on_overflow) {
      ## assert_scalar_logical(on_overflow) # -copy from bytes
      self$.buffer <- ring_buffer_env_create(size)
      self$.check_overflow <- on_overflow != "overwrite"
      self$.prevent_overflow <- on_overflow == "error"
      self$reset()
    },

    reset=function() {
      self$.head <- self$.buffer
      self$.tail <- self$.buffer
      self$.buffer$.used <- 0L
    },

    duplicate=function() {
      ring_buffer_env_duplicate(self)
    },

    size=function() self$.buffer$.size,
    ## bytes_data
    ## stride
    used=function() self$.buffer$.used,
    free=function() self$size() - self$used(),

    is_empty=function() self$used() == 0L,
    is_full=function() self$used() == self$size(),

    ## Mostly debugging:
    head_pos=function() {
      ring_buffer_env_distance_forward(self$.buffer, self$.head)
    },
    tail_pos=function() {
      ring_buffer_env_distance_forward(self$.buffer, self$.tail)
    },

    head=function() {
      ring_buffer_env_check_underflow(self, 1L)
      self$.head$.prev$data
    },
    tail=function() {
      ring_buffer_env_check_underflow(self, 1L)
      self$.tail$data
    },

    ## Start getting strong divergence here:
    set=function(data, n) {
      ring_buffer_env_check_overflow(self, n)
      for (i in seq_len(min(n, self$size()))) {
        ring_buffer_env_write_to_head(self, data)
      }
    },

    ## TODO: It will be probably useful on occassion to add a
    ## data.frame here row-by-row and things like that.  So we might
    ## have to make this one generic -- or find something that makes
    ## things iterable.  There are iterator packages we could depend
    ## on so that it could be possible to say something like:
    ##
    ## buf$push(iterate_by_row(data.frame))
    push=function(data, iterate=TRUE) {
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

    take=function(n) {
      dat <- ring_buffer_env_read_from_tail(self, n)
      self$.tail <- dat[[2L]]
      self$.buffer$.used <- self$.buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read=function(n) ring_buffer_env_read_from_tail(self, n)[[1L]],

    copy=function(dest, n) {
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

    head_offset=function(n) {
      ring_buffer_env_check_underflow(self, n + 1L)
      ring_buffer_env_move_backward(self$.head$.prev, n)$data
    },
    tail_offset=function(n) {
      ring_buffer_env_check_underflow(self, n + 1L)
      ring_buffer_env_move_forward(self$.tail, n)$data
    },

    ## This is the unusual direction...
    take_head=function(n) {
      dat <- ring_buffer_env_read_from_head(self, n)
      self$.head <- dat[[2L]]
      self$.buffer$.used <- self$.buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read_head=function(n) {
      ring_buffer_env_read_from_head(self, n)[[1L]]
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
  n <- buf$.buffer$.used
  full <- n == buf$size()
  buf$.head$data <- data
  buf$.head <- buf$.head$.next
  if (full) {
    buf$.tail <- buf$.tail$.next
  } else {
    buf$.buffer$.used <- n + 1L
  }
}

ring_buffer_env_check_underflow <- function(obj, requested) {
  ## TODO: Perhaps an S3 condition?
  if (requested > obj$used()) {
    stop(sprintf("Buffer underflow (requested %d elements but %d available)",
                 requested, obj$used()))
  }
}

ring_buffer_env_check_overflow <- function(obj, requested) {
  ## TODO: Perhaps an S3 condition?
  if (obj$.check_overflow && requested > obj$free()) {
    if (obj$.prevent_overflow) {
      stop(sprintf("Buffer overflow: (adding %d elements, but %d available)",
                   requested, obj$free()))
    } else {
      ring_buffer_env_grow(obj, requested)
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
