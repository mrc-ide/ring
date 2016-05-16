## an environment-backed circle buffer.  This one is genuinely
## circular via a pair of doubly linked lists.

## This might be more efficient to implement with C lookups, but I'm
## not sure and doubt it will make a big difference.

## There is an interface issue here that I need to deal with once I
## have the basic logic working; is the point of the buffer to
## implement a FIFO queue, or something more general?  In which case
## what do we do about things like the special treatment of head and
## tail for the case of using this like a FIFO/buffer.  The other
## thing I wonder about is whether it's OK to expose the head/tail
## thing or if we should abstract that away...

## Is it also worth keeping an "index" (a second set of pointers in a
## list) so that we can efficiently move from one environment to
## another via random access?  This would speed up things like a
## linear search for an index element perhaps.

##   reset
##   size
##   used
##   free
##   empty
##   full

## In the C version I then have:
##
##   memcpy_into
##   memcpy_from
##   memset
##
## which all act as FIFO
##
## I expect that *writing* and *destructive reading* will generally
## happen in a FIFO way (writes to head, reads from tail)
##
## But then for simple reading we have:
##
## tail_read
## tail_data (as tail)
## head_data
##
## Here I'll implement a similar set of commands and see what happens.

circle_buffer_env_create <- function(size) {
  head <- prev <- NULL
  for (i in seq_len(size)) {
    x <- new.env(parent=emptyenv())
    x$.idx <- i # debug only
    if (is.null(prev)) {
      head <- x
    } else {
      prev$.next <- x
      x$.prev <- prev
    }
    prev <- x
  }

  x$.next <- head
  head$.prev <- x
  head$.size <- as.integer(size)
  head$.used <- 0L

  head
}

circle_buffer_env_duplicate <- function(buffer) {
  ret <- circle_buffer_env(buffer$size())

  ## To *truely* duplicate the buffer, we need to advance the pointers
  ## a little.
  tail <- ret$tail
  for (i in seq_len(buffer$tail_pos())) {
    tail <- tail$.prev
  }
  ret$head <- ret$tail <- tail

  tail <- buffer$tail
  for (i in seq_len(buffer$used())) {
    ret$push(tail$data, FALSE)
    tail <- tail$.next
  }

  ret
}

circle_buffer_env <- function(size) {
  .R6_circle_buffer_env$new(size)
}

##' @importFrom R6 R6Class
.R6_circle_buffer_env <- R6::R6Class(
  "circle_buffer_env",
  ## need to implement our own clone method as the default R6 one is
  ## not going to cut it, given everything inside the class is a
  ## reference.
  cloneable=FALSE,

  public=list(
    buffer=NULL,
    head=NULL,
    tail=NULL,

    initialize=function(size) {
      self$buffer <- circle_buffer_env_create(size)
      self$reset()
    },

    reset=function() {
      self$head <- self$buffer
      self$tail <- self$buffer
      self$buffer$.used <- 0L
    },

    duplicate=function() {
      circle_buffer_env_duplicate(self)
    },

    size=function() self$buffer$.size,
    ## bytes_data
    ## stride
    used=function() self$buffer$.used,
    free=function() self$size() - self$used(),

    empty=function() self$used() == 0L,
    full=function() self$used() == self$size(),

    ## Mostly debugging:
    head_pos=function() distance_forward(self$buffer, self$head),
    tail_pos=function() distance_forward(self$buffer, self$tail),

    head_data=function() {
      check_buffer_underflow(self, 1L)
      self$head$.prev$data
    },
    tail_data=function() {
      check_buffer_underflow(self, 1L)
      self$tail$data
    },

    ## Start getting strong divergence here:
    set=function(data, n) {
      for (i in seq_len(min(n, self$size))) {
        write_to_head(self, data)
      }
    },

    ## TODO: What about writing *single* things to the head and tail
    ## here?  The loop here is likely to be very annoying!  In some
    ## ways the loop is the natural analogue of memcpy but it's not
    ## really needed either.
    ##
    ## It will be probably useful on occassion to add a data.frame
    ## here row-by-row and things like that.  So we might have to make
    ## this one generic -- or find something that makes things
    ## iterable.
    push=function(data, iterate=TRUE) {
      if (iterate) {
        for (el in data) {
          write_to_head(self, el)
        }
      } else {
        write_to_head(self, data)
      }
    },

    take=function(n) {
      dat <- read_from_tail(self, n)
      self$tail <- dat[[2L]]
      self$buffer$.used <- self$buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read=function(n) read_from_tail(self, n)[[1L]],
    copy=function(dest, n) circle_buffer_env_copy(self, dest, n),

    head_offset_data=function(n) {
      check_buffer_underflow(self, n + 1L)
      move_backward(self$head$.prev, n)$data
    },
    tail_offset_data=function(n) {
      check_buffer_underflow(self, n + 1L)
      move_forward(self$tail, n)$data
    },

    ## This is the unusual direction...
    take_head=function(n) {
      dat <- read_from_head(self, n)
      self$head <- dat[[2L]]
      self$buffer$.used <- self$buffer$.used - as.integer(n)
      dat[[1L]]
    },

    read_head=function(n) {
      read_from_head(self, n)[[1L]]
    },

    ## This might come out as simply a free S3 method/function
    to_list=function() {
      read_from_tail(self, self$used())[[1L]]
    }
  ))

read_from_tail <- function(buf, n) {
  check_buffer_underflow(buf, n)
  tail <- buf$tail
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    ret[[i]] <- tail$data
    tail <- tail$.next
  }
  list(ret, tail)
}

read_from_head <- function(buf, n) {
  check_buffer_underflow(buf, n)
  head <- buf$head

  ret <- vector("list", n)
  for (i in seq_len(n)) {
    head <- head$.prev
    ret[[i]] <- head$data
  }
  list(ret, head)
}

write_to_head <- function(buf, data) {
  buf$head$data <- data
  buf$head <- buf$head$.next
  if (buf$buffer$.used < buf$size()) {
    buf$buffer$.used <- buf$buffer$.used + 1L
  } else {
    buf$tail <- buf$tail$.next
  }
}

check_buffer_underflow <- function(obj, requested) {
  ## TODO: Perhaps an S3 condition?
  if (requested > obj$used()) {
    stop(sprintf("Buffer underflow: requested %d, available %d",
                 requested, obj$used()))
  }
}

buffer_underflow <- function(requested, available) {
  ## TODO: classed error?
  stop(sprintf("Buffer underflow: requested %d, available %d",
               requested, available))
}

distance_forward <- function(head, target) {
  i <- 0L
  while (!identical(target, head)) {
    i <- i + 1L
    head <- head$.next
  }
  i
}

distance_backward <- function(tail, target) {
  i <- 0L
  while (!identical(target, tail)) {
    i <- i + 1L
    tail <- tail$.prev
  }
  i
}

move_forward <- function(x, n) {
  for (i in seq_len(n)) {
    x <- x$.next
  }
  x
}

move_backward <- function(x, n) {
  for (i in seq_len(n)) {
    x <- x$.prev
  }
  x
}

circle_buffer_env_copy <- function(buf, dest, n) {
  check_buffer_underflow(buf, n)

  tail <- buf$tail
  for (i in seq_len(n)) {
    dest$push(tail$data)
    tail <- tail$.next
  }

  buf$tail <- tail
  buf$buffer$.used <- buf$buffer$.used - as.integer(n)
}
