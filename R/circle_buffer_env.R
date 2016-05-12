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

circle_buffer_env <- function(size) {
  .R6_circle_buffer_env$new(size)
}

##' @importFrom R6 R6Class
.R6_circle_buffer_env <- R6::R6Class(
  "circle_buffer_env",

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

    size=function() self$buffer$.size,
    used=function() self$buffer$.used,
    free=function() self$size() - self$used(),

    empty=function() self$used() == 0L,
    full=function() self$used() == self$size(),

    ## Mostly debugging:
    head_pos=function() distance_forward(self$buffer, self$head),
    tail_pos=function() distance_forward(self$buffer, self$tail),

    write_to_head=function(data) {
      self$head$data <- data
      self$head <- self$head$.next
      if (self$buffer$.used < self$size()) {
        self$buffer$.used <- self$buffer$.used + 1L
      } else {
        self$tail <- self$tail$.next
      }
    },

    tail_data=function() {
      check_buffer_underflow(self, 1L)
      self$tail$data
    },

    tail_offset=function(count) {
      check_buffer_underflow(self, count + 1L)
      move_forward(self$tail, count)
    },

    head_offset=function(count) {
      check_buffer_underflow(self, count + 1L)
      move_backward(self$head$.prev, count)
    },

    tail_offset_data=function(count) self$tail_offset(count)$data,
    head_offset_data=function(count) self$head_offset(count)$data,

    head_data=function() {
      check_buffer_underflow(self, 1L)
      self$head$.prev$data
    },

    set=function(data, count) {
      for (i in seq_len(min(count, self$size))) {
        self$write_to_head(data)
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
    copy_into=function(data) {
      for (el in data) {
        self$write_to_head(el)
      }
    },

    take_from=function(n) {
      check_buffer_underflow(self, n)
      dat <- read_from_tail(self$tail, n)
      self$tail <- dat[[2L]]
      self$buffer$.used <- self$buffer$.used - as.integer(n)
      dat[[1L]]
    },

    ## This is the unusual direction...
    take_from_head=function(n) {
      check_buffer_underflow(self, n)
      dat <- read_from_head(self$head, n)
      self$head <- dat[[2L]]
      self$buffer$.used <- self$buffer$.used - as.integer(n)
      dat[[1L]]
    },

    tail_read=function(n) {
      check_buffer_underflow(self, n)
      read_from_tail(self$tail, n)[[1L]]
    },

    head_read=function(n) {
      check_buffer_underflow(self, n)
      read_from_head(self$head, n)[[1L]]
    },

    ## This might come out as simply a free S3 method/function
    to_list=function() {
      read_from_tail(self$tail, self$used())[[1L]]
    }
  ))

read_from_tail <- function(tail, n) {
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    ret[[i]] <- tail$data
    tail <- tail$.next
  }
  list(ret, tail)
}

read_from_head <- function(head, n) {
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    head <- head$.prev
    ret[[i]] <- head$data
  }
  list(ret, head)
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
