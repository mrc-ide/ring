##' Simulate an atomic vector or matrix with a ring buffer.  These
##' functions exist mostly as an example of use of a ring buffer
##' designed to work with R functions that do not know (or care) that
##' the object is implemented with a ring buffer behind the scenes.
##' Elements will be added at the end of the vector and taken from the
##' beginning.
##'
##' Note that because the matrix is stored row-wise but R stores
##' matrices column wise, there is a lot of data transposing going on
##' here.  If something like this was needed for performance then
##' you'd want to redo this with column storage.
##'
##' The \code{push} function is generic and can be used to push
##' elements onto either a \code{ring_vector} or a \code{ring_matrix}.
##'
##' Note that these are implemented more as proof-of-concepts rather
##' than really robust data types.
##'
##' @title Ring vectors and matrices
##'
##' @param length_max The maximum number of elements
##'
##' @param type The type of storage.  Can be "logical", "integer",
##'   "double", or "complex"
##'
##' @param environment Logical indicating if we should use an
##'   environment buffer (\code{\link{ring_buffer_env}}) or
##'   a bytes buffer (\code{\link{ring_buffer_bytes}}).
##
##' @export
ring_vector <- function(length_max, type, environment=TRUE) {
  assert_scalar_logical(environment)
  type <- match.arg(type, names(sizes))
  if (environment) {
    buf <- ring_buffer_env(length_max)
  } else {
    buf <- ring_buffer_bytes_typed(length_max, type, 1L)
  }
  ret <- list(buf=buf, length_max=as.integer(length_max),
              type=type, environment=environment)
  class(ret) <- "ring_vector"
  ret
}

##' @param buffer A ring buffer of some sort
##'
##' @param data Data to push into the ring
##'
##' @param ... Additional arguments passed through to methods
##' @export
##' @rdname ring_vector
push <- function(buffer, data, ...) {
  UseMethod("push")
}

ring_vector_push <- function(buffer, data, check=TRUE, ...) {
  if (check) {
    ring_vector_compatible(buffer, data)
  }
  buffer$buf$push(data)
}

##' @export
push.ring_vector <- ring_vector_push

ring_vector_compatible <- function(x, data) {
  if (storage.mode(data) != x$type) {
    stop("Expected storage.mode of ", x$type)
  }
  TRUE
}

ring_vector_get <- function(x, i=NULL) {
  if (is.null(i)) {
    ret <- x$buf$read(x$buf$used())
    if (x$environment) {
      if (length(ret) == 0L) {
        ret <- create[[x$type]]()
      } else {
        ret <- unlist(ret)
      }
    }
  } else {
    len <- x$buf$used()
    i <- ring_vector_index(i, len)
    ret <- create[[x$type]](length(i))
    ## TODO: in theory this should be done by doing relative offsets
    ## against the last place we looked but that's complicated.  Doing
    ## that efficiently would involve taking the *order* of i and
    ## moving along that by the difference in the offset and current
    ## position.  This is also only an optimisation for the env
    ## version.  But probably also worth doing this over unique values
    ## of i?
    for (j in seq_along(i)) {
      k <- i[[j]]
      ret[j] <- if (k <= len) x$buf$tail_offset(k - 1L) else NA
    }
  }

  ret
}

## S3 support:

##' @export
length.ring_vector <- function(x, ...) {
  x$buf$used()
}

##' @export
`[.ring_vector` <- function(x, i, ...) {
  if (missing(i)) {
    ring_vector_get(x, NULL)
  } else {
    ring_vector_get(x, i)
  }
}

##' @export
`[[.ring_vector` <- `[.ring_vector`

##' @export
c.ring_vector <- function(..., recursive=TRUE) {
  if (!inherits(..1, "ring_vector")) {
    args <- list(...)
    i <- vapply(args, inherits, logical(1), "ring_vector")
    args[i] <- lapply(args[i], as.matrix)
    eval(as.call(c(quote(rbind), args)))
  } else {
    x <- ..1
    args <- list(...)[-1]
    ## TODO: does not deal with other ring buffers here yet.
    lapply(args, ring_vector_compatible, x=x)
    for (m in args) {
      ring_vector_push(x, m)
    }
    x
  }
}

ring_vector_index <- function(i, len) {
  if (is.logical(i)) {
    if (length(i) < len) {
      i <- rep_len(i, len)
    }
    i <- which(i)
  } else if (!is.numeric(i)) {
    stop("Invalid type for index")
  } else if (any(i < 0)) {
    ## Negative indexing drops things
    i <- seq_len(len)[i]
  }
  i
}
