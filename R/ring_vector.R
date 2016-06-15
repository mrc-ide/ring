##' Simulate an atomic vector with a ring buffer.  This exists mostly
##' as an example of use of a ring buffer designed to work with R
##' functions that do not know (or care) that the object is
##' implemented with a ring buffer behind the scenes.  Elements will
##' be added at the end of the vector and taken from the beginning.
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

ring_vector_push <- function(x, data, check=TRUE) {
  if (check) {
    ring_vector_compatible(x, data)
  }
  x$buf$push(data)
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
    if (is.logical(i)) {
      i <- which(i)
    } else if (!(is.integer(i) || is.numeric(i))) {
      stop("Invalid type for index")
    } else if (any(i < 0)) {
      i <- seq_len(x$used())[i]
    }

    ret <- create[[x$type]](length(i))
    ## TODO: in theory this should be done by doing relative offsets
    ## against the last place we looked but that's complicated.  Doing
    ## that efficiently would involve taking the *order* of i and
    ## moving along that by the difference in the offset and current
    ## position.  This is also only an optimisation for the env
    ## version.  But probably also worth doing this over unique values
    ## of i?
    for (j in seq_along(i)) {
      ret[j] <- x$buf$tail_offset_data(i[[j]] - 1L)
    }
  }

  ret
}

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
