## Simulate an atomic vector or matrix with a ring buffer.  These
## functions exist mostly as an example of use of a ring buffer
## designed to work with R functions that do not know (or care) that
## the object is implemented with a ring buffer behind the scenes.
## Elements will be added at the end of the vector and taken from the
## beginning.
##
## Note that because the matrix is stored row-wise but R stores
## matrices column wise, there is a lot of data transposing going on
## here.  If something like this was needed for performance then
## you'd want to redo this with column storage.
##
## The `push` function is generic and can be used to push
## elements onto either a `ring_vector` or a `ring_matrix`.
##
## Note that these are implemented more as proof-of-concepts rather
## than really robust data types.
##
## * `length_max`: The maximum number of elements
## * `type`: The type of storage.  Can be "logical", "integer",
##   "double", or "complex"
## * `environment`: Logical indicating if we should use an environment
##   buffer (`ring_buffer_env`) or a bytes buffer
##   (`ring_buffer_bytes`).
ring_vector <- function(length_max, type, environment = TRUE) {
  type <- match.arg(type, names(create))
  if (environment) {
    buf <- ring::ring_buffer_env(length_max)
  } else {
    buf <- ring::ring_buffer_bytes_typed(length_max, type, 1L)
  }
  ret <- list(buf = buf, length_max = as.integer(length_max),
              type = type, environment = environment)
  class(ret) <- "ring_vector"
  ret
}

ring_vector_push <- function(buffer, data, check = TRUE, ...) {
  if (check) {
    ring_vector_compatible(buffer, data)
  }
  buffer$buf$push(data)
}

ring_vector_compatible <- function(x, data) {
  if (storage.mode(data) != x$type) {
    stop("Expected storage.mode of ", x$type)
  }
  TRUE
}

ring_vector_get <- function(x, i = NULL) {
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
    for (j in seq_along(i)) {
      k <- i[[j]]
      ret[j] <- if (k <= len) x$buf$tail_offset(k - 1L) else NA
    }
  }

  ret
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
    i <- seq_len(len)[i]
  }
  i
}

## S3 support:
push <- function(buffer, data, ...) {
  UseMethod("push")
}

push.ring_vector <- ring_vector_push

length.ring_vector <- function(x, ...) {
  x$buf$used()
}

`[[.ring_vector` <- `[.ring_vector` <- function(x, i, ...) {
  if (missing(i)) {
    ring_vector_get(x, NULL)
  } else {
    ring_vector_get(x, i)
  }
}

c.ring_vector <- function(..., recursive = TRUE) {
  if (!inherits(..1, "ring_vector")) {
    args <- list(...)
    i <- vapply(args, inherits, logical(1), "ring_vector")
    args[i] <- lapply(args[i], as.matrix)
    eval(as.call(c(quote(rbind), args)))
  } else {
    x <- ..1
    args <- list(...)[-1]
    lapply(args, ring_vector_compatible, x = x)
    for (m in args) {
      ring_vector_push(x, m)
    }
    x
  }
}

## Support functions; these are functions used to create empty storage
## for the bytes buffers
create <- list(logical = logical,
               integer = integer,
               double = double,
               complex = complex)

registerS3method("[", "ring_vector", `[.ring_vector`, environment())
registerS3method("[[", "ring_vector", `[[.ring_vector`, environment())
registerS3method("length", "ring_vector", length.ring_vector, environment())
registerS3method("c", "ring_vector", c.ring_vector, environment())
