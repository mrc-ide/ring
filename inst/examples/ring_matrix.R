### Be sure to source the ring_vector file first, or copy over the
### functions:
###
### * create
### * ring_vector_index
###
## * `nr_max`: The maximum number of rows
## * `nc`: The number of columns in the matrix
ring_matrix <- function(nr_max, nc, type, environment = TRUE) {
  type <- match.arg(type, names(ring:::sizes))
  if (environment) {
    buf <- ring::ring_buffer_env(nr_max)
  } else {
    buf <- ring::ring_buffer_bytes_typed(nr_max, type, nc)
  }
  ret <- list(buf = buf, nr_max = as.integer(nr_max), nc = as.integer(nc),
              type = type, environment = environment)
  class(ret) <- "ring_matrix"
  ret
}

ring_matrix_push <- function(buffer, data, check = TRUE, ...) {
  if (check) {
    ring_matrix_compatible(buffer, data)
  }
  if (buffer$environment) {
    if (is.matrix(data)) {
      for (i in seq_len(nrow(data))) {
        buffer$buf$push(data[i, ], FALSE)
      }
    } else {
      buffer$buf$push(data, FALSE)
    }
  } else {
    buffer$buf$push(if (is.matrix(data)) t(data) else data)
  }
}

ring_matrix_compatible <- function(x, data) {
  if (storage.mode(data) != x$type) {
    stop("Expected storage.mode of ", x$type)
  }
  if (is.matrix(data)) {
    if (ncol(data) != x$nc) {
      stop(sprintf("Expected a matrix of '%d' columns", x$nc))
    }
  } else {
    if (length(data) != x$nc) {
      stop(sprintf("Expected a matrix of '%d' columns", x$nc))
    }
  }
  TRUE
}

ring_matrix_get <- function(x, i = NULL) {
  if (is.null(i)) {
    dat <- x$buf$read(x$buf$used())
    if (x$environment) {
      if (length(dat) == 0L) {
        dat <- create[[x$type]]()
      } else {
        dat <- unlist(dat)
      }
    }
    ret <- matrix(dat, ncol = x$nc, byrow = TRUE)
  } else {
    len <- x$buf$used()
    i <- ring_vector_index(i, len)
    ret <- matrix(create[[x$type]](length(i) * x$nc), length(i), x$nc)
    for (j in seq_along(i)) {
      k <- i[[j]]
      ret[j, ] <- if (k <= len) x$buf$tail_offset(k - 1L) else NA
    }
  }

  if (!is.null(x$colnames)) {
    colnames(ret) <- x$colnames
  }

  ret
}

## S3 support
push.ring_matrix <- ring_matrix_push

dim.ring_matrix <- function(x, ...) {
  c(x$buf$used(), x$nc)
}

head.ring_matrix <- function(x, n = 6L, ...) {
  head.matrix(x, n, ...)
}

tail.ring_matrix <- function(x, n = 6L, ...) {
  tail.matrix(x, n, FALSE, ...)
}

`[.ring_matrix` <- function(x, i, j, ..., drop = TRUE) {
  if (missing(i)) {
    if (missing(j)) {
      ring_matrix_get(x, NULL)
    } else {
      ring_matrix_get(x, NULL)[, j, drop = drop]
    }
  } else if (is.matrix(i)) {
    if (!missing(j)) {
      stop("subscript out of bounds") # same error as [.matrix
    }
    j <- sort(unique(i[, 1L]))
    ring_matrix_get(x, j)[cbind(match(i[, 1L], j), i[, 2L])]
  } else {
    ring_matrix_get(x, i)[, j, drop = drop]
  }
}

dimnames.ring_matrix <- function(x, ...) {
  if (is.null(x$colnames)) {
    NULL
  } else {
    list(NULL, x$colnames)
  }
}

`dimnames<-.ring_matrix` <- function(x, value) {
  if (is.null(value)) {
    x$colnames <- NULL
  } else if (!is.list(value) || length(value) != 2L) {
    stop("Invalid input for dimnames")
  } else {
    if (!is.null(value[[1L]])) {
      stop("Cannot set rownames of a ring matrix")
    }
    val <- value[[2L]]
    if (!is.null(val) && length(val) != x$nc) {
      stop("Invalid length dimnames")
    }
    x$colnames <- val
  }
  x
}

as.matrix.ring_matrix <- function(x, ...) {
  ring_matrix_get(x, NULL)
}

cbind.ring_matrix <- function(...) {
  stop("It is not possible to cbind() ring_matrices (use as.matrix first?)")
}

rbind.ring_matrix <- function(...) {
  if (!inherits(..1, "ring_matrix")) {
    args <- list(...)
    i <- vapply(args, inherits, logical(1), "ring_matrix")
    args[i] <- lapply(args[i], as.matrix)
    eval(as.call(c(quote(rbind), args)))
  } else {
    x <- ..1
    args <- list(...)[-1]
    lapply(args, ring_matrix_compatible, x = x)
    for (m in args) {
      ring_matrix_push(x, m)
    }
    x
  }
}

length.ring_matrix <- function(x) {
  x$buf$used() * x$nc
}

registerS3method("[", "ring_matrix", `[.ring_matrix`, environment())
registerS3method("dim", "ring_matrix", dim.ring_matrix, environment())
registerS3method("dimnames", "ring_matrix", dimnames.ring_matrix, environment())
registerS3method("dimnames<-", "ring_matrix", `dimnames<-.ring_matrix`,
                 environment())
registerS3method("as.matrix", "ring_matrix", cbind.ring_matrix, environment())
registerS3method("cbind", "ring_matrix", cbind.ring_matrix, environment())
registerS3method("rbind", "ring_matrix", rbind.ring_matrix, environment())
