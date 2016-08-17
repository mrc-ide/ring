##' Simulate a matrix with a ring buffer.  This exists mostly as an
##' example of use of a ring buffer designed to work with R functions
##' that do not know (or care) that the object is implemented with a
##' ring buffer behind the scenes.  Rows will be added at the bottom
##' of the matrix.
##'
##' Note that because the matrix is stored row-wise but R stores
##' matrices column wise, there is a lot of data transposing going on
##' here.  If something like this was needed for performance then
##' you'd want to redo this with column storage.
##
##' @title Ring matrix
##'
##' @param nr_max The maximum number of rows
##'
##' @param nc The number of columns in the matrix
##'
##' @param type The type of storage.  Can be "logical", "integer",
##'   "double", or "complex"
##'
##' @param environment Logical indicating if we should use an
##'   environment buffer (\code{\link{ring_buffer_env}}) or
##'   a bytes buffer (\code{\link{ring_buffer_bytes}}).
##
##' @export
ring_matrix <- function(nr_max, nc, type, environment=TRUE) {
  assert_scalar_logical(environment)
  type <- match.arg(type, names(sizes))
  if (environment) {
    buf <- ring_buffer_env(nr_max)
  } else {
    buf <- ring_buffer_bytes_typed(nr_max, type, nc)
  }
  ret <- list(buf=buf, nr_max=as.integer(nr_max), nc=as.integer(nc),
              type=type, environment=environment)
  class(ret) <- "ring_matrix"
  ret
}

ring_matrix_push <- function(x, data, check=TRUE, ...) {
  if (check) {
    ring_matrix_compatible(x, data)
  }
  ## NOTE: This is the only place outside of construction where we
  ## need to care about the different types of buffer.  So they're not
  ## totally substitutable but not too bad either.
  if (x$environment) {
    if (is.matrix(data)) {
      for (i in seq_len(nrow(data))) {
        x$buf$push(data[i, ], FALSE)
      }
    } else {
      x$buf$push(data, FALSE)
    }
  } else {
    x$buf$push(if (is.matrix(data)) t(data) else data)
  }
}

##' @export
push.ring_matrix <- ring_matrix_push

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

ring_matrix_get <- function(x, i=NULL) {
  if (is.null(i)) {
    dat <- x$buf$read(x$buf$used())
    if (x$environment) {
      if (length(dat) == 0L) {
        dat <- create[[x$type]]()
      } else {
        dat <- unlist(dat)
      }
    }
    ret <- matrix(dat, ncol=x$nc, byrow=TRUE)
  } else {
    if (is.logical(i)) {
      i <- which(i)
    } else if (!(is.integer(i) || is.numeric(i))) {
      stop("Invalid type for index")
    } else if (any(i < 0)) {
      i <- seq_len(x$used())[i]
    }

    ret <- matrix(create[[x$type]](length(i) * x$nc), length(i), x$nc)
    ## TODO: in theory this should be done by doing relative offsets
    ## against the last place we looked but that's complicated.  Doing
    ## that efficiently would involve taking the *order* of i and
    ## moving along that by the difference in the offset and current
    ## position.  This is also only an optimisation for the env
    ## version.  But probably also worth doing this over unique values
    ## of i?
    for (j in seq_along(i)) {
      ret[j, ] <- x$buf$tail_offset_data(i[[j]] - 1L)
    }
  }

  if (!is.null(x$colnames)) {
    colnames(ret) <- x$colnames
  }

  ret
}

##' @export
dim.ring_matrix <- function(x, ...) {
  c(x$buf$used(), x$nc)
}

##' @export
head.ring_matrix <- function(x, n = 6L, ...) {
  head.matrix(x, n, ...)
}

##' @export
tail.ring_matrix <- function(x, n = 6L, ...) {
  tail.matrix(x, n, FALSE, ...)
}

##' @export
`[.ring_matrix` <- function(x, i, j, ..., drop=TRUE) {
  if (missing(i)) {
    if (missing(j)) {
      ring_matrix_get(x, NULL)
    } else {
      ring_matrix_get(x, NULL)[, j, drop=drop]
    }
  } else if (is.matrix(i)) {
    if (!missing(j)) {
      stop("subscript out of bounds") # same error as [.matrix
    }
    j <- sort(unique(i[, 1L]))
    ring_matrix_get(x, j)[cbind(match(i[, 1L], j), i[, 2L])]
  } else {
    ring_matrix_get(x, i)[, j, drop=drop]
  }
}

##' @export
dimnames.ring_matrix <- function(x, ...) {
  if (is.null(x$colnames)) {
    NULL
  } else {
    list(NULL, x$colnames)
  }
}

##' @export
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

##' @export
as.matrix.ring_matrix <- function(x, ...) {
  ring_matrix_get(x, NULL)
}

## NOTE: Roxygen complains about S3method being deprecated here, but
## it's incapable of doing the right thing either (rbind and cbind are
## special and don't obviously look like S3 metods)

##' @S3method cbind ring_matrix
cbind.ring_matrix <- function(...) {
  stop("It is not possible to cbind() ring_matrices (use as.matrix first?)")
}

##' @S3method rbind ring_matrix
rbind.ring_matrix <- function(...) {
  if (!inherits(..1, "ring_matrix")) {
    args <- list(...)
    i <- vapply(args, inherits, logical(1), "ring_matrix")
    args[i] <- lapply(args[i], as.matrix)
    eval(as.call(c(quote(rbind), args)))
  } else {
    x <- ..1
    args <- list(...)[-1]
    ## TODO: does not deal with other ring buffers here yet.
    lapply(args, ring_matrix_compatible, x=x)
    for (m in args) {
      ring_matrix_push(x, m)
    }
    x
  }
}

##' @export
length.ring_matrix <- function(x) {
  x$buf$size()
}
