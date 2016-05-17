## This is really an example of use, but it's a common type so we'll
## implement it here to prevent duplication of effort.
##
## The idea is that we have a matrix of one type (logical, int,
## double, complex) implemented as

## NOTE: This is stored transposed to the original data at the moment.
circle_matrix <- function(nr_max, nc, type, environment=TRUE) {
  type <- match.arg(type, names(sizes))
  if (environment) {
    buf <- circle_buffer_env(nr_max)
  } else {
    buf <- circle_buffer_bytes_typed(nr_max, create[[type]](nc))
  }
  ret <- list(buf=buf, nr_max=nr_max, nc=nc, type=type,
              environment=environment)
  class(ret) <- c("circle_matrix_environment", "circle_matrix")
  ret
}

circle_matrix_push <- function(x, data) {
  ## Allow a vector of nc elements here too?
  if (!is.matrix(data)) {
    stop("Expected a matrix for 'data'")
  }
  if (ncol(data) != x$nc) {
    stop(sprintf("Expected a matrix of '%d' columns", x$nc))
  }
  if (x$environment) {
    for (i in seq_len(nrow(data))) {
      x$buf$push(data[i, ], FALSE)
    }
  } else {
    x$buf$push(t(data))
  }
}

circle_matrix_get <- function(x, i=NULL) {
  if (is.null(i)) {
    used <- x$buf$used()
    dat <- x$buf$read(x$buf$used())
    ret <- matrix(unlist(dat), ncol=x$nc, byrow=TRUE)
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
dim.circle_matrix <- function(x, ...) {
  c(x$buf$used(), x$nc)
}

##' @export
head.circle_matrix <- function(x, n = 6L, ...) {
  head.matrix(x, n, ...)
}

##' @export
tail.circle_matrix <- function(x, n = 6L, ...) {
  tail.matrix(x, n, ...)
}

##' @export
`[.circle_matrix` <- function(x, i, j, ..., drop=TRUE) {
  if (missing(i)) {
    circle_matrix_get(x, NULL)[, j, drop=drop]
  } else if (is.matrix(i)) {
    if (!missing(j)) {
      stop("subscript out of bounds") # same error as [.matrix
    }
    j <- sort(unique(i[, 1L]))
    circle_matrix_get(x, j)[cbind(match(i[, 1L], j), i[, 2L])]
  } else {
    circle_matrix_get(x, i)[, j, drop=drop]
  }
}

##' @export
dimnames.circle_matrix <- function(x, ...) {
  if (is.null(x$colnames)) {
    NULL
  } else {
    list(NULL, x$colnames)
  }
}

##' @export
`dimnames<-.circle_matrix` <- function(x, value) {
  if (is.null(NULL)) {
    x$colnames <- NULL
  } else if (!is.list(value) || length(value) != 2L) {
    stop("Invalid input for dimnames")
  } else {
    val <- value[[2L]]
    if (is.null(val)) {
      x$colnames <- NULL
    } else {
      if (length(val) != x$nc) {
        stop("Invalid length dimnames")
      }
    }
  }
  x
}
