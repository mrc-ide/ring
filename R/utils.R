assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
  assert_nonmissing(x, name)
}
assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x) && !is.na(x)) {
    stop(sprintf("%s must be logical", name), call. = FALSE)
  }
}
assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call. = FALSE)
  }
}
assert_nonmissing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    stop(sprintf("%s must not be NA", name), call. = FALSE)
  }
}

assert_function <- function(x, name = deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x) && !is.na(x)) {
    stop(sprintf("%s must be a character", name), call. = FALSE)
  }
}

## Like with older Rcpp:
include_flags <- function(stdout = TRUE) {
  value <- paste0("-I", system.file("include", package = "ring"))
  if (stdout) {
    cat(value)
  } else {
    value
  }
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

match_value <- function(x, choices, name = deparse(substitute(x))) {
  assert_character(x, name)
  assert_scalar(x, name)
  if (!(x %in% choices)) {
    stop(sprintf("Invalid value for '%s'; must be one of %s", name,
                 paste(dQuote(choices), collapse = ", ")))
  }
}

## This one is a bit different as I've tuned it for speed
C_assert_size <- function(x, name) {
  .Call(Cassert_scalar_size, x, name)
}
