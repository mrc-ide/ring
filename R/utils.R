assert_scalar_logical <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}
assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x) && !is.na(x)) {
    stop(sprintf("%s must be logical", name), call.=FALSE)
  }
}
assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}
