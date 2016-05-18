random_bytes <- function(n) {
  as.raw(sample(2^8, n, TRUE) - 1L)
}

pad <- function(x, len, with=0) {
  c(x, rep_len(as.raw(with), len - length(x)))
}

repr <- function(x, ...) {
  as.raw(rep(x, ...))
}

fill_buffer <- function(x, len) {
  if (is.character(x)) {
    x <- charToRaw(x)
  } else {
    x <- as.raw(x)
  }
  rep(x, length.out=len)
}

## The length of test_pattern should not fit naturally into size, or
## else it won't be possible to detect proper wrapping of the head
## pointer.
test_pattern <- function(size) {
  fill_buffer((size + 1) * 2, "abcdefghijk")
}

first <- function(x) {
  head(x, 1L)
}

last <- function(x) {
  tail(x, 1L)
}
