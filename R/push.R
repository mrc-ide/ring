##' Push elements onto a ring object
##' @title Push into a ring
##' @param x A ring buffer of some sort
##' @param data Data to push into the ring
##' @param ... Additional arguments passed through to methods
##' @export
push <- function(x, data, ...) {
  UseMethod("push")
}
