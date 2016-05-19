##' @useDynLib ring, .registration = TRUE
##' @import utils
sizes <- NULL
.onLoad <- function(...) {
  s <- .Call("sizeof_types", PACKAGE="ring")
  names(s) <- c("logical", "integer", "double", "complex")
  sizes <<- s
}
