##' @useDynLib ring, .registration = TRUE
##' @import utils
sizes <- NULL
.onLoad <- function(...) {
  sizes <<- .Call("sizeof_types", PACKAGE="ring")
}
