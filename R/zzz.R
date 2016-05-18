##' @useDynLib ring, .registration = TRUE
sizes <- NULL
.onLoad <- function(...) {
  sizes <<- setNames(.Call("sizeof_types", PACKAGE="ring"),
                     c("logical", "integer", "double", "complex"))
}
