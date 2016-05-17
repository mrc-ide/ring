##' @useDynLib circle, .registration = TRUE
sizes <- NULL
.onLoad <- function(...) {
  sizes <<- setNames(.Call("sizeof_types", PACKAGE="circle"),
                     c("logical", "integer", "numeric", "complex"))
}
