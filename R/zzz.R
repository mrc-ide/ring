##' @useDynLib ring, .registration = TRUE
sizes <- NULL
.onLoad <- function(libname, pkgname) {
  ## Not sure why this doesn't trigger with covr, but it doesn't much
  ## matter: this runs, unconditionally.
  sizes <<- .Call(Csizeof_types) # nocov
}
