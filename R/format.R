format_ring_buffer <- function(x, ...) {
  if (is.null(x$.type)) {
    ret <- sprintf("<Ring Buffer (%s)>", class(x)[[1L]])
  } else {
    ret <- sprintf("<Ring Buffer (%s | %s)>", class(x)[[1L]], x$.type)
  }
  nms <- setdiff(ls(x), "initialize")
  vals <- vapply(nms, function(i) deparse(args(x[[i]]))[[1L]], character(1))
  details <- sprintf("    %s: %s", nms, trimws(vals))
  paste(c(ret, "  Public:", details), collapse = "\n")
}

##' @export
format.ring_buffer_env <- format_ring_buffer
##' @export
format.ring_buffer_bytes <- format_ring_buffer
##' @export
format.ring_buffer_translate <- format_ring_buffer
