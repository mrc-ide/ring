bytes_to_logical <- function(x) {
  as.logical(bytes_to_int(x))
}
logical_to_bytes <- function(x) {
  int_to_bytes(x)
}
bytes_to_int <- function(x) {
  .Call(Cbytes_to_int, x)
}
int_to_bytes <- function(x) {
  .Call(Cint_to_bytes, x)
}
bytes_to_double <- function(x) {
  .Call(Cbytes_to_double, x)
}
double_to_bytes <- function(x) {
  .Call(Cdouble_to_bytes, x)
}
bytes_to_complex <- function(x) {
  .Call(Cbytes_to_complex, x)
}
complex_to_bytes <- function(x) {
  .Call(Ccomplex_to_bytes, x)
}

convert_to <- list(logical=logical_to_bytes,
                   integer=int_to_bytes,
                   double=double_to_bytes,
                   complex=complex_to_bytes)
convert_from <- list(logical=bytes_to_logical,
                     integer=bytes_to_int,
                     double=bytes_to_double,
                     complex=bytes_to_complex)
create <- list(logical=logical,
               integer=integer,
               double=double,
               complex=complex)
