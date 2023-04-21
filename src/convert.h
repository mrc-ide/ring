#include <R.h>
#include <Rinternals.h>

SEXP logical_to_bytes(SEXP x);
SEXP bytes_to_logical(SEXP x);
SEXP int_to_bytes(SEXP x);
SEXP bytes_to_int(SEXP x);
SEXP double_to_bytes(SEXP x);
SEXP bytes_to_double(SEXP x);
SEXP complex_to_bytes(SEXP x);
SEXP bytes_to_complex(SEXP x);
SEXP sizeof_types(void);
