#include "util.h"

// This is used in the environment buffer to give nice error messages
// without slowing us down too badly.
//
// I've tried using .External but it still looks like the promises are
// forced causing a big slowdown.
SEXP assert_scalar_size(SEXP x, SEXP r_name) {
  const char * name = CHAR(STRING_ELT(r_name, 0));
  if (length(x) != 1) {
    Rf_error("%s must be a scalar", name);
  }
  int ix;
  if (TYPEOF(x) == INTSXP) {
    ix = INTEGER(x)[0];
    if (ix == NA_INTEGER) {
      Rf_error("%s must not be NA", name);
    }
  } else if (TYPEOF(x) == REALSXP) {
    double rx = REAL(x)[0];
    if (!R_FINITE(rx)) {
      Rf_error("%s must not be NA", name);
    }
    ix = (int) rx;
    if (fabs(rx - ix) > 1e-8) {
      Rf_error("%s must be an integer", name);
    }
  } else {
    Rf_error("%s must be an integer", name);
  }
  if (ix < 0) {
    Rf_error("%s must be nonnegative", name);
  }
  return R_NilValue;
}
