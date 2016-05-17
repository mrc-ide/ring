#include "convert.h"

SEXP int_to_bytes(SEXP x) {
  int * data = INTEGER(x);
  size_t len = LENGTH(x) * sizeof(int);
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_int(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(int);
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  memcpy(INTEGER(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP double_to_bytes(SEXP x) {
  double * data = REAL(x);
  size_t len = LENGTH(x) * sizeof(double);
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_double(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(double);
  SEXP ret = PROTECT(allocVector(REALSXP, n));
  memcpy(REAL(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP complex_to_bytes(SEXP x) {
  Rcomplex * data = COMPLEX(x);
  size_t len = LENGTH(x) * sizeof(Rcomplex);
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_complex(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(Rcomplex);
  SEXP ret = PROTECT(allocVector(CPLXSXP, n));
  memcpy(COMPLEX(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP sizeof_types() {
  SEXP ret = PROTECT(allocVector(INTSXP, 4));
  int * tmp = INTEGER(ret);
  tmp[0] = sizeof(int);
  tmp[1] = sizeof(int);
  tmp[2] = sizeof(double);
  tmp[3] = sizeof(Rcomplex);
  UNPROTECT(1);
  return ret;
}
