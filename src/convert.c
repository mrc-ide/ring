#include "convert.h"

SEXP logical_to_bytes(SEXP x) {
  int * data = INTEGER(x);
  size_t len = LENGTH(x) * sizeof(int);
  SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_logical(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(int);
  SEXP ret = PROTECT(Rf_allocVector(LGLSXP, n));
  memcpy(INTEGER(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP int_to_bytes(SEXP x) {
  int * data = INTEGER(x);
  size_t len = LENGTH(x) * sizeof(int);
  SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_int(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(int);
  SEXP ret = PROTECT(Rf_allocVector(INTSXP, n));
  memcpy(INTEGER(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP double_to_bytes(SEXP x) {
  double * data = REAL(x);
  size_t len = LENGTH(x) * sizeof(double);
  SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_double(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(double);
  SEXP ret = PROTECT(Rf_allocVector(REALSXP, n));
  memcpy(REAL(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP complex_to_bytes(SEXP x) {
  Rcomplex * data = COMPLEX(x);
  size_t len = LENGTH(x) * sizeof(Rcomplex);
  SEXP ret = PROTECT(Rf_allocVector(RAWSXP, len));
  memcpy(RAW(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP bytes_to_complex(SEXP x) {
  void * data = RAW(x);
  size_t len = LENGTH(x);
  size_t n = len / sizeof(Rcomplex);
  SEXP ret = PROTECT(Rf_allocVector(CPLXSXP, n));
  memcpy(COMPLEX(ret), data, len);
  UNPROTECT(1);
  return ret;
}

SEXP sizeof_types(void) {
  SEXP sizes = PROTECT(Rf_allocVector(INTSXP, 4));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 4)); // R style names
  int * s = INTEGER(sizes);
  SET_STRING_ELT(nms, 0, Rf_mkChar("logical"));
  s[0] = sizeof(int);
  SET_STRING_ELT(nms, 1, Rf_mkChar("integer"));
  s[1] = sizeof(int);
  SET_STRING_ELT(nms, 2, Rf_mkChar("double"));
  s[2] = sizeof(double);
  SET_STRING_ELT(nms, 3, Rf_mkChar("complex"));
  s[3] = sizeof(Rcomplex);
  Rf_setAttrib(sizes, R_NamesSymbol, nms);
  UNPROTECT(2);
  return sizes;
}
