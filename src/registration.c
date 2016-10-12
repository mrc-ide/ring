// Dynamic routine registration; this speeds up using the compiled
// routines from .Call() because the symbol lookup is done once at
// package load and not when the routines are used (see R-exts).

#include "ring_r.h"
#include "convert.h"

#include <R_ext/Rdynload.h>

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

static const R_CallMethodDef call_methods[] = {
  {"Cring_buffer_create",      (DL_FUNC) &R_ring_buffer_create,      3},
  {"Cring_buffer_duplicate",   (DL_FUNC) &R_ring_buffer_duplicate,   1},
  {"Cring_buffer_grow",        (DL_FUNC) &R_ring_buffer_grow,        3},
  {"Cring_buffer_bytes_data",  (DL_FUNC) &R_ring_buffer_bytes_data,  1},
  {"Cring_buffer_size",        (DL_FUNC) &R_ring_buffer_size,        2},
  {"Cring_buffer_stride",      (DL_FUNC) &R_ring_buffer_stride,      1},
  {"Cring_buffer_is_full",     (DL_FUNC) &R_ring_buffer_is_full,     1},
  {"Cring_buffer_is_empty",    (DL_FUNC) &R_ring_buffer_is_empty,    1},
  {"Cring_buffer_head",        (DL_FUNC) &R_ring_buffer_head,        1},
  {"Cring_buffer_tail",        (DL_FUNC) &R_ring_buffer_tail,        1},
  {"Cring_buffer_data",        (DL_FUNC) &R_ring_buffer_data,        1},
  {"Cring_buffer_head_pos",    (DL_FUNC) &R_ring_buffer_head_pos,    2},
  {"Cring_buffer_tail_pos",    (DL_FUNC) &R_ring_buffer_tail_pos,    2},
  {"Cring_buffer_free",        (DL_FUNC) &R_ring_buffer_free,        2},
  {"Cring_buffer_used",        (DL_FUNC) &R_ring_buffer_used,        2},
  {"Cring_buffer_reset",       (DL_FUNC) &R_ring_buffer_reset,       2},
  {"Cring_buffer_set",         (DL_FUNC) &R_ring_buffer_set,         3},
  {"Cring_buffer_push",        (DL_FUNC) &R_ring_buffer_push,        2},
  {"Cring_buffer_take",        (DL_FUNC) &R_ring_buffer_take,        2},
  {"Cring_buffer_read",        (DL_FUNC) &R_ring_buffer_read,        2},
  {"Cring_buffer_take_head",   (DL_FUNC) &R_ring_buffer_take_head,   2},
  {"Cring_buffer_read_head",   (DL_FUNC) &R_ring_buffer_read_head,   2},
  {"Cring_buffer_copy",        (DL_FUNC) &R_ring_buffer_copy,        3},
  {"Cring_buffer_mirror",      (DL_FUNC) &R_ring_buffer_mirror,      2},
  {"Cring_buffer_tail_offset", (DL_FUNC) &R_ring_buffer_tail_offset, 2},
  {"Cring_buffer_head_offset", (DL_FUNC) &R_ring_buffer_head_offset, 2},
  {"Cring_buffer_head_set",    (DL_FUNC) &R_ring_buffer_head_set,    2},
  {"Cring_buffer_head_data",   (DL_FUNC) &R_ring_buffer_head_data,   1},
  {"Cring_buffer_head_advance",(DL_FUNC) &R_ring_buffer_head_advance,1},
  // conversion code, used in ring_buffer_bytes_typed
  {"Clogical_to_bytes",        (DL_FUNC) &logical_to_bytes,          1},
  {"Cbytes_to_logical",        (DL_FUNC) &bytes_to_logical,          1},
  {"Cint_to_bytes",            (DL_FUNC) &int_to_bytes,              1},
  {"Cbytes_to_int",            (DL_FUNC) &bytes_to_int,              1},
  {"Cdouble_to_bytes",         (DL_FUNC) &double_to_bytes,           1},
  {"Cbytes_to_double",         (DL_FUNC) &bytes_to_double,           1},
  {"Ccomplex_to_bytes",        (DL_FUNC) &complex_to_bytes,          1},
  {"Cbytes_to_complex",        (DL_FUNC) &bytes_to_complex,          1},
  // Utility
  {"Cassert_scalar_size",      (DL_FUNC) &assert_scalar_size,        2},
  {NULL,                       NULL,                                 0}
};

// Package initialisation, required for the registration
void R_init_ring(DllInfo *info) {
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
}
