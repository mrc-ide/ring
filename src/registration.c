#include "convert.h"
#include "ring_r.h"
#include "test.h"
#include "util.h"

#include <R_ext/Rdynload.h>
#include <Rversion.h>

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
  {"Csizeof_types",            (DL_FUNC) &sizeof_types,              0},
  // Utility
  {"Cassert_scalar_size",      (DL_FUNC) &assert_scalar_size,        2},
  // Testing
  {"Ctest_search_linear",      (DL_FUNC) &test_search_linear,        2},
  {"Ctest_search_bisect",      (DL_FUNC) &test_search_bisect,        3},
  {"Ctest_advance_head",       (DL_FUNC) &test_advance_head,         2},
  {NULL,                       NULL,                                 0}
};

// Package initialisation, required for the registration
void R_init_ring(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
#endif
}
