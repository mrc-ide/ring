// Dynamic routine registration; this speeds up using the compiled
// routines from .Call() because the symbol lookup is done once at
// package load and not when the routines are used (see R-exts).

#include "ring_r.h"
#include "convert.h"

#include <R_ext/Rdynload.h>
static const R_CallMethodDef call_methods[] = {
  {"Cring_buffer_create",      (DL_FUNC) &R_ring_buffer_create,      2},
  {"Cring_buffer_duplicate",   (DL_FUNC) &R_ring_buffer_duplicate,   1},
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
  {"Cring_buffer_reset",       (DL_FUNC) &R_ring_buffer_reset,       1},
  {"Cring_buffer_set",         (DL_FUNC) &R_ring_buffer_set,         3},
  {"Cring_buffer_push",        (DL_FUNC) &R_ring_buffer_push,        2},
  {"Cring_buffer_take",        (DL_FUNC) &R_ring_buffer_take,        2},
  {"Cring_buffer_read",        (DL_FUNC) &R_ring_buffer_read,        2},
  {"Cring_buffer_take_head",   (DL_FUNC) &R_ring_buffer_take_head,   2},
  {"Cring_buffer_read_head",   (DL_FUNC) &R_ring_buffer_read_head,   2},
  {"Cring_buffer_copy",        (DL_FUNC) &R_ring_buffer_copy,        3},
  {"Cring_buffer_tail_offset", (DL_FUNC) &R_ring_buffer_tail_offset, 2},
  {"Cring_buffer_head_offset", (DL_FUNC) &R_ring_buffer_head_offset, 2},
  // conversion code, used in ring_buffer_bytes_typed
  {"Clogical_to_bytes",        (DL_FUNC) &logical_to_bytes,          1},
  {"Cbytes_to_logical",        (DL_FUNC) &bytes_to_logical,          1},
  {"Cint_to_bytes",            (DL_FUNC) &int_to_bytes,              1},
  {"Cbytes_to_int",            (DL_FUNC) &bytes_to_int,              1},
  {"Cdouble_to_bytes",         (DL_FUNC) &double_to_bytes,           1},
  {"Cbytes_to_double",         (DL_FUNC) &bytes_to_double,           1},
  {"Ccomplex_to_bytes",        (DL_FUNC) &complex_to_bytes,          1},
  {"Cbytes_to_complex",        (DL_FUNC) &bytes_to_complex,          1},
  {NULL,                       NULL,                                 0}
};

// Package initialisation, required for the registration
void R_init_ring(DllInfo *info) {
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
}
