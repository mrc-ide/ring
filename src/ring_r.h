#define STRICT_R_HEADERS
#include <ring/ring.h>
#include <R.h>
#include <Rinternals.h>

SEXP R_ring_buffer_create(SEXP r_size, SEXP r_stride, SEXP r_on_overflow);
SEXP R_ring_buffer_duplicate(SEXP extPtr);
SEXP R_ring_buffer_grow(SEXP extPtr, SEXP r_n, SEXP r_exact);
SEXP R_ring_buffer_size(SEXP extPtr, SEXP bytes);
SEXP R_ring_buffer_stride(SEXP extPtr);
SEXP R_ring_buffer_bytes_data(SEXP extPtr);
SEXP R_ring_buffer_is_full(SEXP extPtr);
SEXP R_ring_buffer_is_empty(SEXP extPtr);
SEXP R_ring_buffer_head(SEXP extPtr);
SEXP R_ring_buffer_tail(SEXP extPtr);
SEXP R_ring_buffer_data(SEXP extPtr);
SEXP R_ring_buffer_head_pos(SEXP extPtr, SEXP bytes);
SEXP R_ring_buffer_tail_pos(SEXP extPtr, SEXP bytes);
SEXP R_ring_buffer_free(SEXP extPtr, SEXP bytes);
SEXP R_ring_buffer_used(SEXP extPtr, SEXP bytes);
SEXP R_ring_buffer_reset(SEXP extPtr, SEXP clear);
SEXP R_ring_buffer_set(SEXP extPtr, SEXP r_data, SEXP r_n);
SEXP R_ring_buffer_push(SEXP extPtr, SEXP r_data);
SEXP R_ring_buffer_take(SEXP extPtr, SEXP r_n);
SEXP R_ring_buffer_read(SEXP extPtr, SEXP r_n);
SEXP R_ring_buffer_take_head(SEXP extPtr, SEXP r_n);
SEXP R_ring_buffer_read_head(SEXP extPtr, SEXP r_n);
SEXP R_ring_buffer_tail_offset(SEXP extPtr, SEXP r_offset);
SEXP R_ring_buffer_head_offset(SEXP extPtr, SEXP r_offset);
SEXP R_ring_buffer_copy(SEXP srcPtr, SEXP destPtr, SEXP r_n);
SEXP R_ring_buffer_mirror(SEXP srcPtr, SEXP destPtr);
SEXP R_ring_buffer_head_set(SEXP extPtr, SEXP r_data);
SEXP R_ring_buffer_head_data(SEXP extPtr);
SEXP R_ring_buffer_head_advance(SEXP extPtr);
