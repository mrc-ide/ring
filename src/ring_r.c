#include <ring/ring.h>
#include <R.h>
#include <Rinternals.h>
#include "convert.h"

static void ring_buffer_finalize(SEXP extPtr);
ring_buffer* ring_buffer_get(SEXP extPtr, int closed_error);
int scalar_logical(SEXP x);
size_t scalar_size(SEXP x);

SEXP R_ring_buffer_build(ring_buffer *buffer) {
  SEXP extPtr = PROTECT(R_MakeExternalPtr(buffer, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(extPtr, ring_buffer_finalize);
  UNPROTECT(1);
  return extPtr;
}

SEXP R_ring_buffer_create(SEXP r_size, SEXP r_stride) {
  size_t size = (size_t)scalar_size(r_size),
    stride = scalar_size(r_stride);
  if (size == 0) {
    Rf_error("Can't create ring buffer with size 0");
  }
  if (stride == 0) {
    Rf_error("Can't create ring buffer with stride 0");
  }
  return R_ring_buffer_build(ring_buffer_create(size, stride));
}

SEXP R_ring_buffer_clone(SEXP extPtr) {
  ring_buffer *prev = ring_buffer_get(extPtr, 1);
  return R_ring_buffer_build(ring_buffer_clone(prev));
}

SEXP R_ring_buffer_size(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(ring_buffer_size(ring_buffer_get(extPtr, 1),
                                          scalar_logical(bytes)));
}

SEXP R_ring_buffer_stride(SEXP extPtr) {
  return ScalarInteger(ring_buffer_get(extPtr, 1)->stride);
}

SEXP R_ring_buffer_bytes_data(SEXP extPtr) {
  return ScalarInteger(ring_buffer_bytes_data(ring_buffer_get(extPtr, 1)));
}

SEXP R_ring_buffer_full(SEXP extPtr) {
  return ScalarLogical(ring_buffer_full(ring_buffer_get(extPtr, 1)));
}

SEXP R_ring_buffer_empty(SEXP extPtr) {
  return ScalarLogical(ring_buffer_empty(ring_buffer_get(extPtr, 1)));
}

SEXP R_ring_buffer_head_data(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  if (ring_buffer_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), ring_buffer_head(buffer), buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_tail_data(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  if (ring_buffer_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), ring_buffer_tail(buffer), buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_buffer_data(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  size_t len = buffer->bytes_data;
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), ring_buffer_data(buffer), len);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_head_pos(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(ring_buffer_head_pos(ring_buffer_get(extPtr, 1),
                                              scalar_logical(bytes)));
}

SEXP R_ring_buffer_tail_pos(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(ring_buffer_tail_pos(ring_buffer_get(extPtr, 1),
                                              scalar_logical(bytes)));
}

SEXP R_ring_buffer_free(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(ring_buffer_free(ring_buffer_get(extPtr, 1),
                                          scalar_logical(bytes)));
}

SEXP R_ring_buffer_used(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(ring_buffer_used(ring_buffer_get(extPtr, 1),
                                          scalar_logical(bytes)));
}

SEXP R_ring_buffer_reset(SEXP extPtr) {
  ring_buffer_reset(ring_buffer_get(extPtr, 1));
  return R_NilValue;
}

SEXP R_ring_buffer_memset(SEXP extPtr, SEXP c, SEXP len) {
  ring_buffer *buffer = ring_buffer_get(extPtr, 1);
  size_t n = scalar_size(len);
  data_t *data = RAW(c);
  if (length(c) == 1) {
    return ScalarInteger(ring_buffer_memset(buffer, data[0], n));
  } else if (length(c) == buffer->stride) {
    return ScalarInteger(ring_buffer_memset_stride(buffer, data, n));
  } else {
    Rf_error("Invalid length input");
    return R_NilValue;
  }
}

SEXP R_ring_buffer_memcpy_into(SEXP extPtr, SEXP src) {
  ring_buffer *buffer = ring_buffer_get(extPtr, 1);
  size_t len = LENGTH(src), stride = buffer->stride;
  if (len % stride != 0) {
    Rf_error("Incorrect size data (%d bytes); expected multiple of %d bytes",
             len, stride);
  }
  size_t count = len / stride;
  data_t * head = (data_t *) ring_buffer_memcpy_into(buffer, RAW(src), count);
  return ScalarInteger(head - buffer->data);
}

SEXP R_ring_buffer_memcpy_from(SEXP extPtr, SEXP r_count) {
  size_t count = scalar_size(r_count);
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, count * buffer->stride));
  if (ring_buffer_memcpy_from(RAW(ret), buffer, count) == NULL) {
    // TODO: this would be better reporting than just saying "Buffer
    // underflow".  But we need to switch here on stride being 1 or
    // compute the byte size of `count` entries.
    Rf_error("Buffer underflow (requested %d bytes but %d available)",
             count, ring_buffer_used(buffer, 1) / buffer->stride);
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_tail_read(SEXP extPtr, SEXP r_count) {
  size_t count = scalar_size(r_count);
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, count * buffer->stride));
  if (ring_buffer_tail_read(buffer, RAW(ret), count) == NULL) {
    Rf_error("Buffer underflow");
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_tail_offset(SEXP extPtr, SEXP r_offset) {
  size_t offset = scalar_size(r_offset);
  ring_buffer * buffer = ring_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  data_t *data = (data_t*) ring_buffer_tail_offset(buffer, offset);
  if (data == NULL) {
    Rf_error("Buffer underflow");
  }
  memcpy(RAW(ret), data, buffer->stride);
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_copy(SEXP srcPtr, SEXP destPtr, SEXP r_count) {
  size_t count = scalar_size(r_count);
  ring_buffer *src = ring_buffer_get(srcPtr, 1),
    *dest = ring_buffer_get(destPtr, 1);
  data_t * head = (data_t *) ring_buffer_copy(dest, src, count);
  if (head == NULL) {
    // TODO: better reporting here; make this a general thing I think.
    // See memcpy_from and tail_read for other places this is needed.
    Rf_error("Buffer underflow");
  }
  return ScalarInteger(head - dest->data);
}

void ring_buffer_finalize(SEXP extPtr) {
  ring_buffer *buffer = ring_buffer_get(extPtr, 0);
  if (buffer) {
    ring_buffer_destroy(buffer);
    R_ClearExternalPtr(extPtr);
  }
}

// Some utilities
ring_buffer* ring_buffer_get(SEXP extPtr, int closed_error) {
  ring_buffer *buffer = NULL;
  if (TYPEOF(extPtr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  buffer = (ring_buffer*) R_ExternalPtrAddr(extPtr);
  if (!buffer && closed_error) {
    Rf_error("ring_buffer already freed");
  }
  return buffer;
}

int scalar_logical(SEXP x) {
  if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    return INTEGER(x)[0];
  } else {
    Rf_error("Expected a logical scalar");
    return 0;
  }
}

size_t scalar_size(SEXP x) {
  if (TYPEOF(x) == INTSXP && LENGTH(x) == 1){
    int val = INTEGER(x)[0];
    if (val == NA_INTEGER || val < 0) {
      Rf_error("Expected a nonnegative value");
    }
    return INTEGER(x)[0];
  } else if (TYPEOF(x) == REALSXP && LENGTH(x) == 1) {
    double val = REAL(x)[0];
    if (!R_FINITE(val) || val < 0) {
      Rf_error("Expected a nonnegative value");
    }
    return (size_t) val;
  } else {
    Rf_error("Expected a nonnegative scalar integer");
    return 0;
  }
}

// Registration

#include <R_ext/Rdynload.h>
static const R_CallMethodDef callMethods[] = {
  {"Cring_buffer_create",      (DL_FUNC) &R_ring_buffer_create,      2},
  {"Cring_buffer_clone",       (DL_FUNC) &R_ring_buffer_clone,       1},
  {"Cring_buffer_bytes_data",  (DL_FUNC) &R_ring_buffer_bytes_data,  1},
  {"Cring_buffer_size",        (DL_FUNC) &R_ring_buffer_size,        2},
  {"Cring_buffer_stride",      (DL_FUNC) &R_ring_buffer_stride,      1},
  {"Cring_buffer_full",        (DL_FUNC) &R_ring_buffer_full,        1},
  {"Cring_buffer_empty",       (DL_FUNC) &R_ring_buffer_empty,       1},
  {"Cring_buffer_head_data",   (DL_FUNC) &R_ring_buffer_head_data,   1},
  {"Cring_buffer_tail_data",   (DL_FUNC) &R_ring_buffer_tail_data,   1},
  {"Cring_buffer_buffer_data", (DL_FUNC) &R_ring_buffer_buffer_data, 1},
  {"Cring_buffer_head_pos",    (DL_FUNC) &R_ring_buffer_head_pos,    2},
  {"Cring_buffer_tail_pos",    (DL_FUNC) &R_ring_buffer_tail_pos,    2},
  {"Cring_buffer_free",        (DL_FUNC) &R_ring_buffer_free,        2},
  {"Cring_buffer_used",        (DL_FUNC) &R_ring_buffer_used,        2},
  {"Cring_buffer_reset",       (DL_FUNC) &R_ring_buffer_reset,       1},
  {"Cring_buffer_memset",      (DL_FUNC) &R_ring_buffer_memset,      3},
  {"Cring_buffer_memcpy_into", (DL_FUNC) &R_ring_buffer_memcpy_into, 2},
  {"Cring_buffer_memcpy_from", (DL_FUNC) &R_ring_buffer_memcpy_from, 2},
  {"Cring_buffer_tail_read",   (DL_FUNC) &R_ring_buffer_tail_read,   2},
  {"Cring_buffer_copy",        (DL_FUNC) &R_ring_buffer_copy,        3},
  {"Cring_buffer_tail_offset", (DL_FUNC) &R_ring_buffer_tail_offset, 2},
  // conversion code
  {"Cint_to_bytes",              (DL_FUNC) &int_to_bytes,                1},
  {"Cbytes_to_int",              (DL_FUNC) &bytes_to_int,                1},
  {"Cdouble_to_bytes",           (DL_FUNC) &double_to_bytes,             1},
  {"Cbytes_to_double",           (DL_FUNC) &bytes_to_double,             1},
  {"Ccomplex_to_bytes",          (DL_FUNC) &complex_to_bytes,            1},
  {"Cbytes_to_complex",          (DL_FUNC) &bytes_to_complex,            1},
  {NULL,                         NULL,                                   0}
};

void R_init_ring(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
