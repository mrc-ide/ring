#include "ring_r.h"

// Internal prototypes:
SEXP R_ring_buffer_alloc(ring_buffer *buffer);
static void ring_buffer_finalize(SEXP extPtr);
ring_buffer* ring_buffer_get(SEXP extPtr, bool closed_error);
bool scalar_logical(SEXP x);
size_t scalar_size(SEXP x);
SEXP scalar_size_sexp(size_t x);
void throw_underflow(ring_buffer *buffer, size_t n);
const data_t * get_raw(SEXP data);

// Definitions:
SEXP R_ring_buffer_create(SEXP r_size, SEXP r_stride, SEXP r_on_overflow) {
  size_t size = (size_t)scalar_size(r_size),
    stride = scalar_size(r_stride);
  if (size == 0) {
    Rf_error("Can't create ring buffer with size 0");
  }
  if (stride == 0) {
    Rf_error("Can't create ring buffer with stride 0");
  }
  overflow_action on_overflow = scalar_size(r_on_overflow);
  return R_ring_buffer_alloc(ring_buffer_create(size, stride, on_overflow));
}

SEXP R_ring_buffer_duplicate(SEXP extPtr) {
  ring_buffer *prev = ring_buffer_get(extPtr, true);
  return R_ring_buffer_alloc(ring_buffer_duplicate(prev));
}

SEXP R_ring_buffer_grow(SEXP extPtr, SEXP r_n, SEXP r_exact) {
  ring_buffer_grow(ring_buffer_get(extPtr, true),
                   scalar_size(r_n), scalar_logical(r_exact));
  return R_NilValue;
}

SEXP R_ring_buffer_size(SEXP extPtr, SEXP bytes) {
  return scalar_size_sexp(ring_buffer_size(ring_buffer_get(extPtr, true),
                                           scalar_logical(bytes)));
}

SEXP R_ring_buffer_stride(SEXP extPtr) {
  return scalar_size_sexp(ring_buffer_get(extPtr, true)->stride);
}

SEXP R_ring_buffer_bytes_data(SEXP extPtr) {
  size_t ret = ring_buffer_bytes_data(ring_buffer_get(extPtr, true));
  return scalar_size_sexp(ret);
}

SEXP R_ring_buffer_is_full(SEXP extPtr) {
  return ScalarLogical(ring_buffer_is_full(ring_buffer_get(extPtr, true)));
}

SEXP R_ring_buffer_is_empty(SEXP extPtr) {
  return ScalarLogical(ring_buffer_is_empty(ring_buffer_get(extPtr, true)));
}

// NOTE: this is slightly different behaviour than the C API because
// it is not useful to return the memory location; instead we return
// the head contents.
SEXP R_ring_buffer_head(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  if (ring_buffer_is_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  data_t *data = (data_t*) ring_buffer_head_offset(buffer, 0);
  memcpy(RAW(ret), data, buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_tail(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  if (ring_buffer_is_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), ring_buffer_tail(buffer), buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_data(SEXP extPtr) {
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  size_t len = buffer->bytes_data;
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), ring_buffer_data(buffer), len);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_head_pos(SEXP extPtr, SEXP bytes) {
  return scalar_size_sexp(ring_buffer_head_pos(ring_buffer_get(extPtr, true),
                                            scalar_logical(bytes)));
}

SEXP R_ring_buffer_tail_pos(SEXP extPtr, SEXP bytes) {
  return scalar_size_sexp(ring_buffer_tail_pos(ring_buffer_get(extPtr, true),
                                            scalar_logical(bytes)));
}

SEXP R_ring_buffer_free(SEXP extPtr, SEXP bytes) {
  return scalar_size_sexp(ring_buffer_free(ring_buffer_get(extPtr, true),
                                          scalar_logical(bytes)));
}

SEXP R_ring_buffer_used(SEXP extPtr, SEXP bytes) {
  return scalar_size_sexp(ring_buffer_used(ring_buffer_get(extPtr, true),
                                          scalar_logical(bytes)));
}

SEXP R_ring_buffer_reset(SEXP extPtr, SEXP clear) {
  ring_buffer_reset(ring_buffer_get(extPtr, true), scalar_logical(clear));
  return R_NilValue;
}

SEXP R_ring_buffer_set(SEXP extPtr, SEXP r_data, SEXP r_n) {
  ring_buffer *buffer = ring_buffer_get(extPtr, true);
  const size_t n = scalar_size(r_n), n_data = length(r_data);
  const data_t *data = get_raw(r_data);
  if (n_data == 1) {
    size_t ret = ring_buffer_set(buffer, data[0], n) / buffer->stride;
    return scalar_size_sexp(ret);
  } else if (n_data == buffer->stride) {
    return scalar_size_sexp(ring_buffer_set_stride(buffer, data, n));
  } else {
    Rf_error("Invalid length data");
    return R_NilValue;
  }
}

SEXP R_ring_buffer_push(SEXP extPtr, SEXP r_data) {
  ring_buffer *buffer = ring_buffer_get(extPtr, true);
  size_t len = LENGTH(r_data), stride = buffer->stride;
  if (len % stride != 0) {
    Rf_error("Incorrect size data (%d bytes); expected multiple of %d bytes",
             len, stride);
  }
  size_t n = len / stride;
  const data_t *data = get_raw(r_data);
  data_t *head = (data_t *) ring_buffer_push(buffer, data, n);
  return scalar_size_sexp(head - buffer->data);
}

SEXP R_ring_buffer_take(SEXP extPtr, SEXP r_n) {
  size_t n = scalar_size(r_n);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, n * buffer->stride));
  if (ring_buffer_take(buffer, RAW(ret), n) == NULL) {
    throw_underflow(buffer, n);
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_read(SEXP extPtr, SEXP r_n) {
  size_t n = scalar_size(r_n);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, n * buffer->stride));
  if (ring_buffer_read(buffer, RAW(ret), n) == NULL) {
    throw_underflow(buffer, n);
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_take_head(SEXP extPtr, SEXP r_n) {
  size_t n = scalar_size(r_n);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, n * buffer->stride));
  if (ring_buffer_take_head(buffer, RAW(ret), n) == NULL) {
    throw_underflow(buffer, n);
  }
  UNPROTECT(1);
  // NOTE: In C we return the head position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_read_head(SEXP extPtr, SEXP r_n) {
  size_t n = scalar_size(r_n);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, n * buffer->stride));
  if (ring_buffer_read_head(buffer, RAW(ret), n) == NULL) {
    throw_underflow(buffer, n);
  }
  UNPROTECT(1);
  // NOTE: In C we return the head position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_ring_buffer_tail_offset(SEXP extPtr, SEXP r_offset) {
  size_t offset = scalar_size(r_offset);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  data_t *data = (data_t*) ring_buffer_tail_offset(buffer, offset);
  if (data == NULL) {
    throw_underflow(buffer, offset);
  }
  memcpy(RAW(ret), data, buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_head_offset(SEXP extPtr, SEXP r_offset) {
  size_t offset = scalar_size(r_offset);
  ring_buffer * buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  data_t *data = (data_t*) ring_buffer_head_offset(buffer, offset);
  if (data == NULL) {
    throw_underflow(buffer, offset);
  }
  memcpy(RAW(ret), data, buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_copy(SEXP srcPtr, SEXP destPtr, SEXP r_n) {
  size_t n = scalar_size(r_n);
  ring_buffer *src = ring_buffer_get(srcPtr, true),
    *dest = ring_buffer_get(destPtr, true);
  data_t * head = (data_t *) ring_buffer_copy(src, dest, n);
  if (head == NULL) {
    if (src == dest) {
      Rf_error("Can't copy a buffer into itself");
    } else if (src->stride != dest->stride) {
      Rf_error("Can't copy as buffers differ in their stride (%d vs %d)",
               src->stride, dest->stride);
    } else {
      throw_underflow(src, n);
    }
  } // #nocov
  return scalar_size_sexp(head - dest->data);
}

SEXP R_ring_buffer_mirror(SEXP srcPtr, SEXP destPtr) {
  ring_buffer *src = ring_buffer_get(srcPtr, true),
    *dest = ring_buffer_get(destPtr, true);
  bool ok = ring_buffer_mirror(src, dest);
  if (!ok) {
    if (src == dest) {
      Rf_error("Can't mirror a buffer into itself");
    } else if (src->stride != dest->stride) {
      Rf_error("Can't mirror as buffers differ in their stride (%d vs %d)",
               src->stride, dest->stride);
    } else if (src->size != dest->size) {
      Rf_error("Can't mirror as buffers differ in their size (%d vs %d)",
               src->size, dest->size);
    } else {
      Rf_error("Unknown error [ring bug]"); // #nocov
    }
  }
  return R_NilValue;
}

SEXP R_ring_buffer_head_set(SEXP extPtr, SEXP r_data) {
  ring_buffer *buffer = ring_buffer_get(extPtr, true);
  const size_t len = LENGTH(r_data), stride = buffer->stride;
  if (len != stride) {
    Rf_error("Incorrect size data (%d bytes); expected exactly %d bytes",
             len, stride);
  }
  const data_t *data = get_raw(r_data);
  memcpy(buffer->head, data, stride);
  return R_NilValue;
}

SEXP R_ring_buffer_head_data(SEXP extPtr) {
  ring_buffer *buffer = ring_buffer_get(extPtr, true);
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), buffer->head, buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_ring_buffer_head_advance(SEXP extPtr) {
  ring_buffer *buffer = ring_buffer_get(extPtr, true);
  ring_buffer_head_advance(buffer);
  return R_NilValue;
}

// Allocation and finalisation:
SEXP R_ring_buffer_alloc(ring_buffer *buffer) {
  SEXP extPtr = PROTECT(R_MakeExternalPtr(buffer, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(extPtr, ring_buffer_finalize);
  UNPROTECT(1);
  return extPtr;
}

void ring_buffer_finalize(SEXP extPtr) {
  ring_buffer *buffer = ring_buffer_get(extPtr, false);
  if (buffer) {
    ring_buffer_destroy(buffer);
    R_ClearExternalPtr(extPtr);
  }
}

// Some utilities:
ring_buffer* ring_buffer_get(SEXP extPtr, bool closed_error) {
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

bool scalar_logical(SEXP x) {
  if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    int ret = INTEGER(x)[0];
    if (ret == NA_LOGICAL) {
      Rf_error("Expected a non-missing logical scalar");
    }
    return (bool)(ret);
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
    if (val - (size_t)val > sqrt(DBL_EPSILON)) {
      Rf_error("Expected an integer value");
    }
    return (size_t) val;
  } else {
    Rf_error("Expected a nonnegative scalar integer");
    return 0;
  }
}

SEXP scalar_size_sexp(size_t x) {
  return x > INT_MAX ? ScalarReal(x) : ScalarInteger(x);
}

void throw_underflow(ring_buffer *buffer, size_t n) {
  Rf_error("Buffer underflow (requested %d elements but %d available)",
           n, ring_buffer_used(buffer, false));
} // #nocov

const data_t * get_raw(SEXP data) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("Expected a raw vector 'data'");
  }
  return RAW(data);
}
