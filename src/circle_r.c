#include <circle/circle.h>
#include <R.h>
#include <Rinternals.h>

static void circle_buffer_finalize(SEXP extPtr);
circle_buffer* circle_buffer_get(SEXP extPtr, int closed_error);
int logical_scalar(SEXP x);

SEXP R_circle_buffer_build(circle_buffer *buffer) {
  SEXP extPtr = PROTECT(R_MakeExternalPtr(buffer, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(extPtr, circle_buffer_finalize);
  SEXP ret = PROTECT(allocVector(VECSXP, 3));
  SEXP nms = PROTECT(allocVector(STRSXP, 3));
  SET_VECTOR_ELT(ret, 0, extPtr);
  SET_STRING_ELT(nms, 0, mkChar("ptr"));
  SET_VECTOR_ELT(ret, 1, ScalarInteger(circle_buffer_size(buffer, 0)));
  SET_STRING_ELT(nms, 1, mkChar("size"));
  SET_VECTOR_ELT(ret, 2, ScalarInteger(buffer->stride));
  SET_STRING_ELT(nms, 2, mkChar("stride"));
  setAttrib(ret, R_NamesSymbol, nms);
  setAttrib(ret, R_ClassSymbol, mkString("circle_buffer"));
  UNPROTECT(3);
  return ret;
}

SEXP R_circle_buffer_create(SEXP r_size, SEXP r_stride) {
  size_t size = (size_t)INTEGER(r_size)[0], stride = INTEGER(r_stride)[0];
  return R_circle_buffer_build(circle_buffer_create(size, stride));
}

SEXP R_circle_buffer_clone(SEXP extPtr) {
  circle_buffer *prev = circle_buffer_get(extPtr, 1);
  return R_circle_buffer_build(circle_buffer_clone(prev));
}

SEXP R_circle_buffer_size(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(circle_buffer_size(circle_buffer_get(extPtr, 1),
                                          logical_scalar(bytes)));
}

SEXP R_circle_buffer_bytes_data(SEXP extPtr) {
  return ScalarInteger(circle_buffer_bytes_data(circle_buffer_get(extPtr, 1)));
}

SEXP R_circle_buffer_full(SEXP extPtr) {
  return ScalarLogical(circle_buffer_full(circle_buffer_get(extPtr, 1)));
}

SEXP R_circle_buffer_empty(SEXP extPtr) {
  return ScalarLogical(circle_buffer_empty(circle_buffer_get(extPtr, 1)));
}

SEXP R_circle_buffer_head(SEXP extPtr) {
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  if (circle_buffer_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), circle_buffer_head(buffer), buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_circle_buffer_tail(SEXP extPtr) {
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  if (circle_buffer_empty(buffer)) {
    Rf_error("Buffer is empty");
  }
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  memcpy(RAW(ret), circle_buffer_tail(buffer), buffer->stride);
  UNPROTECT(1);
  return ret;
}

SEXP R_circle_buffer_data(SEXP extPtr) {
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  size_t len = circle_buffer_size(buffer, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, len));
  memcpy(RAW(ret), circle_buffer_data(buffer), len);
  UNPROTECT(1);
  return ret;
}

SEXP R_circle_buffer_head_pos(SEXP extPtr) {
  return ScalarInteger(circle_buffer_head_pos(circle_buffer_get(extPtr, 1)));
}

SEXP R_circle_buffer_tail_pos(SEXP extPtr) {
  return ScalarInteger(circle_buffer_tail_pos(circle_buffer_get(extPtr, 1)));
}

SEXP R_circle_buffer_free(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(circle_buffer_free(circle_buffer_get(extPtr, 1),
                                          logical_scalar(bytes)));
}

SEXP R_circle_buffer_used(SEXP extPtr, SEXP bytes) {
  return ScalarInteger(circle_buffer_used(circle_buffer_get(extPtr, 1),
                                          logical_scalar(bytes)));
}

SEXP R_circle_buffer_reset(SEXP extPtr) {
  circle_buffer_reset(circle_buffer_get(extPtr, 1));
  return R_NilValue;
}

SEXP R_circle_buffer_memset(SEXP extPtr, SEXP c, SEXP len) {
  return ScalarInteger(circle_buffer_memset(circle_buffer_get(extPtr, 1),
                                            RAW(c)[0], INTEGER(len)[0]));
}

SEXP R_circle_buffer_memcpy_into(SEXP extPtr, SEXP src) {
  circle_buffer *buffer = circle_buffer_get(extPtr, 1);
  // TODO: warn about lack of stride fit.
  size_t count = LENGTH(src) / buffer->stride;
  data_t * head = (data_t *) circle_buffer_memcpy_into(buffer, RAW(src), count);
  return ScalarInteger(head - buffer->data);
}

SEXP R_circle_buffer_memcpy_from(SEXP extPtr, SEXP r_count) {
  size_t count = INTEGER(r_count)[0];
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, count * buffer->stride));
  if (circle_buffer_memcpy_from(RAW(ret), buffer, count) == NULL) {
    // TODO: this would be better reporting than just saying "Buffer
    // underflow".  But we need to switch here on stride being 1 or
    // compute the byte size of `count` entries.
    Rf_error("Buffer underflow (requested %d bytes but %d available)",
             count, circle_buffer_used(buffer, 1) / buffer->stride);
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_circle_buffer_tail_read(SEXP extPtr, SEXP r_count) {
  size_t count = INTEGER(r_count)[0];
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, count * buffer->stride));
  if (circle_buffer_tail_read(buffer, RAW(ret), count) == NULL) {
    Rf_error("Buffer underflow");
  }
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_circle_buffer_tail_offset(SEXP extPtr, SEXP r_offset) {
  size_t offset = INTEGER(r_offset)[0];
  circle_buffer * buffer = circle_buffer_get(extPtr, 1);
  SEXP ret = PROTECT(allocVector(RAWSXP, buffer->stride));
  data_t *data = (data_t*) circle_buffer_tail_offset(buffer, offset);
  if (data == NULL) {
    Rf_error("Buffer underflow");
  }
  memcpy(RAW(ret), data, buffer->stride);
  UNPROTECT(1);
  // NOTE: In C we return the tail position here but that is not done
  // for the R version.
  return ret;
}

SEXP R_circle_buffer_copy(SEXP srcPtr, SEXP destPtr, SEXP r_count) {
  size_t count = INTEGER(r_count)[0];
  circle_buffer *src = circle_buffer_get(srcPtr, 1),
    *dest = circle_buffer_get(destPtr, 1);
  data_t * head = (data_t *) circle_buffer_copy(dest, src, count);
  if (head == NULL) {
    // TODO: better reporting here; make this a general thing I think.
    // See memcpy_from and tail_read for other places this is needed.
    Rf_error("Buffer underflow");
  }
  return ScalarInteger(head - dest->data);
}

void circle_buffer_finalize(SEXP extPtr) {
  circle_buffer *buffer = circle_buffer_get(extPtr, 0);
  if (buffer) {
    circle_buffer_destroy(buffer);
    R_ClearExternalPtr(extPtr);
  }
}

// Some utilities
circle_buffer* circle_buffer_get(SEXP extPtr, int closed_error) {
  circle_buffer *buffer = NULL;
  if (TYPEOF(extPtr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  buffer = (circle_buffer*) R_ExternalPtrAddr(extPtr);
  if (!buffer && closed_error) {
    Rf_error("circle_buffer already freed");
  }
  return buffer;
}

int logical_scalar(SEXP x) {
  if (TYPEOF(x) == LGLSXP && LENGTH(x) == 1) {
    return INTEGER(x)[0];
  } else {
    Rf_error("Expected a logical scalar");
    return 0;
  }
}

// Registration

#include <R_ext/Rdynload.h>
static const R_CallMethodDef callMethods[] = {
  // circle_r
  {"Ccircle_buffer_create",      (DL_FUNC) &R_circle_buffer_create,      2},
  {"Ccircle_buffer_clone",       (DL_FUNC) &R_circle_buffer_clone,       1},
  {"Ccircle_buffer_bytes_data",  (DL_FUNC) &R_circle_buffer_bytes_data,  1},
  {"Ccircle_buffer_size",        (DL_FUNC) &R_circle_buffer_size,        2},
  {"Ccircle_buffer_full",        (DL_FUNC) &R_circle_buffer_full,        1},
  {"Ccircle_buffer_empty",       (DL_FUNC) &R_circle_buffer_empty,       1},
  {"Ccircle_buffer_head",        (DL_FUNC) &R_circle_buffer_head,        1},
  {"Ccircle_buffer_tail",        (DL_FUNC) &R_circle_buffer_tail,        1},
  {"Ccircle_buffer_data",        (DL_FUNC) &R_circle_buffer_data,        1},
  {"Ccircle_buffer_head_pos",    (DL_FUNC) &R_circle_buffer_head_pos,    1},
  {"Ccircle_buffer_tail_pos",    (DL_FUNC) &R_circle_buffer_tail_pos,    1},
  {"Ccircle_buffer_free",        (DL_FUNC) &R_circle_buffer_free,        2},
  {"Ccircle_buffer_used",        (DL_FUNC) &R_circle_buffer_used,        2},
  {"Ccircle_buffer_reset",       (DL_FUNC) &R_circle_buffer_reset,       1},
  {"Ccircle_buffer_memset",      (DL_FUNC) &R_circle_buffer_memset,      3},
  {"Ccircle_buffer_memcpy_into", (DL_FUNC) &R_circle_buffer_memcpy_into, 2},
  {"Ccircle_buffer_memcpy_from", (DL_FUNC) &R_circle_buffer_memcpy_from, 2},
  {"Ccircle_buffer_tail_read",   (DL_FUNC) &R_circle_buffer_tail_read,   2},
  {"Ccircle_buffer_copy",        (DL_FUNC) &R_circle_buffer_copy,        3},
  {"Ccircle_buffer_tail_offset", (DL_FUNC) &R_circle_buffer_tail_offset, 2},
  {NULL,                         NULL,                                   0}
};

void R_init_circle(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
