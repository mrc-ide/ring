#include <ring/ring.h>
#include <R.h>
#include <Rinternals.h>

// NOTE: Duplicated from ring_r.c
ring_buffer* ring_buffer_get(SEXP extPtr, int closed_error);

size_t ring_buffer_compute_offset(ring_buffer* buffer, data_t *x) {
  int ret = -1;
  if (x != NULL) {
    ret = (x - buffer->data) / buffer->stride;
    // Correct for position of the tail, so that this is a tail
    // offset.  Note that this just back computes the tail offset from
    // before.  It might be worth rolling this into the main ring
    // library.
    size_t tail_pos = ring_buffer_tail_pos(buffer, 0);
    if (ret >= (int)tail_pos) {
      ret -= tail_pos;
    } else {
      ret += (ring_buffer_size(buffer, 0) + 1 - tail_pos);
    }
  }
  return ret;
}

// Need to be really explicit here about what we are looking for.  The
// main use case is to imagine that the buffer has a series of
// _increasing_ values in it.  We're imagining starting looking at the
// tail (the smallest number) and we're looking for the last entry
// *smaller* than the number.  That will leave the number we're
// looking for bracketed by the returned data point and a data point
// one advanced (towards the head).
//
// So a positive value here means *keep looking upwards*.  That is
// going to be when the data value is smaller than the target value.
// Off-by-one issues here are tricky though; how we should behave when
// the values are identical is not obvious and may require some
// tweaking.
bool test_find_double(const void *x, void *data) {
  double x_value = *((double*) x);
  double data_value = *((double*) data);
  return x_value <= data_value;
}

SEXP test_search_linear(SEXP r_buffer, SEXP r_value) {
  ring_buffer *buffer = ring_buffer_get(r_buffer, 1);
  double value = REAL(r_value)[0];
  data_t *x = (data_t*)
    ring_buffer_search_linear(buffer, test_find_double, &value);
  return ScalarInteger(ring_buffer_compute_offset(buffer, x));
}

SEXP test_search_bisect(SEXP r_buffer, SEXP r_value, SEXP r_i) {
  ring_buffer *buffer = ring_buffer_get(r_buffer, 1);
  double value = REAL(r_value)[0];
  int i = INTEGER(r_i)[0];
  data_t *x = (data_t*)
    ring_buffer_search_bisect(buffer, i, test_find_double, &value);
  return ScalarInteger(ring_buffer_compute_offset(buffer, x));
}
