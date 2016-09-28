#include <ring/ring.h>

#include <R.h>
#include <Rinternals.h>

// Definition used below (can't be called step() because that may
// conflict with regexp.h on some platforms).
int step_x(ring_buffer *r, int x);

void example(size_t nstep, double *ret) {
  // Construct a ring buffer of (max) size 5, each element of which is
  // big enough to contain an integer (probably 4 bytes).
  ring_buffer *r = ring_buffer_create(5, sizeof(int), OVERFLOW_OVERWRITE);

  // Starting point of the simulation, as in the R version:
  int x = 0;

  // Push the initial state into the ring buffer:
  ring_buffer_push(r, &x, 1);
  ret[0] = x;

  for (size_t i = 1; i < nstep; ++i) {
    x = step_x(r, x);
    ring_buffer_push(r, &x, 1);
    ret[i] = x;
  }

  // Cleanup:
  ring_buffer_destroy(r);
}

int step_x(ring_buffer *r, int x) {
  size_t n = ring_buffer_used(r, false);
  double p;
  if (n < 2) {
    p = 0.5;
  } else {
    // Oldest non-overflowed element is in the tail.  Note that the
    // return value (which is void*) must be first cast to (int*) then
    // dereferenced.
    ///
    // NOTE: In general, check that the return value here is not NULL
    // (indicating an underflow); here we're OK because we checked the
    // number of used elements at the beginning.
    int x0 = *(int*)ring_buffer_tail(r);
    size_t increases = 0;
    for (size_t i = 1; i < n; ++i) {
      // Moving through the more recently added elements:
      int x1 = *(int*)ring_buffer_tail_offset(r, i);
      if (x1 > x0) {
        increases++;
      }
      x0 = x1;
    }
    p = ((double)increases) / (n - 1);
  }

  if (unif_rand() < p) {
    --x;
  } else {
    ++x;
  }

  return x;
}

// This function collects all the R API bits that deal with
// communication between C and R.  "Writing R extensions" is the
// canonical documentation source.
SEXP r_example(SEXP r_nstep) {
  size_t nstep = (size_t) INTEGER(r_nstep)[0];
  SEXP ret = PROTECT(allocVector(REALSXP, nstep));
  GetRNGstate(); // because we'll work with random numbers
  example(nstep, REAL(ret));
  // Cleanup:
  PutRNGstate();
  UNPROTECT(1);
  return ret;
}

// This can be included in a different file, or, for a single file
// project like this one, include here.
#include <ring/ring.c>
