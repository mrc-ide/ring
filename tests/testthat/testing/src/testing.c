#include <ring/ring.c>
#include <stdbool.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

SEXP test(SEXP r_n, SEXP r_p) {
  int n = INTEGER(r_n)[0];
  double p = REAL(r_p)[0];

  // Stride must be given here.
  ring_buffer * r = ring_buffer_create(n, sizeof(double), OVERFLOW_OVERWRITE);

  // Generate a bunch of random numbers until we hit the cut off 'p'
  GetRNGstate();
  while (true) {
    R_CheckUserInterrupt();
    double x = unif_rand();
    ring_buffer_push(r, &x, 1);
    if (x < p) {
      break;
    }
  }
  PutRNGstate();

  // Then we'll copy these bits back to R
  size_t used = ring_buffer_used(r, 0);
  SEXP ret = PROTECT(allocVector(REALSXP, used));
  ring_buffer_take(r, REAL(ret), used);

  // destroy the buffer on exit (note: if anything above throws, this
  // will leak).
  ring_buffer_destroy(r);
  UNPROTECT(1);
  return ret;
}
