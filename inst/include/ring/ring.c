#ifndef _RING_H_
#include <ring/ring.h>
#endif

// Some prototypes used here that aren't public:
bool ring_buffer_handle_overflow(ring_buffer *buffer, size_t n);
const data_t * ring_buffer_end(const ring_buffer *buffer);
data_t * ring_buffer_nextp(ring_buffer *buffer, const data_t *p);
int imin(int a, int b);

#ifdef RING_USE_STDLIB_ALLOC
#include <stdlib.h>
#include <string.h>
#include <math.h>
#else
#include <R.h>
#endif

ring_buffer * ring_buffer_create(size_t size, size_t stride,
                                 overflow_action on_overflow) {
  const size_t bytes_data = (size + 1) * stride;
#ifdef RING_USE_STDLIB_ALLOC
  ring_buffer * buffer = (ring_buffer*) calloc(1, sizeof(ring_buffer));
  if (buffer == NULL) {
    return NULL;
  }
  buffer->data = (data_t*) calloc(bytes_data, 1);
  if (buffer->data == NULL) {
    free(buffer);
    return NULL;
  }
#else
  ring_buffer * buffer = (ring_buffer*) Calloc(1, ring_buffer);
  buffer->data = (data_t*) Calloc(bytes_data, data_t);
#endif
  buffer->size = size;
  buffer->stride = stride;
  buffer->bytes_data = bytes_data;
  buffer->on_overflow = on_overflow;
  ring_buffer_reset(buffer, false);
  return buffer;
}

void ring_buffer_destroy(ring_buffer *buffer) {
#ifdef RING_USE_STDLIB_ALLOC
  free(buffer->data);
  free(buffer);
#else
  Free(buffer->data);
  Free(buffer);
#endif
}

ring_buffer * ring_buffer_duplicate(const ring_buffer *buffer) {
  ring_buffer *ret = ring_buffer_create(buffer->size, buffer->stride,
                                        buffer->on_overflow);
#ifdef RING_USE_STDLIB_ALLOC
  if (ret == NULL) {
    return NULL;
  }
#endif
  ring_buffer_mirror(buffer, ret);
  return ret;
}

#define LOG_PHI 0.481211825028767
void ring_buffer_grow(ring_buffer *buffer, size_t n, bool exact) {
  if (n == 0) {
    return;
  }
  const size_t
    curr_size = ring_buffer_size(buffer, false),
    head_pos = ring_buffer_head_pos(buffer, true),
    tail_pos = ring_buffer_tail_pos(buffer, true);

  size_t size;
  if (exact) {
    size = curr_size + n;
  } else {
    const size_t curr_used = ring_buffer_used(buffer, false);
    const double r = (double) (curr_used + n) / (double) curr_size;
    if (r <= 1) {
      // Refuse to shrink the buffer:
      return;
    }
    size = ceil(curr_size * exp(ceil(log(r) / LOG_PHI) * LOG_PHI));
  }
  const size_t bytes_data = (size + 1) * buffer->stride;

#ifdef RING_USE_STDLIB_ALLOC
  void *tmp = realloc(buffer->data, bytes_data);
  if (tmp != NULL) {
    buffer->data = (data_t*) tmp;
  } else {
    // Out of memory.
#ifdef USING_R
    // Probably the most polite thing to do; could cause a memory leak
    // or leave things in a fairly inconsistent state.
    Rf_error("Ran out of memory while resizing ring buffer");
#else
    // This should cause a crash pretty quickly
    buffer->data = NULL;
#endif
  }
#else
  // R will handle the crash here for us:
  buffer->data = (data_t*) Realloc(buffer->data, bytes_data, data_t);
#endif
  // Ensure that all newly allocated data is zeroed
  const size_t len = (curr_size + 1) * buffer->stride;
  memset(buffer->data + len, 0, bytes_data - len);
  // And then correctly reset all the pointers
  buffer->head = buffer->data + head_pos;
  buffer->tail = buffer->data + tail_pos;
  buffer->size = size;
  buffer->bytes_data = bytes_data;
}

// Below here, nothing else should vary on RING_USE_STDLIB_ALLOC,
// though there is one dependency on USING_R

void ring_buffer_reset(ring_buffer *buffer, bool clear) {
  buffer->head = buffer->tail = buffer->data;
  if (clear) {
    memset(buffer->data, 0, buffer->bytes_data);
  }
}

size_t ring_buffer_size(const ring_buffer *buffer, bool bytes) {
  return bytes ? buffer->bytes_data - buffer->stride : buffer->size;
}

size_t ring_buffer_free(const ring_buffer *buffer, bool bytes) {
  size_t diff;
  if (buffer->head >= buffer->tail) {
    diff = ring_buffer_size(buffer, true) - (buffer->head - buffer->tail);
  } else {
    diff = buffer->tail - buffer->head - buffer->stride;
  }
  return bytes ? diff : diff / buffer->stride;
}

size_t ring_buffer_used(const ring_buffer *buffer, bool bytes) {
  return ring_buffer_size(buffer, bytes) - ring_buffer_free(buffer, bytes);
}

size_t ring_buffer_bytes_data(const ring_buffer *buffer) {
  return buffer->bytes_data;
}

bool ring_buffer_is_full(const ring_buffer *buffer) {
  return ring_buffer_free(buffer, true) == 0;
}

bool ring_buffer_is_empty(const ring_buffer *buffer) {
  return ring_buffer_free(buffer, true) == ring_buffer_size(buffer, true);
}

size_t ring_buffer_head_pos(const ring_buffer *buffer, bool bytes) {
  const size_t diff = buffer->head - buffer->data;
  return bytes ? diff : diff / buffer->stride;
}
size_t ring_buffer_tail_pos(const ring_buffer *buffer, bool bytes) {
  const size_t diff = buffer->tail - buffer->data;
  return bytes ? diff : diff / buffer->stride;
}

const void * ring_buffer_head(const ring_buffer *buffer) {
  return buffer->head;
}
const void * ring_buffer_tail(const ring_buffer *buffer) {
  return buffer->tail;
}
const void * ring_buffer_data(const ring_buffer *buffer) {
  return buffer->data;
}

size_t ring_buffer_set(ring_buffer *buffer, data_t c, size_t n) {
  if (buffer->on_overflow == OVERFLOW_OVERWRITE) {
    n = imin(n, ring_buffer_size(buffer, false) + 1);
  }
  const bool overflow = ring_buffer_handle_overflow(buffer, n);
  size_t nwritten = 0, nbytes = n * buffer->stride;
  const data_t *bufend = ring_buffer_end(buffer);

  while (nwritten != nbytes) {
    // don't copy beyond the end of the buffer
    const size_t n = imin(bufend - buffer->head, nbytes - nwritten);
    memset(buffer->head, c, n);
    buffer->head += n;
    nwritten += n;

    // wrap?
    if (buffer->head == bufend) {
      buffer->head = buffer->data;
    }
  }

  if (overflow) {
    buffer->tail = ring_buffer_nextp(buffer, buffer->head);
  }

  return nwritten;
}

// A downside of the current approach here is that we will go through
// the overflow check function n times.
size_t ring_buffer_set_stride(ring_buffer *buffer, const void *x, size_t n) {
  if (buffer->on_overflow == OVERFLOW_OVERWRITE) {
    n = imin(n, ring_buffer_size(buffer, false));
  } else {
    ring_buffer_handle_overflow(buffer, n);
  }
  for (size_t i = 0; i < n; ++i) {
    ring_buffer_push(buffer, x, 1);
  }
  return n;
}

const void * ring_buffer_push(ring_buffer *buffer, const void *src, size_t n) {
  const size_t overflow = ring_buffer_handle_overflow(buffer, n);
  const size_t nbytes = n * buffer->stride;
  const data_t *source = (const data_t*)src;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nread = 0;
  while (nread != nbytes) {
    size_t n = imin(bufend - buffer->head, nbytes - nread);
    memcpy(buffer->head, source + nread, n);
    buffer->head += n;
    nread += n;

    if (buffer->head == bufend) {
      buffer->head = buffer->data;
    }
  }

  if (overflow) {
    buffer->tail = ring_buffer_nextp(buffer, buffer->head);
  }
  return buffer->head;
}

const void * ring_buffer_take(ring_buffer *buffer, void *dest, size_t n) {
  const void * tail = ring_buffer_read(buffer, dest, n);
  if (tail != 0) {
    buffer->tail = buffer->data + ((data_t*)tail - buffer->data);
  }
  return tail;
}

const void * ring_buffer_read(const ring_buffer *buffer, void *dest, size_t n) {
  size_t bytes_used = ring_buffer_used(buffer, true);
  size_t nbytes = n * buffer->stride;
  if (nbytes > bytes_used) {
    return NULL;
  }
  const data_t *tail = buffer->tail;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nwritten = 0;
  while (nwritten != nbytes) {
    size_t n = imin(bufend - tail, nbytes - nwritten);
    memcpy((data_t*)dest + nwritten, tail, n);
    tail += n;
    nwritten += n;
    if (tail == bufend) {
      tail = buffer->data;
    }
  }
  return tail;
}

const void * ring_buffer_take_head(ring_buffer *buffer, void *dest, size_t n) {
  const void * head = ring_buffer_read_head(buffer, dest, n);
  if (head != 0) {
    buffer->head = buffer->data + ((data_t*)head - buffer->data);
  }
  return head;
}

const void * ring_buffer_read_head(const ring_buffer *buffer, void *dest,
                                   size_t n) {
  const size_t bytes_used = ring_buffer_used(buffer, true);
  const size_t nbytes = n * buffer->stride;
  if (nbytes > bytes_used) {
    return NULL;
  }
  const data_t *head = buffer->head;
  const data_t *bufend = ring_buffer_end(buffer);
  data_t *dest_data = (data_t*) dest; // cast so pointer arithmetic works

  for (size_t nwritten = 0; nwritten < n; ++nwritten) {
    if (head == buffer->data) {
      head = bufend;
    }
    head -= buffer->stride;
    memcpy((void*)dest_data, head, buffer->stride);
    dest_data += buffer->stride;
  }

  return head;
}

const void * ring_buffer_copy(ring_buffer *src, ring_buffer *dest, size_t n) {
  const size_t src_bytes_used = ring_buffer_used(src, true);
  const size_t nbytes = n * src->stride;
  if (src == dest || src->stride != dest->stride || nbytes > src_bytes_used) {
    return NULL;
  }
  const bool overflow = ring_buffer_handle_overflow(dest, n);

  const data_t *src_bufend = ring_buffer_end(src);
  const data_t *dest_bufend = ring_buffer_end(dest);
  size_t ncopied = 0;
  while (ncopied != nbytes) {
    size_t nsrc = imin(src_bufend - src->tail, nbytes - ncopied);
    size_t n = imin(dest_bufend - dest->head, nsrc);
    memcpy(dest->head, src->tail, n);
    src->tail += n;
    dest->head += n;
    ncopied += n;

    // wrap?
    if (src->tail == src_bufend) {
      src->tail = src->data;
    }
    if (dest->head == dest_bufend) {
      dest->head = dest->data;
    }
  }

  if (overflow) {
    dest->tail = ring_buffer_nextp(dest, dest->head);
  }

  return dest->head;
}

bool ring_buffer_mirror(const ring_buffer *src, ring_buffer *dest) {
  const bool ok = src != dest &&
    src->size == dest->size && src->stride == dest->stride;
  if (ok) {
    memcpy(dest->data, src->data, dest->bytes_data);
    dest->head = dest->data + ring_buffer_head_pos(src, true);
    dest->tail = dest->data + ring_buffer_tail_pos(src, true);
  }
  return ok;
}

const void * ring_buffer_tail_offset(const ring_buffer *buffer, size_t offset) {
  const size_t bytes_used = ring_buffer_used(buffer, true);
  const size_t nbytes = offset * buffer->stride;
  if (nbytes >= bytes_used) {
    return NULL;
  }
  const data_t *tail = buffer->tail;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nmoved = 0;

  while (nmoved < nbytes) {
    size_t n = imin(bufend - tail, nbytes - nmoved);
    tail += n;
    nmoved += n;
    if (tail == bufend) {
      tail = buffer->data;
    }
  }

  return tail;
}

const void * ring_buffer_head_offset(const ring_buffer *buffer, size_t offset) {
  const size_t bytes_used = ring_buffer_used(buffer, true);
  const size_t nbytes = (offset + 1) * buffer->stride;
  if (nbytes > bytes_used) {
    return NULL;
  }
  const data_t *head = buffer->head;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nmoved = 0;

  while (nmoved < nbytes) {
    if (head == buffer->data) {
      head = bufend;
    }
    size_t n = imin(head - buffer->data, nbytes - nmoved);
    head -= n;
    nmoved += n;
  }

  return head;
}

void * ring_buffer_head_advance(ring_buffer *buffer) {
  const bool overflow = ring_buffer_handle_overflow(buffer, 1);
  const data_t *bufend = ring_buffer_end(buffer);

  buffer->head += buffer->stride;
  if (buffer->head == bufend) {
    buffer->head = buffer->data;
  }
  if (overflow) {
    buffer->tail = ring_buffer_nextp(buffer, buffer->head);
  }

  return buffer->head;
}

// This one is really just for testing; it's designed to be stupid and
// simple and check that the general search system works, but not to
// be fast.
const void * ring_buffer_search_linear(const ring_buffer *buffer,
                                       ring_predicate *pred, void *data) {
  const size_t n = ring_buffer_used(buffer, false);
  if (n == 0) {
    // Don't do any search here; there is no position such that
    //   buffer[i] < data
    return NULL;
  }
  size_t i = 0;
  const void *xl = ring_buffer_tail_offset(buffer, i), *xr;
  if (!pred(xl, data)) {
    // There will be not a single value here that satisfies the
    // required condition
    return NULL;
  }

  do {
    i++;
    if (i == n) {
      return xl;
    }
    xr = ring_buffer_tail_offset(buffer, i);
    if (!pred(xr, data)) {
      return xl;
    } else {
      xl = xr;
    }
  } while (1);

  return NULL; // # nocov
}

// Do a search.  There a few possibilities of where to start from
// here; we could start with the edges of the array, or we could start
// at one end and grow, or from a position in the array itself.
const void * ring_buffer_search_bisect(const ring_buffer *buffer, size_t i,
                                       ring_predicate *pred, void *data) {
  const size_t n = ring_buffer_used(buffer, false);
  if (n == 0) {
    return NULL;
  }
  int i0 = i, i1 = i;
  const void *x0 = ring_buffer_tail_offset(buffer, i0), *x1;
  int inc = 1;

  // Predicate should return true if we should look further back
  // (increase the tail offset), false otherwise.
  if (pred((void*) x0, data)) { // advance up until we hit the top
    if (i0 == (int)n - 1) { // guess is already *at* the top.
      return x0;
    }
    i1 = i0 + 1;
    x1 = ring_buffer_tail_offset(buffer, i1);
    while (pred((void*) x1, data)) {
      i0 = i1;
      x0 = x1;
      inc *= 2;
      i1 += inc;
      if (i1 >= (int)n) { // off the end of the buffer
        i1 = n - 1;
        x1 = ring_buffer_tail_offset(buffer, i1);
        if (pred((void*) x1, data)) {
          return x1;
        }
        break;
      }
      x1 = ring_buffer_tail_offset(buffer, i1);
    }
  } else { // advance down
    // who else uses the bisect search?  they'll have the same issue
    // that I see here; odin interpolation.
    if (i0 == 0) { // guess is already at the bottom
      return NULL;
    }
    i1 = i0;
    x1 = x0;
    i0 = i0 - 1;
    x0 = ring_buffer_tail_offset(buffer, i0);
    while (!pred((void*) x0, data)) {
      i1 = i0;
      x1 = x0;
      inc *= 2;
      if (i0 < inc) {
        i0 = 0;
        x0 = ring_buffer_tail_offset(buffer, i0);
        if (!pred((void*) x0, data)) {
          return NULL;
        }
        break;
      }
      i0 -= inc;
      x0 = ring_buffer_tail_offset(buffer, i0);
    }
  }

  // TODO: Here, we'll do a bit of trickery because we'll want to
  // treat the case of the ends being wrapped or not.  This is going
  // to be the case when x0 > x1; in that case we can pop the first
  // point to check at the end of the buffer, compare that and
  // continue.  The actual checks simplify after that because the
  // indices go away and everything is pointer arithmetic, based on
  // the ring buffer stride.  For now, use the bisection search:
  while (i1 - i0 > 1) {
    int i2 = (i1 + i0) / 2;
    const void *x2 = ring_buffer_tail_offset(buffer, i2);
    if (pred((void*) x2, data)) {
      i0 = i2;
      x0 = x2;
    } else {
      i1 = i2;
      x1 = x2;
    }
  }

  return x0;
}

// Internal functions below here...
const data_t * ring_buffer_end(const ring_buffer *buffer) {
  return buffer->data + ring_buffer_bytes_data(buffer);
}

// Given a ring buffer buffer and a pointer to a location within its
// contiguous buffer, return the a pointer to the next logical
// location in the ring buffer.
data_t * ring_buffer_nextp(ring_buffer *buffer, const data_t *p) {
  p += buffer->stride;
  return buffer->data + (p - buffer->data) % ring_buffer_bytes_data(buffer);
}

int imin(int a, int b) {
  return a < b ? a : b;
}

bool ring_buffer_handle_overflow(ring_buffer *buffer, size_t n) {
  bool overflow = ring_buffer_free(buffer, true) < n * buffer->stride;
  if (overflow) {
    switch (buffer->on_overflow) {
    case OVERFLOW_OVERWRITE:
      break; // do nothing
    case OVERFLOW_GROW:
      ring_buffer_grow(buffer, n, false);
      overflow = false;
      break;
#ifdef USING_R
    case OVERFLOW_ERROR:
      Rf_error("Buffer overflow (adding %d elements, but %d available)",
               n, ring_buffer_free(buffer, false));
      break;
#endif
    }
  }
  return overflow;
}
