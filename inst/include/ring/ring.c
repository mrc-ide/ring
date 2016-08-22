#include <ring/ring.h>

// Some prototypes used here that aren't public:
const data_t * ring_buffer_end(const ring_buffer *buffer);
data_t * ring_buffer_nextp(ring_buffer *buffer, const data_t *p);
int imin(int a, int b);

#ifdef RING_USE_STDLIB_ALLOC
#include <stdlib.h>
#include <string.h>
#else
#include <R.h>
#endif

ring_buffer * ring_buffer_create(size_t size, size_t stride) {
  size_t bytes_data = (size + 1) * stride;
#ifdef RING_USE_STDLIB_ALLOC
  ring_buffer * buffer = (ring_buffer*) calloc(1, sizeof(ring_buffer));
  if (buffer == NULL) {
    return NULL;
  }
  buffer->data = (data_t*) calloc(bytes_data, sizeof(data_t));
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
  ring_buffer_reset(buffer);
  return buffer;
}

ring_buffer * ring_buffer_clone(const ring_buffer *buffer) {
  ring_buffer *ret = ring_buffer_create(buffer->size, buffer->stride);
#ifdef RING_USE_STDLIB_ALLOC
  if (ret == NULL) {
    return NULL;
  }
#endif
  memcpy(ret->data, buffer->data, ret->bytes_data);
  ret->head += ring_buffer_head_pos(buffer, true);
  ret->tail += ring_buffer_tail_pos(buffer, true);
  return ret;
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

size_t ring_buffer_bytes_data(const ring_buffer *buffer) {
  return buffer->bytes_data;
}

size_t ring_buffer_size(const ring_buffer *buffer, bool bytes) {
  return bytes ? buffer->bytes_data - buffer->stride : buffer->size;
}

bool ring_buffer_full(const ring_buffer *buffer) {
  return ring_buffer_free(buffer, true) == 0;
}

bool ring_buffer_empty(const ring_buffer *buffer) {
  return ring_buffer_free(buffer, true) == ring_buffer_size(buffer, true);
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

size_t ring_buffer_head_pos(const ring_buffer *buffer, bool bytes) {
  size_t diff = buffer->head - buffer->data;
  return bytes ? diff : diff / buffer->stride;
}
size_t ring_buffer_tail_pos(const ring_buffer *buffer, bool bytes) {
  size_t diff = buffer->tail - buffer->data;
  return bytes ? diff : diff / buffer->stride;
}

void ring_buffer_reset(ring_buffer *buffer) {
  buffer->head = buffer->tail = buffer->data;
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

/*
 * Beginning at ring buffer buffer's head pointer, fill the ring buffer
 * with a repeating sequence of len bytes, each of value c (converted
 * to an unsigned char). len can be as large as you like, but the
 * function will never write more than ringbuf_buffer_size(buffer) bytes
 * in a single invocation, since that size will cause all bytes in the
 * ring buffer to be written exactly once each.
 *
 * Note that if len is greater than the number of free bytes in buffer,
 * the ring buffer will overflow. When an overflow occurs, the state
 * of the ring buffer is guaranteed to be consistent, including the
 * head and tail pointers; old data will simply be overwritten in FIFO
 * fashion, as needed. However, note that, if calling the function
 * results in an overflow, the value of the ring buffer's tail pointer
 * may be different than it was before the function was called.
 *
 * Returns the actual number of bytes written to buffer: len, if len <
 * ring_buffer_bytes_data(buffer), else
 * ring_buffer_bytes_data(buffer).
 */
size_t ring_buffer_set(ring_buffer *buffer, data_t c, size_t n) {
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nwritten = 0;
  size_t len = imin(n * buffer->stride, ring_buffer_bytes_data(buffer));
  bool overflow = len > ring_buffer_free(buffer, true);

  while (nwritten != len) {
    /* don't copy beyond the end of the buffer */
    // assert(bufend > buffer->head);
    size_t n = imin(bufend - buffer->head, len - nwritten);
    memset(buffer->head, c, n);
    buffer->head += n;
    nwritten += n;

    /* wrap? */
    if (buffer->head == bufend) {
      buffer->head = buffer->data;
    }
  }

  if (overflow) {
    buffer->tail = ring_buffer_nextp(buffer, buffer->head);
    //assert(ring_buffer_is_full(buffer));
  }

  return nwritten;
}

size_t ring_buffer_set_stride(ring_buffer *buffer, const void *x, size_t len) {
  size_t n = imin(len, ring_buffer_size(buffer, false));
  for (size_t i = 0; i < n; ++i) {
    ring_buffer_push(buffer, x, 1);
  }
  return n;
}

// The bits that are complicated..
/*
 * Copy n * stride bytes from a contiguous memory area src into the
 * ring buffer dest. Returns the ring buffer's new head pointer.
 *
 * It is possible to copy more data from src than is available in the
 * buffer; i.e., it's possible to overflow the ring buffer using this
 * function. When an overflow occurs, the state of the ring buffer is
 * guaranteed to be consistent, including the head and tail pointers;
 * old data will simply be overwritten in FIFO fashion, as
 * needed. However, note that, if calling the function results in an
 * overflow, the value of the ring buffer's tail pointer may be
 * different than it was before the function was called.
 */
const void * ring_buffer_push(ring_buffer *buffer, const void *src, size_t n) {
  const size_t len = n * buffer->stride;
  const data_t *source = (const data_t*)src;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t overflow = len > ring_buffer_free(buffer, true);
  size_t nread = 0;
  while (nread != len) {
    size_t n = imin(bufend - buffer->head, len - nread);
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

// Advance the ring buffer by one position and return a pointer to the
// memory *without writing anything to it*.  In this case, the calling
// function is responsible for setting the memory to something
// sensible.  This is currently used in dde where we want to write
// directly to the head.
//
// This requires alignment is bang on, and I think I have got that
// working now.
//
// TODO: This needs solid testing, but that's actually pretty hard to
// do because this one is designed only to be used in C code.
void * ring_buffer_head_advance(ring_buffer *buffer) {
  bool overflow = ring_buffer_full(buffer);
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

/*
 * Copy n bytes from the ring buffer "buffer", starting from its tail
 * pointer, into a contiguous memory area dest. Returns the value of
 * src's tail pointer after the copy is finished.
 *
 * Note that this copy is destructive with respect to the ring buffer:
 * the n bytes copied from the ring buffer are no longer available in
 * the ring buffer after the copy is complete, and the ring buffer
 * will have n more free bytes than it did before the function was
 * called.
 *
*  This function will *not* allow the ring buffer to underflow. If
 * n is greater than the number of bytes used in the ring buffer,
 * no bytes are copied, and the function will return NULL.
 */
const void * ring_buffer_take(ring_buffer *buffer, void *dest, size_t n) {
  const void * tail = ring_buffer_read(buffer, dest, n);
  if (tail != 0) {
    buffer->tail = buffer->data + ((data_t*)tail - buffer->data);
  }
  return tail;
}

// This is like the function above, but it is not destructive to the
// buffer.  I need to write a similar one that reads relative to the
// head too (i.e. that will pull the most recently added data).
const void * ring_buffer_read(const ring_buffer *buffer, void *dest, size_t n) {
  size_t bytes_used = ring_buffer_used(buffer, true);
  size_t len = n * buffer->stride;
  if (len > bytes_used) {
    return NULL;
  }
  const data_t *tail = buffer->tail;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nwritten = 0;
  // TODO: This can be rewritten to allow at once one switch point
  // which is probably the same assembly but might be nicer to read?
  // I believe that this is going to be sufficiently tested that I can
  // just try replacing the logic here and seeing if they all pass.
  while (nwritten != len) {
    size_t n = imin(bufend - tail, len - nwritten);
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

// This is like the function above, but it is not destructive to the
// buffer.  I need to write a similar one that reads relative to the
// head too (i.e. that will pull the most recently added data).
const void * ring_buffer_read_head(const ring_buffer *buffer, void *dest,
                                   size_t n) {
  size_t bytes_used = ring_buffer_used(buffer, true);
  size_t len = n * buffer->stride;
  if (len > bytes_used) {
    return NULL;
  }
  const data_t *head = buffer->head;
  const data_t *bufend = ring_buffer_end(buffer);
  data_t *dest_data = (data_t*) dest; // cast so pointer arithmetic works

  for (size_t nwritten = 0; nwritten < len; ++nwritten) {
    if (head == buffer->data) {
      head = bufend;
    }
    head -= buffer->stride;
    memcpy((void*)dest_data, head, buffer->stride);
    dest_data += buffer->stride;
  }

  return head;
}

// OK, so it would be great to have a check here that can be used to
// move an offset around without doing the inbounds check
// everyiteration.  But to do that I think that we'll still have to
// separate into a safe and unsafe interface.  The memory returned
// here could be invalidated by any write function (any many of the
// read functions too) so this is inherently unsafe, but on entry it
// does seem worth checking.
const void * ring_buffer_tail_offset(const ring_buffer *buffer, size_t offset) {
  size_t bytes_used = ring_buffer_used(buffer, true);
  size_t len = offset * buffer->stride;
  if (len >= bytes_used) {
    return NULL;
  }
  const data_t *tail = buffer->tail;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nmoved = 0;

  // TODO: this is really a much simpler construct than this as we can
  // only go around once (see also head_offset below which is
  // basically the same code).
  while (nmoved < len) {
    size_t n = imin(bufend - tail, len - nmoved);
    tail += n;
    nmoved += n;
    if (tail == bufend) {
      tail = buffer->data;
    }
  }

  return tail;
}

const void * ring_buffer_head_offset(const ring_buffer *buffer, size_t offset) {
  size_t bytes_used = ring_buffer_used(buffer, true);
  size_t len = (offset + 1) * buffer->stride;
  if (len > bytes_used) {
    return NULL;
  }
  const data_t *head = buffer->head;
  const data_t *bufend = ring_buffer_end(buffer);
  size_t nmoved = 0;

  while (nmoved < len) {
    if (head == buffer->data) {
      head = bufend;
    }
    size_t n = imin(head - buffer->data, len - nmoved);
    head -= n;
    nmoved += n;
  }

  return head;
}

/*
 * Copy n bytes from ring buffer src, starting from its tail
 * pointer, into ring buffer dest. Returns dest's new head pointer after
 * the copy is finished.
 *
 * Note that this copy is destructive with respect to the ring buffer
 * src: any bytes copied from src into dest are no longer available in
 * src after the copy is complete, and src will have 'n' more free
 * bytes than it did before the function was called.
 *
 * It is possible to copy more data from src than is available in dest;
 * i.e., it's possible to overflow dest using this function. When an
 * overflow occurs, the state of dest is guaranteed to be consistent,
 * including the head and tail pointers; old data will simply be
 * overwritten in FIFO fashion, as needed. However, note that, if
 * calling the function results in an overflow, the value dest's tail
 * pointer may be different than it was before the function was
 * called.
 *
 * It is *not* possible to underflow src; if n is greater than the
 * number of bytes used in src, no bytes are copied, and the function
 * returns 0.
 */
const void * ring_buffer_copy(ring_buffer *src, ring_buffer *dest, size_t n) {
  // TODO: Not clear what should be done (if anything other than an
  // error) if the two buffers differ in their stride.
  size_t src_bytes_used = ring_buffer_used(src, true);
  size_t n_bytes = n * src->stride;
  if (n_bytes > src_bytes_used) {
    return NULL;
  }
  bool overflow = n_bytes > ring_buffer_free(dest, true);

  const data_t *src_bufend = ring_buffer_end(src);
  const data_t *dest_bufend = ring_buffer_end(dest);
  size_t ncopied = 0;
  while (ncopied != n_bytes) {
    // assert(src_bufend > src->tail);
    size_t nsrc = imin(src_bufend - src->tail, n_bytes - ncopied);
    // assert(dest_bufend > dest->head);
    size_t n = imin(dest_bufend - dest->head, nsrc);
    memcpy(dest->head, src->tail, n);
    src->tail += n;
    dest->head += n;
    ncopied += n;

    /* wrap ? */
    if (src->tail == src_bufend) {
      src->tail = src->data;
    }
    if (dest->head == dest_bufend) {
      dest->head = dest->data;
    }
  }

  // assert(n_bytes + ring_buffer_used(src, true) == src_bytes_used);

  if (overflow) {
    dest->tail = ring_buffer_nextp(dest, dest->head);
    // assert(ring_buffer_full(dest));
  }

  return dest->head;
}

// TODO: Still need one that can copy an element from 'n' ago without moving
// the head/tail pointers.
//
// TODO: Still need one that can do a linear search on an index (that
// would really require passing in a predicate function that can take
// a constant data thing as the argument, which is a bit of a faff in
// C, so worth creating a specialised function for the case where
// there is an int/double and we check up or down).
//
// TODO: Still need (for the above) something that will represent a
// cursor around the buffer.

// Internal functions below here...
const data_t * ring_buffer_end(const ring_buffer *buffer) {
  return buffer->data + ring_buffer_bytes_data(buffer);
}

/*
 * Given a ring buffer buffer and a pointer to a location within its
 * contiguous buffer, return the a pointer to the next logical
 * location in the ring buffer.
 */
data_t * ring_buffer_nextp(ring_buffer *buffer, const data_t *p) {
  /*
   * The assert guarantees the expression (++p - buffer->data) is
   * non-negative; therefore, the modulus operation is safe and
   * portable.
   */
  // assert((p >= buffer->data) && (p < ring_buffer_end(buffer)));
  p += buffer->stride;
  return buffer->data + (p - buffer->data) % ring_buffer_bytes_data(buffer);
}

int imin(int a, int b) {
  return a < b ? a : b;
}

// This one is really just for testing; it's designed to be stupid and
// simple and check that the general search system works, but not to
// be fast.
const void * ring_buffer_search_linear(const ring_buffer *buffer,
                                       ring_predicate *pred, void *data) {
  size_t n = ring_buffer_used(buffer, false);
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

  return xl;
}

// Do a search.  There a few possibilities of where to start from
// here; we could start with the edges of the array, or we could start
// at one end and grow, or from a position in the array itself.
const void * ring_buffer_search_bisect(const ring_buffer *buffer, size_t i,
                                       ring_predicate *pred, void *data) {
  size_t n = ring_buffer_used(buffer, false);
  if (n == 0) {
    return NULL;
  }
  int i0 = i, i1 = i;
  const void *x0 = ring_buffer_tail_offset(buffer, i0), *x1;
  int inc = 1;

  // Predicate should return 1 if we should look further back, -1
  // otherwise.
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
        break;
      }
      x1 = ring_buffer_tail_offset(buffer, i1);
    }
  } else { // advance down
    if (i0 == 0) { // guess is already at the bottom
      return NULL;
    }
    x1 = x0;
    i0 = i0 - 1;
    x0 = ring_buffer_tail_offset(buffer, i1);
    while (!pred((void*) x0, data)) {
      i1 = i0;
      x1 = x0;
      inc *= 2;
      if (i0 < inc) {
        i0 = 0;
        x0 = ring_buffer_tail_offset(buffer, i0);
        break;
      }
      i0 -= inc;
      x0 = ring_buffer_tail_offset(buffer, i0);
    }
  }

  // Need to deal specially with this case apparently, but not sure
  // why.  It's possible that this only needs doing on one of the
  // early exits from the above loops.
  if (i1 - i0 == 1 && pred((void*) x1, data)) {
    x0 = x1;
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
