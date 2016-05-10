#define CIRCLE_INTERNAL_PROTOTYPES
#include <circle/circle.h>
#include "util.h"
// only used for the Calloc and Free -- could drop back to use plain C?
//
// using plain C only would make this nicer to include in arbitrary
// projects without having to worry about it being an R project but
// that's a pretty minor gain at this point.
#include <R.h>

circle_buffer * circle_buffer_create(size_t size, size_t stride) {
  circle_buffer * buffer = Calloc(1, circle_buffer);
  buffer->size = size;
  buffer->stride = stride;
  buffer->bytes_data = size * stride + 1;

  buffer->data = Calloc(buffer->bytes_data, data_t);
  circle_buffer_reset(buffer);
  return buffer;
}

circle_buffer * circle_buffer_clone(const circle_buffer *buffer) {
  circle_buffer *ret = circle_buffer_create(buffer->size, buffer->stride);
  memcpy(ret->data, buffer->data, ret->bytes_data);
  ret->head += circle_buffer_head_pos(buffer);
  ret->tail += circle_buffer_tail_pos(buffer);
  return ret;
}

void circle_buffer_destroy(circle_buffer *buffer) {
  Free(buffer->data);
  Free(buffer);
}

size_t circle_buffer_bytes_data(const circle_buffer *buffer) {
  return buffer->bytes_data;
}

size_t circle_buffer_size(const circle_buffer *buffer, int bytes) {
  return bytes ? buffer->bytes_data - 1 : buffer->size;
}

int circle_buffer_full(circle_buffer *buffer) {
  return circle_buffer_bytes_free(buffer) == 0;
}

int circle_buffer_empty(circle_buffer *buffer) {
  return circle_buffer_bytes_free(buffer) == circle_buffer_size(buffer, 1);
}

const void * circle_buffer_head(circle_buffer *buffer) {
  return buffer->head;
}
const void * circle_buffer_tail(circle_buffer *buffer) {
  return buffer->tail;
}
const void * circle_buffer_data(circle_buffer *buffer) {
  return buffer->data;
}

int circle_buffer_head_pos(const circle_buffer *buffer) {
  return buffer->head - buffer->data;
}
int circle_buffer_tail_pos(const circle_buffer *buffer) {
  return buffer->tail - buffer->data;
}

void circle_buffer_reset(circle_buffer *buffer) {
  buffer->head = buffer->tail = buffer->data;
}

size_t circle_buffer_bytes_free(const circle_buffer *buffer) {
  if (buffer->head >= buffer->tail) {
    return circle_buffer_size(buffer, 1) - (buffer->head - buffer->tail);
  } else {
    return buffer->tail - buffer->head - 1;
  }
}

size_t circle_buffer_bytes_used(const circle_buffer *buffer) {
  return circle_buffer_size(buffer, 1) - circle_buffer_bytes_free(buffer);
}

size_t circle_buffer_free(const circle_buffer *buffer) {
  return circle_buffer_bytes_free(buffer) / buffer->stride;
}

size_t circle_buffer_used(const circle_buffer *buffer) {
  return circle_buffer_bytes_used(buffer) / buffer->stride;
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
 * circle_buffer_bytes_data(buffer), else
 * circle_buffer_bytes_data(buffer).
 */
// It's not really clear what this should do with stride?  Probably if
// len does not divide neatly through by stride we should error.  For
// now, leave it be though.
size_t circle_buffer_memset(circle_buffer *buffer, int c, size_t len) {
  const data_t *bufend = circle_buffer_end(buffer);
  size_t nwritten = 0;
  size_t count = imin(len, circle_buffer_bytes_data(buffer));
  int overflow = count > circle_buffer_bytes_free(buffer);

  while (nwritten != count) {
    /* don't copy beyond the end of the buffer */
    // assert(bufend > buffer->head);
    size_t n = imin(bufend - buffer->head, count - nwritten);
    memset(buffer->head, c, n);
    buffer->head += n;
    nwritten += n;

    /* wrap? */
    if (buffer->head == bufend) {
      buffer->head = buffer->data;
    }
  }

  if (overflow) {
    buffer->tail = circle_buffer_nextp(buffer, buffer->head);
    //assert(circle_buffer_is_full(buffer));
  }

  return nwritten;
}

// The bits that are complicated..
/*
 * Copy n * stride bytes from a contiguous memory area src into the
 * ring buffer dst. Returns the ring buffer's new head pointer.
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
void *circle_buffer_memcpy_into(circle_buffer *buffer, const void *src,
                                size_t count) {
  // TODO: if length of count is not divisible nicely by stride, it is an error
  const size_t len = count * buffer->stride;
  const data_t *source = src;
  const data_t *bufend = circle_buffer_end(buffer);
  size_t overflow = count > circle_buffer_bytes_free(buffer);
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
    buffer->tail = circle_buffer_nextp(buffer, buffer->head);
  }
  return buffer->head;
}

/*
 * Copy n bytes from the ring buffer src, starting from its tail
 * pointer, into a contiguous memory area dst. Returns the value of
 * src's tail pointer after the copy is finished.
 *
 * Note that this copy is destructive with respect to the ring buffer:
 * the n bytes copied from the ring buffer are no longer available in
 * the ring buffer after the copy is complete, and the ring buffer
 * will have n more free bytes than it did before the function was
 * called.
 *
*  This function will *not* allow the ring buffer to underflow. If
 * count is greater than the number of bytes used in the ring buffer,
 * no bytes are copied, and the function will return 0.
 */
void *circle_buffer_memcpy_from(void *dest, circle_buffer *buffer,
                                size_t count) {
  // TODO: if length of count is not divisible nicely by stride, it is an error
  size_t bytes_used = circle_buffer_bytes_used(buffer);
  size_t len = count * buffer->stride;
  if (len > bytes_used) {
    return 0;
  }

  data_t *dest_data = dest;
  const data_t *bufend = circle_buffer_end(buffer);
  size_t nwritten = 0;
  while (nwritten != len) {
    // assert(bufend > buffer->tail);
    size_t n = imin(bufend - buffer->tail, len - nwritten);
    memcpy(dest_data + nwritten, buffer->tail, n);
    buffer->tail += n;
    nwritten += n;

    // wrap?
    if (buffer->tail == bufend) {
      buffer->tail = buffer->data;
    }
  }

  // assert(len + circle_buffer_bytes_used(buffer) == bytes_used);
  return buffer->tail;
  // TODO: try
  /* void * tail = circle_buffer_tail_read(buffer, dest, count); */
  /* if (tail != 0) { */
  /*   buffer->tail = tail; */
  /* } */
  /* return tail; */
}

// This is like the function above, but it is not destructive to the
// buffer.  I need to write a similar one that reads relative to the
// head too (i.e. that will pull the most recently added data).
void *circle_buffer_tail_read(circle_buffer *buffer, void *dest, size_t count) {
  // TODO: if length of count is not divisible nicely by stride, it is an error
  size_t bytes_used = circle_buffer_bytes_used(buffer);
  size_t len = count * buffer->stride;
  if (len > bytes_used) {
    return 0;
  }
  data_t *tail = buffer->tail;

  data_t *dest_data = dest;
  const data_t *bufend = circle_buffer_end(buffer);
  size_t nwritten = 0;
  while (nwritten != len) {
    size_t n = imin(bufend - tail, len - nwritten);
    memcpy(dest_data + nwritten, tail, n);
    tail += n;
    nwritten += n;
    if (tail == bufend) {
      tail = buffer->data;
    }
  }
  return tail;
}

// OK, so it would be great to have a check here that can be used to
// move an offset around without doing the inbounds check
// everyiteration.  But to do that I think that we'll still have to
// separate into a safe and unsafe interface.  The memoset returned
// here could be invalidated by any write function (any many of the
// read functions too) so this is inherently unsafe, but on entry it
// does seem worth checking.
void *circle_buffer_tail_offset(circle_buffer *buffer, size_t offset) {
  size_t bytes_used = circle_buffer_bytes_used(buffer);
  size_t len = offset * buffer->stride;
  if (len >= bytes_used) {
    // TODO: elsewhere this is > bytes used but I think that here,
    // because the offset is the number of places to *move* the
    // pointer, then copy one, we can't do that.
    return 0;
  }
  data_t *tail = buffer->tail;
  const data_t *bufend = circle_buffer_end(buffer);
  size_t nmoved = 0;

  // TODO: this is really a much simpler construct than this as we can
  // only go around once.
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

/*
 * Copy count bytes from ring buffer src, starting from its tail
 * pointer, into ring buffer dst. Returns dst's new head pointer after
 * the copy is finished.
 *
 * Note that this copy is destructive with respect to the ring buffer
 * src: any bytes copied from src into dst are no longer available in
 * src after the copy is complete, and src will have 'count' more free
 * bytes than it did before the function was called.
 *
 * It is possible to copy more data from src than is available in dst;
 * i.e., it's possible to overflow dst using this function. When an
 * overflow occurs, the state of dst is guaranteed to be consistent,
 * including the head and tail pointers; old data will simply be
 * overwritten in FIFO fashion, as needed. However, note that, if
 * calling the function results in an overflow, the value dst's tail
 * pointer may be different than it was before the function was
 * called.
 *
 * It is *not* possible to underflow src; if count is greater than the
 * number of bytes used in src, no bytes are copied, and the function
 * returns 0.
 */
void * circle_buffer_copy(circle_buffer *dst, circle_buffer *src,
                          size_t count) {
  // TODO: Not clear what should be done (if anything other than an
  // error) if the two buffers differ in their stride.
  size_t src_bytes_used = circle_buffer_bytes_used(src);
  size_t n_bytes = count * src->stride;
  if (n_bytes > src_bytes_used) {
    return 0;
  }
  int overflow = n_bytes > circle_buffer_bytes_free(dst);

  const data_t *src_bufend = circle_buffer_end(src);
  const data_t *dst_bufend = circle_buffer_end(dst);
  size_t ncopied = 0;
  while (ncopied != n_bytes) {
    // assert(src_bufend > src->tail);
    size_t nsrc = imin(src_bufend - src->tail, n_bytes - ncopied);
    // assert(dst_bufend > dst->head);
    size_t n = imin(dst_bufend - dst->head, nsrc);
    memcpy(dst->head, src->tail, n);
    src->tail += n;
    dst->head += n;
    ncopied += n;

    /* wrap ? */
    if (src->tail == src_bufend) {
      src->tail = src->data;
    }
    if (dst->head == dst_bufend) {
      dst->head = dst->data;
    }
  }

  // assert(n_bytes + circle_buffer_bytes_used(src) == src_bytes_used);

  if (overflow) {
    dst->tail = circle_buffer_nextp(dst, dst->head);
    // assert(circle_buffer_full(dst));
  }

  return dst->head;
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
data_t * circle_buffer_end(circle_buffer *buffer) {
  return buffer->data + circle_buffer_bytes_data(buffer);
}

/*
 * Given a ring buffer buffer and a pointer to a location within its
 * contiguous buffer, return the a pointer to the next logical
 * location in the ring buffer.
 */
data_t * circle_buffer_nextp(circle_buffer *buffer, const data_t *p) {
  /*
   * The assert guarantees the expression (++p - buffer->data) is
   * non-negative; therefore, the modulus operation is safe and
   * portable.
   */
  // assert((p >= buffer->data) && (p < circle_buffer_end(buffer)));
  // TODO: fix this up for working with the stride information...
  //   p += buffer->stride;
  //   buffer->data + (p - buffer->data) % circle_buffer_bytes_data(buffer);
  return buffer->data + ((++p - buffer->data) % circle_buffer_bytes_data(buffer));
}
