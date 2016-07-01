#ifndef _RING_H_
#define _RING_H_
#include <stddef.h>
#include <stdbool.h>

typedef unsigned char data_t;

// TODO: consider having an integer that keeps track of the number f
// times that the buffer has wrapped.  We could use this to test
// cursor invalidation (though that would require some care I think).

// As we move from storing bytes to storing all sorts of things, we
// need to tweak the storage model a bit:
//
//   size: the number of logical units that may be stored in the buffer
//   bytes_data: the number of bytes (including padding) that data holds

typedef struct ring_buffer {
  size_t size;
  size_t stride;
  size_t bytes_data;

  data_t *data;
  data_t *head;
  data_t *tail;
} ring_buffer;

ring_buffer * ring_buffer_create(size_t size, size_t stride);
ring_buffer * ring_buffer_clone(const ring_buffer *buffer);
void ring_buffer_destroy(ring_buffer *buffer);
size_t ring_buffer_bytes_data(const ring_buffer *buffer);

size_t ring_buffer_size(const ring_buffer *buffer, int bytes);

int ring_buffer_full(ring_buffer *buffer);
int ring_buffer_empty(ring_buffer *buffer);

const void * ring_buffer_head(ring_buffer *buffer);
const void * ring_buffer_tail(ring_buffer *buffer);
const void * ring_buffer_data(ring_buffer *buffer);

int ring_buffer_head_pos(const ring_buffer *buffer, int bytes);
int ring_buffer_tail_pos(const ring_buffer *buffer, int bytes);

void * ring_buffer_tail_read(ring_buffer *buffer, void *dest, size_t count);

void ring_buffer_reset(ring_buffer *buffer);

size_t ring_buffer_free(const ring_buffer *buffer, int bytes);
size_t ring_buffer_used(const ring_buffer *buffer, int bytes);

size_t ring_buffer_memset(ring_buffer *buffer, int c, size_t len);
size_t ring_buffer_memset_stride(ring_buffer *buffer, data_t *x, size_t len);
void *ring_buffer_memcpy_into(ring_buffer *buffer, const void *src,
                              size_t count);
void *ring_buffer_memcpy_from(void *dest, ring_buffer *buffer, size_t count);

void * ring_buffer_copy(ring_buffer *dst, ring_buffer *src, size_t count);

void * ring_buffer_tail_offset(ring_buffer *buffer, size_t offset);

void* ring_buffer_head_advance(ring_buffer* buffer);

typedef bool ring_predicate(void *x, void *data);
void* ring_buffer_search_linear(ring_buffer* buffer,
                                ring_predicate pred, void *data);
void* ring_buffer_search_bisect(ring_buffer* buffer, size_t i,
                                ring_predicate pred, void *data);

#endif
