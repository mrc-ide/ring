/* #ifndef _CIRCLE_H_ */
/* #define _CIRCLE_H_ */
#include <stddef.h>

typedef unsigned char data_t;

// TODO: consider having an integer that keeps track of the number f
// times that the buffer has wrapped.  We could use this to test
// cursor invalidation (though that would require some care I think).

// As we move from storing bytes to storing all sorts of things, we
// need to tweak the storage model a bit:
//
//   size: the number of logical units that may be stored in the buffer
//   bytes_data: the number of bytes (including padding) that data holds

typedef struct circle_buffer {
  size_t size;
  size_t stride;
  size_t bytes_data;

  data_t *data;
  data_t *head;
  data_t *tail;
} circle_buffer;

circle_buffer * circle_buffer_create(size_t size, size_t stride);
circle_buffer * circle_buffer_clone(const circle_buffer *buffer);
void circle_buffer_destroy(circle_buffer *buffer);
size_t circle_buffer_bytes_data(const circle_buffer *buffer);

size_t circle_buffer_size(const circle_buffer *buffer, int bytes);

int circle_buffer_full(circle_buffer *buffer);
int circle_buffer_empty(circle_buffer *buffer);

const void * circle_buffer_head(circle_buffer *buffer);
const void * circle_buffer_tail(circle_buffer *buffer);
const void * circle_buffer_data(circle_buffer *buffer);

int circle_buffer_head_pos(const circle_buffer *buffer, int bytes);
int circle_buffer_tail_pos(const circle_buffer *buffer, int bytes);

void * circle_buffer_tail_read(circle_buffer *buffer, void *dest,
                               size_t count);

void circle_buffer_reset(circle_buffer *buffer);

size_t circle_buffer_free(const circle_buffer *buffer, int bytes);
size_t circle_buffer_used(const circle_buffer *buffer, int bytes);

size_t circle_buffer_memset(circle_buffer *dst, int c, size_t len);
void *circle_buffer_memcpy_into(circle_buffer *buffer, const void *src,
                                size_t count);
void *circle_buffer_memcpy_from(void *dest, circle_buffer *buffer,
                                size_t count);

void * circle_buffer_copy(circle_buffer *dst, circle_buffer *src, size_t count);

void * circle_buffer_tail_offset(circle_buffer *buffer, size_t offset);

#ifdef CIRCLE_INTERNAL_PROTOTYPES
data_t * circle_buffer_end(circle_buffer *buffer);
data_t * circle_buffer_nextp(circle_buffer *buffer, const data_t *p);
#endif

/* #endif */
