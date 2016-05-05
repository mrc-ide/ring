/* #ifndef _CIRCLE_H_ */
/* #define _CIRCLE_H_ */
#include <stddef.h>

typedef unsigned char data_t;

typedef struct circle_buffer {
  data_t *data;
  data_t *head;
  data_t *tail;
  size_t size;
  size_t stride;
} circle_buffer;

circle_buffer * circle_buffer_create(size_t size, size_t stride);
void circle_buffer_free(circle_buffer *buffer);
size_t circle_buffer_size(const circle_buffer *buffer);
size_t circle_buffer_capacity(const circle_buffer *buffer);
int circle_buffer_full(circle_buffer *buffer);
int circle_buffer_empty(circle_buffer *buffer);

const void * circle_buffer_head(circle_buffer *buffer);
const void * circle_buffer_tail(circle_buffer *buffer);
const void * circle_buffer_data(circle_buffer *buffer);

int circle_buffer_head_pos(circle_buffer *buffer);
int circle_buffer_tail_pos(circle_buffer *buffer);

void * circle_buffer_tail_read(circle_buffer *buffer, void *dest,
                               size_t count);

void circle_buffer_reset(circle_buffer *buffer);
size_t circle_buffer_bytes_free(const struct circle_buffer *buffer);
size_t circle_buffer_bytes_used(const struct circle_buffer *buffer);
size_t circle_buffer_memset(circle_buffer *dst, int c, size_t len);
void *circle_buffer_memcpy_into(circle_buffer *buffer, const void *src,
                                size_t count);
void *circle_buffer_memcpy_from(void *dest, circle_buffer *buffer,
                                size_t count);

#ifdef CIRCLE_INTERNAL_PROTOTYPES
data_t * circle_buffer_end(circle_buffer *buffer);
data_t * circle_buffer_nextp(circle_buffer *buffer, const data_t *p);
#endif

/* #endif */
