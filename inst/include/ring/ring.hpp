#ifndef _RING_HPP_
#define _RING_HPP_

#include <ring/ring.h>

class RingBuffer {
  ring_buffer * buffer;
public:
  RingBuffer(size_t size, size_t stride);
  ~RingBuffer();
  RingBuffer(const RingBuffer& other);
  RingBuffer& operator=(RingBuffer other);

  void reset();
  size_t size(bool bytes) const;
  size_t free(bool bytes) const;
  size_t used(bool bytes) const;
  size_t bytes_data() const;
  bool is_full() const;
  bool is_empty() const;
  const void * data() const;
  const void * head() const;
  const void * tail() const;
  size_t head_pos(bool bytes) const;
  size_t tail_pos(bool bytes) const;
  size_t set(data_t c, size_t len);
  size_t set_stride(const void *x, size_t len);
  const void * push(const void *src, size_t n);
  const void * take(void *dest, size_t n);
  const void * read(void *dest, size_t n) const;
  const void * copy(RingBuffer& dest, size_t n);
  const void * tail_offset(size_t offset) const;
  const void * head_offset(size_t offset) const;
};

#endif
