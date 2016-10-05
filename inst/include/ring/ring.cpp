#include <ring/ring.hpp>
#include <algorithm>
#include <new>
extern "C" {
#include <ring/ring.c>
}

RingBuffer::RingBuffer(size_t size, size_t stride,
                       overflow_action on_overflow) :
  buffer(ring_buffer_create(size, stride, on_overflow)) {
  if (buffer == NULL) {
    throw std::bad_alloc();
  }
}

RingBuffer::~RingBuffer() {
  ring_buffer_destroy(buffer);
}

RingBuffer::RingBuffer(const RingBuffer& other) :
  buffer(ring_buffer_duplicate(other.buffer)) {
  if (buffer == NULL) {
    throw std::bad_alloc();
  }
}
RingBuffer& RingBuffer::operator=(RingBuffer other) {
  std::swap(buffer, other.buffer);
  return *this;
}

void RingBuffer::grow(size_t n, bool exact) {
  ring_buffer_grow(buffer, n, exact);
}

void RingBuffer::reset(bool clear) {
  ring_buffer_reset(buffer, clear);
}
size_t RingBuffer::size(bool bytes) const {
  return ring_buffer_size(buffer, bytes);
}
size_t RingBuffer::free(bool bytes) const {
  return ring_buffer_free(buffer, bytes);
}
size_t RingBuffer::used(bool bytes) const {
  return ring_buffer_used(buffer, bytes);
}
size_t RingBuffer::bytes_data() const {
  return ring_buffer_bytes_data(buffer);
}
bool RingBuffer::is_full() const {
  return ring_buffer_is_full(buffer);
}
bool RingBuffer::is_empty() const {
  return ring_buffer_is_empty(buffer);
}
const void * RingBuffer::data() const {
  return ring_buffer_data(buffer);
}
const void * RingBuffer::head() const {
  return ring_buffer_head(buffer);
}
const void * RingBuffer::tail() const {
  return ring_buffer_tail(buffer);
}

size_t RingBuffer::head_pos(bool bytes) const {
  return ring_buffer_head_pos(buffer, bytes);
}
size_t RingBuffer::tail_pos(bool bytes) const {
  return ring_buffer_tail_pos(buffer, bytes);
}

size_t RingBuffer::set(data_t c, size_t len) {
  return ring_buffer_set(buffer, c, len);
}
size_t RingBuffer::set_stride(const void *x, size_t len) {
  return ring_buffer_set_stride(buffer, x, len);
}
const void * RingBuffer::push(const void *src, size_t n) {
  return ring_buffer_push(buffer, src, n);
}
const void * RingBuffer::take(void *dest, size_t n) {
  return ring_buffer_take(buffer, dest, n);
}
const void * RingBuffer::read(void *dest, size_t n) const {
  return ring_buffer_read(buffer, dest, n);
}
const void * RingBuffer::copy(RingBuffer& dest, size_t n) {
  return ring_buffer_copy(buffer, dest.buffer, n);
}
bool RingBuffer::mirror(RingBuffer& dest) const {
  return ring_buffer_mirror(buffer, dest.buffer);
}
const void * RingBuffer::tail_offset(size_t offset) const {
  return ring_buffer_tail_offset(buffer, offset);
}
const void * RingBuffer::head_offset(size_t offset) const {
  return ring_buffer_head_offset(buffer, offset);
}
