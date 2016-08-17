#define RING_USE_STDLIB_ALLOC 1
#include <ring/ring.c>

int main(void) {
  ring_buffer * r = ring_buffer_create(10, 1);
  ring_buffer_destroy(r);
  return 0;
}
