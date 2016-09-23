#define RING_USE_STDLIB_ALLOC 1
#include <ring/ring.cpp>

int main(void) {
  RingBuffer r(10, 1, OVERFLOW_OVERWRITE);
  return 0;
}
