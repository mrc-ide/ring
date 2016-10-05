#ifndef _RING_H_
#define _RING_H_
#include <stddef.h>
#include <stdbool.h>

#ifndef RING_USE_STDLIB_ALLOC
#ifndef USING_R
#define USING_R
#endif
#endif

// Allow use from C++
#ifdef __cplusplus
extern "C" {
#endif

// What to do on overflow.
//
// The OVERFLOW_ERROR action (which calls R's error function) is only
// available when using R, which is detected by the <R.h> header
// included.  If you are using RING_USE_STDLIB_ALLOC (see below) but
// want to use OVERFLOW_ERROR then you'll need to include <R.h> as
// well, and be willing to deal with an R error and the longjmp that
// it causes.
typedef enum overflow_action {
  OVERFLOW_OVERWRITE,
  OVERFLOW_GROW
#ifdef USING_R
  , OVERFLOW_ERROR
#endif
} overflow_action;

// The underlying data structure.  None of the fields here should be
// directly accessed in normal use; use the accessor functions
// instead.
//
// The ring buffer is a FIFO (first-in-first-out) queue.  It is
// implemented as a single block of memory (data) and a pair of
// pointers:
//
//   head: the starting location where data should be written when
//         copying data *into* the buffer.
//
//   tail: the starting location where data should be read when
//         copying data *from* the buffer.
//
// The buffer has a concept of a stride; the number of bytes per
// buffer entry.  This is fixed across the entire ring.  As such, some
// functions that return size_t have a booleanargument "bytes" that
// switches between measuring in bytes and measuring in logical
// elements.  In the case where stride=1, these are identical.
//
// In general, the ring buffer is totally happy to overflow; if you
// write too much into the ring buffer it will destructively erase
// data (i.e., your tail will move).  The ring buffer will never
// underflow, but functions may return `NULL` on underflow - read the
// documentation below carefully.
typedef unsigned char data_t;
typedef struct ring_buffer {
  size_t size;
  size_t stride;
  size_t bytes_data;
  overflow_action on_overflow;

  data_t *data;
  data_t *head;
  data_t *tail;
} ring_buffer;

//// Creation, deletion, etc: ////

// Create a ring buffer.  After creating, be sure to free the memory
// with `ring_buffer_destroy`.
//
//   size: (maximum) number of elements that the ring buffer may contain
//
//   stride: number of *bytes* per ring buffer element
//
// See the note above the struct for details on size/stride.
//
// If the buffer cannot be allocated (e.g., too big a buffer is
// requested) then an R error will be thrown as this uses `Calloc`.
//
// This may not always be desirable (e.g., if using from within C++,
// or in a project that does not actually use R).  To use plain C
// stdlib calloc/free, in the ring.c use:
//
//     #define RING_USE_STDLIB_ALLOC 1
//     #include <ring/ring.c>
//
// which will not depend on *any* R code and use stdlib calloc/free
// (except for the issue with USING_R/OVERFLOW_ERROR above).  With
// RING_USE_STDLIB_ALLOC defined, if an allocation fails, then
// ring_buffer_create (and ring_buffer_duplicate below) will return
// NULL.  So if using this approach be sure to check the return value!
//
// The main wrinkle to using RING_USE_STDLIB_ALLOC 1 is that the
// `overflow_action` `OVERFLOW_ERROR` will not work.  At present this
// will fail to compile, but in future I may add an error handler.
ring_buffer * ring_buffer_create(size_t size, size_t stride,
                                 overflow_action on_overflow);

// Destroy a ring buffer.  Frees the memory
//
//   buffer: the ring buffer to copy; after calling this function all
//           memory associated with the buffer is freed.
void ring_buffer_destroy(ring_buffer *buffer);

// Duplicate (copy) a ring buffer.  Copies both the underlying data and
// the position of the head and tail.  A new buffer will be allocated
// and must be freed when finished with, using `ring_buffer_destroy`
//
//   buffer: a ring buffer to copy from; will not be modified
ring_buffer * ring_buffer_duplicate(const ring_buffer *buffer);

// Increase the size of the ring buffer so that it can hold additional
// elements.  This does not alter existing elements but increases the
// capacity (similar to he `reserve` method in the C++ standard
// library).
//
//   buffer: a ring buffer to increase the size of
//
//   n: the number of elements to increase the buffer by
//
//   exact: boolean, indicating if the buffer should be increased by
//          exactly `n` elements (if true) or by at least `n` elements
//          (if false).  If using the inexact method, the buffer is
//          increased in size using geometric growth using the golden
//          ratio.
//
// After using this function, all references to the head or tail are
// broken and the memory may have been freed and the contents moved
// elsewhere.
//
// If RING_USE_STDLIB_ALLOC is defined, and if an allocation fails,
// then this may leave things in an undesirable state (this is
// particularly a problem when using on_overflow = OVERFLOW_GROW).
// Currently, if R is used an R error will be thrown (possibly not a
// good idea if running under Rcpp) and if running as a standalone
// application then the data will be set to NULL, probably causing a
// crash pretty quickly (improvements welcome).
void ring_buffer_grow(ring_buffer *buffer, size_t n, bool exact);

// Reset the state of the buffer.  This "zeros" the head and tail
// pointer (and may or may not actually reset the data) so that the
// buffer can be used as if fresh.
//
//   buffer: a ring buffer to reset
//
//   clear: boolean, indicating if memory should also be zeroed
void ring_buffer_reset(ring_buffer *buffer, bool clear);

//// Basic querying: ////

// Return the maximum size of the ring buffer
//
//   buffer: the ring buffer to test (will not be modified)
//
//   bytes: indicates if size should be in bytes (if true) or elements
//          (if false)
size_t ring_buffer_size(const ring_buffer *buffer, bool bytes);

// Report the free and used space in the ring buffer
//
//   buffer: the ring buffer to test (will not be modified)
//
//   bytes: indicates if used/free space should be in bytes (if true)
//          or elements (if false)
size_t ring_buffer_free(const ring_buffer *buffer, bool bytes);
size_t ring_buffer_used(const ring_buffer *buffer, bool bytes);

// Report the number of bytes of data that have been allocated.  Note
// that this is likely `stride` more bytes than was requested as this
// avoids a lot of awkward bookkeeping later, allowing the "full"
// state to be distinguished from the "empty" state.
size_t ring_buffer_bytes_data(const ring_buffer *buffer);

// Report if the ring buffer is full or empty
bool ring_buffer_is_full(const ring_buffer *buffer);
bool ring_buffer_is_empty(const ring_buffer *buffer);

//// Additional querying: ////

// Return the position of the head and tail pointers relative to the
// data pointer (this is an offset, so 0 means the pointer is at the
// start of the data array).
//
//   bytes: indicates if offset should be bytes (if true) or elements (if false)
size_t ring_buffer_head_pos(const ring_buffer *buffer, bool bytes);
size_t ring_buffer_tail_pos(const ring_buffer *buffer, bool bytes);

// Return pointers to the the data, head and tail members of the ring
// buffer.  These are preferred over directly accessing the "data",
// "head" and "tail" elements of the ring buffer structure itself
// because with these the compiler will enforce read-only access for
// you.
//
// WARNING: the head buffer is *not* the most recently added element,
// but instead the bit of memory that will be written to next; it's
// generally not terribly useful and a better way of getting the last
// written element is to use:
//
//   ring_buffer_head_offset(buffer, 0);
//
// which will look after wrapping the ring buffer appropriately.
const void * ring_buffer_data(const ring_buffer *buffer);
const void * ring_buffer_head(const ring_buffer *buffer);
const void * ring_buffer_tail(const ring_buffer *buffer);

//// Setting repeated values: ////

// Set all bytes of a length of the buffer to 'c'.  Here, 'len' is the
// number of *entries*, so stride * len bytes will be set.  This will
// mostly be uesful with c=0.
//
//   buffer: the ring buffer to set data into
//
//   c: value (0-255) to set all bytes to
//
//   n: number of elements to set
//
// This starts adding data at `head`.  If the buffer will overflow, at
// most `bytes_data` bytes will be written (i.e., each element will be
// written to once).
//
// Returns the number of bytes actually written to the buffer (so if
// the buffer overflows this may be less than `len`).
size_t ring_buffer_set(ring_buffer *buffer, data_t c, size_t n);

// Set a number of the elements of the buffer to a particular byte
// pattern.  In contrast with `ring_buffer_set`, this does not set
// individual bytes, but instead complete elements.
//
//    buffer: the ring buffer to set data into
//
//    x: pointer to a set of data to copy into the ring buffer.  This
//            must be (at least) stride bytes long.
//
//    n: number of elements to set
//
// This starts adding data at `head`.  If the buffer will overflow, at
// most `bytes_data` bytes will be written (i.e., each element will be
// written to once).
size_t ring_buffer_set_stride(ring_buffer *buffer, const void *x, size_t n);

//// Read and write ////

// Copy `n` entries, each of `stride` bytes from a contiguous memory
// area src into the ring `buffer`. Returns the ring buffer's new head
// pointer.
//
// It is possible to overflow the buffer with this function
//
//   buffer: the ring buffer to copy data into
//
//   src: the source memory to copy from (make sure this is big enough
//           or you will get crashes and other terrible things).
//
//   n: the number of entries to copy from `src` into `buffer` (each
//           of which is `stride` bytes long).
const void * ring_buffer_push(ring_buffer *buffer, const void *src, size_t n);

// Destructively copy `n` entries (each of which is `stride` bytes)
// from a ring buffer `buffer` into contiguous memory region `dest`.
// This updates the `tail` pointers in the ring buffer and returns the
// new tail pointer.
//
// The `n` entries will no longer be available in the ring buffer.
// To do a nondestructive read, use `ring_buffer_read()`.
//
//   buffer: the ring buffer to copy data from
//
//   dest: the destination memory to copy into (make sure this is big enough
//           or you will get crashes and other terrible things).
//
//   n: the number of entries to copy from `src` into `buffer` (each
//           of which is `stride` bytes long).
//
// This function will not allow the ring buffer to underflow.  If
// `n` is greater than the number of available entries, then
// nothing is copied (and the ring buffer remains unmodified) and NULL
// is returned.
const void * ring_buffer_take(ring_buffer *buffer, void *dest, size_t n);

// Nondestructively read from a ring buffer.  This function is
// essentially identical to `ring_buffer_take` but does not alter the
// tail pointer.
const void * ring_buffer_read(const ring_buffer *buffer, void *dest, size_t n);

// ring_buffer_take_head and ring_buffer_read_head are like
// ring_buffer_take and ring_buffer_read (respectively) but operate on
// the *head* of the ring (i.e., removing the most recently added
// elements rather than the oldest elements).
//
// Neither will underflow, returning NULL if there are not enough
// elements, and without copying anything.
const void * ring_buffer_take_head(ring_buffer *buffer, void *dest, size_t n);
const void * ring_buffer_read_head(const ring_buffer *buffer, void *dest,
                                   size_t n);

// Copy `n` entries (each of `stride` bytes) from one ring buffer
// `src` into another `dest`.  The copy starts at the tail of this
// ring buffer, pushing onto the head of the destination buffer.
//
//   src: A ring buffer to copy data from

//   dest: A ring buffer to copy data into
//
//   n: the number of entries to copy (each of which is `stride` bytes)
//
// This is destructive to both buffers as pointers will be updated in
// both.
//
// This function returns the new head pointer of the destination buffer.
//
// It is not possible to underflow `src`; if too few entries are
// available, then nothing is copied, `src` and `dest` are not
// modified, and the function returns NULL
//
// It is possible to overflow `dest` and the tail pointer will be
// updated appropriately if so.
//
// Warning: the two buffers must have the same stride.  If the buffers
// do not have the same stride, the function will return NULL (this
// means if the function returns NULL it could either be an underflow
// or an incompatible buffer).
const void * ring_buffer_copy(ring_buffer *src, ring_buffer *dest, size_t n);

// Mirror the contents of ring buffer `src` into ring buffer `dest`.
// This differs from `ring_buffer_copy` in that the `src` buffer is
// not modified and that the *entire* state of the ring buffer is
// duplicated.
//
// The function requires (and checks) that `src` and `dest` agree on
// size and stride (and therefore total bytes).  It returns `true` if
// the mirror was done, and `false` if the buffers are incompatible.
//
// This function will destroy all data in `dest`, but not allocate any
// memory.
//
// Warning: the two buffers must have the same stride *and* the same
// size.  If they do not, the function will return NULL (this means if
// the function returns NULL it could either be an underflow or an
// incompatible buffer).
bool ring_buffer_mirror(const ring_buffer *src, ring_buffer *dest);

// Returns a pointer to the tail (reading end) of the buffer, offset
// by `offset` entries.  When used as `ring_buffer_tail_offset(x, 0)`
// this is equivalent to `ring_buffer_tail(x)` except that it will do
// underflow checking.
//
//   buffer: the ring buffer to use
//
//   offset: the number of entries (each of which are `stride` bytes)
//           to offset by
//
// It is not possible to underflow the buffer here; if `offset` is so
// large that it would underflow, then NULL will be returned.
const void * ring_buffer_tail_offset(const ring_buffer *buffer, size_t offset);

// As for `ring_buffer_tail_offset`, but offsetting the *head*
// pointer.  This offsets in the opposite direction (moving from the
// most recently added element towards the oldest element).
const void * ring_buffer_head_offset(const ring_buffer *buffer, size_t offset);

//// For advanced use: ////

// Advance the ring buffer by one entry and return a pointer to the
// memory *without writing anything to it*.  In this case, the calling
// function is responsible for setting the memory to something
// sensible.  This is currently used in the dde package where we want
// to write directly to the head.
//
// This is (roughly) equivalent to:
//
//    ring_buffer_set(buffer, 0, 1);
//    return buffer->head;
//
// but does not actually copy any data.
//
// Note that the pointer returned is *not* const; this is always used
// in a case where the aim is to write to the head directly!
void * ring_buffer_head_advance(ring_buffer *buffer);

//// Search: ////

// There are two functions for searching for data within a ring buffer
// that consists of *sorted* entries.  This might be the case if
// entries are added sequentially with (say) a timestamp.
//
// To locate an entry, a predicate function (pointer) must be
// provided.  This must be a function taking two void pointers as
// arguments; the first will be the pointer to an entry in the ring
// buffer, the second will be any data that *you* provide (may be
// NULL).  This function must return "true" if the value is *less
// than* the target value (i.e. true if we should search *earlier* in
// the buffer).  The "x" argument must be treated as read-only.
//
// For example, a predictate function that would find an entry where
// the first 8 bytes of a ring buffer entry represent doubles could be
// written as:
//
//     bool test_find_double(const void *x, void *data) {
//       double x_value = *((double*) x);
//       double data_value = *((double*) data);
//       return x_value <= data_value;
//     }
//
// Where the "data" argument will be passed through as the number to
// search for.
//
// These functions return NULL if no entry is found, otherwise they
// return the pointer to the largest entry in the buffer that the
// predicate returns false.
//
// The _linear search does a naive linear search from the tail of the
// buffer (i.e., the last entry that was added) towards the beginning.
//
// The _bisect search tries to be more clever and does a bisect
// search.  It requires an initial guess "i" to the location of the
// data.  You can provide '0' as 'i' to start at the tail.
//
// The "data" argument to both functions will be passed through to the
// predicate function.
typedef bool ring_predicate(const void *x, void *data);
const void * ring_buffer_search_linear(const ring_buffer *buffer,
                                       ring_predicate pred, void *data);
const void * ring_buffer_search_bisect(const ring_buffer *buffer, size_t i,
                                       ring_predicate pred, void *data);

#ifdef __cplusplus
}
#endif
#endif
