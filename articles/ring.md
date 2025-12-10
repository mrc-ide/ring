# ring

This package implements [ring
buffers](https://en.wikipedia.org/wiki/Ring_buffer). A ring buffer can
be used as a first-in-first-out (FIFO) buffer where the maximum size is
known ahead of time. Because they do not grow in size, they are useful
to avoid using more and more memory as a process runs. Because the data
reading and writing happens in an (apparently) circular way, once data
is added to the buffer it is not copied (in contrast if you used a
vector then every time data is consumed you’d have to shuffle the vector
around).

`ring` implements two different ring buffers that will likely suit
different applications.

- `ring_buffer_bytes` is implemented as a byte array in C, possibly with
  a “stride” indicating a set of bytes that go together. Once the data
  reaches the end of the array we start writing to the beginning again.
- `ring_buffer_env` is implemented as a doubly linked list using R’s
  environments. This buffer can hold arbitrary R objects at each
  position.

The target audience for this package is either other package developers
who need a ring buffer in their package, or modellers who have decided
that a ring buffer is the right data structure to help with their
simulation model.

For all buffers, `head` will refer to the next bit of the buffer to be
written to, and `tail` will refer to the next bit of the buffer to be
read. That is, elements are pushed onto the `head` of the buffer and
retrieved from the `tail`. (There is no direct analogy between these
terms and the R functions `head` and `tail` which operate on fixed-size
vectors.)

## The environment buffer `ring_buffer_env`

This is the simplest buffer to understand because we don’t have to deal
with raw vectors.

To create a buffer that can hold up to 100 elements, use the
`ring_buffer_env` function:

``` r
buf <- ring::ring_buffer_env(100)
```

This is an [R6](https://github.com/r-lib/R6) class, with several
methods:

``` r
buf
```

    ## <Ring Buffer (ring_buffer_env)>
    ##   Public:
    ##     copy: function (dest, n)
    ##     duplicate: function ()
    ##     free: function ()
    ##     grow: function (n)
    ##     head: function ()
    ##     head_advance: function ()
    ##     head_data: function ()
    ##     head_offset: function (n)
    ##     head_pos: function ()
    ##     head_set: function (data)
    ##     is_empty: function ()
    ##     is_full: function ()
    ##     mirror: function (dest)
    ##     push: function (data, iterate = TRUE)
    ##     read: function (n)
    ##     read_head: function (n)
    ##     reset: function (clear = FALSE)
    ##     set: function (data, n)
    ##     size: function ()
    ##     tail: function ()
    ##     tail_offset: function (n)
    ##     tail_pos: function ()
    ##     take: function (n)
    ##     take_head: function (n)
    ##     used: function ()

Operations on the class happen by running methods using `$`. So the size
of the buffer:

``` r
buf$size()
```

    ## [1] 100

…the number of elements free and used:

``` r
buf$free()
```

    ## [1] 100

``` r
buf$used()
```

    ## [1] 0

…whether the buffer is empty or full:

``` r
buf$is_empty()
```

    ## [1] TRUE

``` r
buf$is_full()
```

    ## [1] FALSE

To start using the buffer we need to put some data in it. There are two
main functions for adding data:

- `buf$set(data, n)` sets `n` elements to be the value `data`
- `buf$push(data, iterate)` pushes `data` into the buffer, with the
  `iterate` argument indicating if we should iterate over `data` or
  treat it as a single element

So to set the first 5 elements to be “a”, “b”, …, “e”, use:

``` r
buf$push(letters[1:5])
```

The buffer is no longer empty

``` r
buf$is_empty()
```

    ## [1] FALSE

…having 5 elements:

``` r
buf$used()
```

    ## [1] 5

…and room for 95 more:

``` r
buf$free()
```

    ## [1] 95

To read the content of the buffer without modifying it, use `read(n)`
where `n` is the number of elements to read. This *always* returns a
list of length `n`:

``` r
buf$read(1)
```

    ## [[1]]
    ## [1] "a"

``` r
buf$read(2)
```

    ## [[1]]
    ## [1] "a"
    ## 
    ## [[2]]
    ## [1] "b"

If you try to read too far, then the buffer will underflow and you will
get an error:

``` r
buf$read(20)
```

    ## Error: Buffer underflow (requested 20 elements but 5 available)

If you just want the the first element, use
[`tail()`](https://rdrr.io/r/utils/head.html)

``` r
buf$tail()
```

    ## [1] "a"

The tail returns the first element in (so the buffer naturally operates
as a first-in-first-out queue).

You can also read the most recently added element with
[`head()`](https://rdrr.io/r/utils/head.html)

``` r
buf$head()
```

    ## [1] "e"

And you can offset these by an integer number of steps. So moving one
position into the buffer from the tail gets the second element added:

``` r
buf$tail_offset(1)
```

    ## [1] "b"

or moving three elements into the buffer from the head (most recently
added element) gets the same bit of data

``` r
buf$head_offset(3)
```

    ## [1] "b"

The above operations are all nondestructive – they leave the buffer
unchanged. To consume elements, use `take(n)` which operates the same
way as `read` but it also moves the buffer `tail`; it consumes elements
leaving space for more.

``` r
buf$free()
```

    ## [1] 95

``` r
buf$take(1)
```

    ## [[1]]
    ## [1] "a"

``` r
buf$free()
```

    ## [1] 96

Now we have consumed an element the tail has moved along, so `tail`
contains “b” and “a” is removed from the buffer:

``` r
buf$tail()
```

    ## [1] "b"

To reset the buffer, use `reset()`. This empties the buffer of all data:

``` r
buf$reset()
buf$used()
```

    ## [1] 0

``` r
buf$is_empty()
```

    ## [1] TRUE

While the ring buffer is fixed in size in typical use, you can grow it
explicitly. To add additional space, use the `grow` method:

``` r
buf$size()
```

    ## [1] 100

``` r
buf$grow(20)
buf$size()
```

    ## [1] 120

### Application: simulation with recent history

The whole point of the ring buffer though is that we can push things
onto it and pull the most recent out, even when the number of things
pushed *overall* is larger than the buffer size.

So imagine a simulation where we need to keep track of the last 5 steps.
The simulation is a random walk.

``` r
step <- function(x) {
  if (runif(1) < 0.5) x - 1L else x + 1L
}

x <- 0L
buf <- ring::ring_buffer_env(5)
h <- integer(20)
buf$push(x)
h[1L] <- x

set.seed(1)
for (i in seq_len(length(h) - 1L)) {
  x <- step(x)
  buf$push(x)
  h[i + 1L] <- x
}
```

The whole history:

``` r
h
```

    ##  [1]  0 -1 -2 -1  0 -1  0  1  2  3  2  1  0  1  0  1  0  1  2  1

The last 5 steps:

``` r
unlist(buf$read(5))
```

    ## [1] 1 0 1 2 1

So we could rewrite the simulation so that the random walk tends up if
the last few steps have been increases and tends down if the last few
steps have been decreases:

``` r
step <- function(x) {
  if (length(x) > 1) {
    p <- mean(diff(x)) / 2 + 0.5
  } else {
    p <- 0.5
  }
  if (runif(1) < p) x[length(x)] - 1L else x[length(x)] + 1L
}

x <- 0L
buf <- ring::ring_buffer_env(5)
h <- integer(100)
buf$push(x)
h[1L] <- x

set.seed(1)
for (i in seq_len(length(h) - 1L)) {
  x <- step(unlist(buf$read(buf$used())))
  buf$push(x)
  h[i + 1L] <- x
}
```

Now we have a simulation with a strong mean reverting tendency:

``` r
par(mar = c(4, 4, .5, .5))
plot(h, type = "l", xlab = "step", ylab = "y", las = 1)
```

![](ring_files/figure-html/unnamed-chunk-25-1.png)

Because the buffer always holds the last 5 (or fewer) elements the
book-keeping involved in working with the last few elements out is
simplified. Ignoring the fact that we hold the entire history in the
fixed size vector `h`, only the last few elements need to be retained
which may be useful if the simulation generates a lot of data.

A downside of this implementation is that `buf$read()` returns a list
that must be turned into a vector with `unlist`, even though we know in
this case that the simulation will always produce an integer vector. The
ring buffers described below can help with that problem.

## The bytes buffer `ring_buffer_bytes`

This is the classical implementation of a ring buffer, and the
implementation is broadly based on the one
[here](https://github.com/dhess/c-ringbuf), by
[@dhess](https://github.com/dhess).

This operates basically the same way as `ring_buffer_env`, and presents
a very similar interface to R, but with a few key differences:

- The contents of the buffer are raw bytes (using R’s raw vectors).
  These are a bit fiddly to work with but can be very powerful.
- The `iterate` distinction of `push` disappears because there is no
  ambiguity with R objects

To construct a buffer of 1000 bytes:

``` r
buf <- ring::ring_buffer_bytes(1000)
```

Most of the same methods apply directly:

``` r
buf$free()
```

    ## [1] 1000

``` r
buf$used()
```

    ## [1] 0

``` r
buf$is_full()
```

    ## [1] FALSE

``` r
buf$is_empty()
```

    ## [1] TRUE

Generate a byte sequence:

``` r
bytes <- as.raw(0:255)
```

…and push them into the buffer:

``` r
buf$push(bytes)
```

…read from the buffer

``` r
buf$read(10)
```

    ##  [1] 00 01 02 03 04 05 06 07 08 09

…destructively take the oldest elements

``` r
buf$used()
```

    ## [1] 256

``` r
buf$take(20)
```

    ##  [1] 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13

``` r
buf$used()
```

    ## [1] 236

### Striding

Single bytes can hold only values 0 to 255 (or character equivalents,
such as `a` becomes 61 via `charToRaw("a")`. But if you want to hold a
full integer, that (usually) takes 4 bytes, a double (usually) takes 8.

To allow this, a bytes buffer can be “strided”; this indicates the
number of consecutive bytes that should together make up one logical
entry. The buffer then contains `size` of these. So to create a buffer
of 100 entries, each of `8` bytes you could do:

``` r
buf <- ring::ring_buffer_bytes(100, 8)
```

Each element pushed onto the buffer must have the correct size. So to
push the byte sequence 1..8 onto the buffer:

``` r
buf$push(as.raw(1:8))
```

but if you pushed more or less it would be an error:

``` r
buf$push(as.raw(1:4))
```

    ## Error in buf$push(as.raw(1:4)): Incorrect size data; expected multiple of 8 bytes

Reading happens in *logical* units, not bytes:

``` r
buf$read(1)
```

    ## [1] 01 02 03 04 05 06 07 08

and you can get the number of elements used:

``` r
buf$used()
```

    ## [1] 1

or the number of bytes

``` r
buf$used(bytes = TRUE)
```

    ## [1] 8

### The typed bytes buffer `ring_buffer_bytes_typed`

If 8 bytes is a double, it should be possible to make a bytes buffer
that holds one (or more) doubles per entry. That is what the
`ring_buffer_bytes_typed` buffer does, with a few corner cases dealt
with. To use, you decide what the *R interpretation* of an entry is, it
will determine the size per entry and appropriate encoding and decoding
functions and you can ignore that it is storing bytes. For performance
reasons this does not use R’s serialisation and simply copies the data
stored in vectors.

For example, to make a buffer of 10 elements, each of which is a single
real number (double), use:

``` r
buf <- ring::ring_buffer_bytes_typed(10, double(1))
```

onto which real numbers can be pushed:

``` r
buf$push(pi)
```

And retrieve the data.

``` r
buf$take(1)
```

    ## [1] 3.141593

Entries can contain more than one number; to make a buffer of length 10,
each element of which is a vector of 5 doubles:

``` r
buf <- ring::ring_buffer_bytes_typed(10, double(5))
buf$push(rnorm(5))
buf$read(1)
```

    ## [1]  0.26613736 -0.37670272  2.44136463 -0.79533912 -0.05487747

Because this is just implemented as a byte array, we can just push a
bunch of numbers straight into the buffer:

``` r
buf$push(rnorm(5 * 10))
```

With elements in the buffer, we can request them. The integer argument
of `take` indicates the number of groups of 5 doubles we would like
back:

``` r
buf$take(1)
```

    ## [1]  0.2501413  0.6182433 -0.1726235 -2.2239003 -1.2636144

If you try to take more than is in the buffer it is an error:

``` r
buf$take(10)
```

    ## Error in super$take(n): Buffer underflow (requested 10 elements but 9 available)

### The translating bytes buffer `ring_buffer_bytes_translate`

The `ring_buffer_bytes_typed` function is implemented by translating R
objects to bytes (when storing with `$set()`, `$push()`, etc). and from
bytes back to R objects (when retrieving with `$read()`, `$take()`,
etc). `ring_buffer_bytes_translate` exposes this interface.

The “typed” buffers do not allow storing strings because they can be any
number of bytes long (the bytes buffers require a fixed “stride” within
a buffer). But we can store fixed length strings.

To convert a string to a byte sequence, use `charToRaw` (or
`as.raw(utf8ToInt(x))`, but then multi-byte sequences might start being
difficult).

``` r
(bytes <- charToRaw("hello world"))
```

    ##  [1] 68 65 6c 6c 6f 20 77 6f 72 6c 64

The inverse transformation is `rawToChar` (or
`intToUtf8(as.integer(x))`):

``` r
rawToChar(bytes)
```

    ## [1] "hello world"

The function `ring_buffer_bytes_translate` takes these functions as its
3rd and fourth arguments. So to make a buffer that will hold up to 100
strings, each of 8 bytes:

``` r
b <- ring::ring_buffer_bytes_translate(100, 8, charToRaw, rawToChar)
```

We can now store 8 character strings:

``` r
b$push("abcdefgh")
b$tail()
```

    ## [1] "abcdefgh"

But other length strings cannot be added:

``` r
b$push("hello!")
```

    ## Error in super$push(self$.to(data)): Incorrect size data; expected multiple of 8 bytes

Probably this would be most useful storing just single characters as
then it would make a buffer of *text*.

## The C API

The `ring` package can be used in other R packages using the `LinkingTo`
mechanism. To do so:

- In your `DESCRIPTION`, add a line `LinkingTo: ring` (you do not need
  to include `ring` in `Depends` or `Imports` as we need it only for the
  package build).

- In your `src/` directory, add a file `ring.c` containing just the line
  `#include <ring/ring.c>` (but see the note in the documentation for
  `ring_buffer_create` below).

- Anywhere in your code you want to use the ring buffer, include the
  line `#include <dde/dde.h>` to include the prototypes and use the
  interface as described below.

(I am not sure what the best practice way of doing this with a
standalone shared library compiled with `R CMD SHLIB` is though;
probably best to make a package.)

The C API is documented only in the header file, and it should be fairly
straightforward to use (with reference to the docs above; this is the
code underlying the `ring_buffer_bytes` interface).

``` c
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
```

For a complete real-world example of use, see
[dde](https://github.com/mrc-ide/dde), which uses a ring buffer to hold
the history of a set of differential equations, and uses that to
implement delay equations. Here, the ring buffer means that the memory
requirements don’t grow with the length of running the simulation (as it
only cares about fairly recent history, the natural overflow from the
ring buffer is well suited). The memory is only allocated at the
beginning of the simulation so there is no additional memory
allocations. And because `ring` returns (const) pointers to the
appropriate place in memory there is little copying.

A simple application that implements the same mean-reverting simulation
from above:

``` c
#include <ring/ring.h>

#include <R.h>
#include <Rinternals.h>

// Definition used below (can't be called step() because that may
// conflict with regexp.h on some platforms).
int step_x(ring_buffer *r, int x);

void example(size_t nstep, double *ret) {
  // Construct a ring buffer of (max) size 5, each element of which is
  // big enough to contain an integer (probably 4 bytes).
  ring_buffer *r = ring_buffer_create(5, sizeof(int), OVERFLOW_OVERWRITE);

  // Starting point of the simulation, as in the R version:
  int x = 0;

  // Push the initial state into the ring buffer:
  ring_buffer_push(r, &x, 1);
  ret[0] = x;

  for (size_t i = 1; i < nstep; ++i) {
    x = step_x(r, x);
    ring_buffer_push(r, &x, 1);
    ret[i] = x;
  }

  // Cleanup:
  ring_buffer_destroy(r);
}

int step_x(ring_buffer *r, int x) {
  size_t n = ring_buffer_used(r, false);
  double p;
  if (n < 2) {
    p = 0.5;
  } else {
    // Oldest non-overflowed element is in the tail.  Note that the
    // return value (which is void*) must be first cast to (int*) then
    // dereferenced.
    ///
    // NOTE: In general, check that the return value here is not NULL
    // (indicating an underflow); here we're OK because we checked the
    // number of used elements at the beginning.
    int x0 = *(int*)ring_buffer_tail(r);
    size_t increases = 0;
    for (size_t i = 1; i < n; ++i) {
      // Moving through the more recently added elements:
      int x1 = *(int*)ring_buffer_tail_offset(r, i);
      if (x1 > x0) {
        increases++;
      }
      x0 = x1;
    }
    p = ((double)increases) / (n - 1);
  }

  if (unif_rand() < p) {
    --x;
  } else {
    ++x;
  }

  return x;
}

// This function collects all the R API bits that deal with
// communication between C and R.  "Writing R extensions" is the
// canonical documentation source.
SEXP r_example(SEXP r_nstep) {
  size_t nstep = (size_t) INTEGER(r_nstep)[0];
  SEXP ret = PROTECT(allocVector(REALSXP, nstep));
  GetRNGstate(); // because we'll work with random numbers
  example(nstep, REAL(ret));
  // Cleanup:
  PutRNGstate();
  UNPROTECT(1);
  return ret;
}

// This can be included in a different file, or, for a single file
// project like this one, include here.
#include <ring/ring.c>
```

### A nontrivial example

In the [`dde`](https://github.com/mrc-ide/dde) package (not yet on
CRAN), I use ring buffers to solve delay differential equations (DDEs).
To solve these, we need to know the state of the system at a series of
points in the past. So at every time step we push the state of the
system onto a ring buffer. Then, as the solver moves forward in time we
can get the system at some previous point in time by looking back
through the ring buffer until the time in question is found.

In this application a ring buffer is the ideal data structure because we
often want to solve equations where the time we look back is a small
fraction of the total time. Without a ring buffer we’d either have to
store the *entire* history (with a large memory cost, most of which is
not needed) or periodically copy the history around.

To use `ring` within the `dde` package:

- In the `DESCRIPTION` we [declare a link to
  `ring`](https://github.com/mrc-ide/dde/blob/7ebaefd/DESCRIPTION#L14)
  using the `LinkingTo:` field.

- In the `src` directory, [the contents of `<ring/ring.c>` are
  included](https://github.com/mrc-ide/dde/blob/7ebaefd/src/ring.c);
  this is possible because of the `LinkingTo` field. This file now
  includes all the actual ring buffer implementation.

- In
  [src/dopri.h](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.h#L8)
  we include `<ring/ring.h>` which allows the ring buffer code to be
  used in any file that includes `dopri.h`. There is a [data structure
  in this
  header](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.h#L77-L111)
  that includes within itself a ring buffer to hold the history.

- In
  [src/dopri.c](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.c)
  the ring buffer code is actually used:

  - [initialisation](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.c#L48-L50)
  - [a time is found within the ring
    buffer](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.c#L694-L719)
  - [the ring buffer is
    advanced](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.c#L417)
  - [the data is
    freed](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri.c#L220)

- In
  [src/dopri_5.c](https://github.com/mrc-ide/dde/blob/7ebaefd/src/dopri_5.c#L109-L119)
  new data is written to the head of the ring buffer, being the state of
  the system at the end of the step. The history head is treated as a
  big block of contiguous doubles.

Used this way, the programmer can focus on simply writing to the
application and do as little work on bookkeeping as possible.

## The C++ API

If you’re using C++ you may find the [Boost circular
buffer](https://www.boost.org/doc/libs/1_61_0/doc/html/circular_buffer.html)
is likely to be far better; you can use this by `LinkingTo:` the `BH`
package and using `#include <boost/circular_buffer.hpp>` in your code.

Alternatively, the `ring` C code can be directly used in C++ as above.
Or, there is a class-based approach available:

- In your `src/` directory, add a file `ring.cpp` containing just the
  line `#include <ring/ring.cpp>`

- Anywhere in your code you want to use the ring buffer, include the
  line `#include <dde/dde.hpp>` to include the class definition:

``` cpp
#ifndef _RING_HPP_
#define _RING_HPP_

// NOTE: the C++ version always uses non-R memory allocation functions
// because otherwise the R error can jump over destructors causing
// memory leaks or worse.  Errors will be thrown with "throw", which
// Rcpp will catch if you use that.
//
// However, note that if using overflow_action of anything other than
// OVERFLOW_OVERWRITE is possibly unsafe; OVERFLOW_GROW is fine so
// long as you never run out of memory, and OVERFLOW_ERROR is probably
// never safe.
#define RING_USE_STDLIB_ALLOC 1
#include <ring/ring.h>

class RingBuffer {
  ring_buffer * buffer;
public:
  RingBuffer(size_t size, size_t stride, overflow_action on_overflow);
  ~RingBuffer();
  RingBuffer(const RingBuffer& other);
  RingBuffer& operator=(RingBuffer other);

  void grow(size_t n, bool exact);
  void reset(bool clear);
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
  bool mirror(RingBuffer& dest) const;
  const void * tail_offset(size_t offset) const;
  const void * head_offset(size_t offset) const;
};

#endif
```
