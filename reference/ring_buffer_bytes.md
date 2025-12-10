# Byte array based ring buffer

Construct a ring buffer where the buffer holds a stream of bytes.
Optionally, the buffer can be "strided" so that the bytes naturally fall
into chunks of exactly the same size. It is implemented in C in the hope
that it will be fast, with the limitation that any data transfer to or
from R will always involve copies.

## Usage

``` r
ring_buffer_bytes(size, stride = 1L, on_overflow = "overwrite")
```

## Arguments

- size:

  Number of elements in the buffer, each of which will be `stride` bytes
  long.

- stride:

  Number of bytes per buffer element. Defaults to 1 byte. If you want to
  store anything other than a bytestream in the buffer, you will
  probably want more than one byte per element; for example, on most R
  platforms an integer takes 4 bytes and a double takes 8 (see
  [`.Machine`](https://rdrr.io/r/base/zMachine.html), and also
  [`ring_buffer_bytes_typed`](https://mrc-ide.github.io/ring/reference/ring_buffer_bytes_typed.md)).

- on_overflow:

  Behaviour on buffer overflow. The default is to overwrite the oldest
  elements in the buffer (`"overwrite"`). Alternative actions are
  `"error"` which will throw an error if a function tries to add more
  elements than there are space for, or `"grow"` which will grow the
  buffer to accept the new elements (this uses an approximately golden
  ratio approach; see details below).

## Details

In contrast with
[`ring_buffer_env`](https://mrc-ide.github.io/ring/reference/ring_buffer_env.md),
every element of this buffer has the same size; this makes it less
flexible (because you have to decide ahead of time what you will be
storing), but at the same time this can make using the buffer easier to
think about (because you decided ahead of time what you are storing).

If you want to use this to store fixed-size arrays of integers,
numerics, etc, see
[`ring_buffer_bytes_typed`](https://mrc-ide.github.io/ring/reference/ring_buffer_bytes_typed.md)
which wraps this with fast conversion functions.

If the `on_overflow` action is `"grow"` and the buffer overflows, then
the size of the buffer will grow geometrically (this is also the case if
you manually `$grow()` the buffer with `exact = FALSE`. When used this
way, let `n` is the number of *additional* elements that space is needed
for; `ring` then looks at the total needed capacity (used plus `n`
relative to `size()`). *If* the buffer needs to be made larger to fit
`n` elements in then it is grown by a factor of phi (the golden ratio,
approximately 1.6). So if to fit `n` elements in the buffer needs to be
increased in size by `m` then the smallest of `size * phi`,
`size * phi^2`, `size * phi^3`, ... will be used as the new size.

In contrast, using the `grow()` method with `exact = TRUE` will *always*
increase the size of the buffer so long as `n` is positive.

## Methods

Note that this methods reference section is repeated verbatim between
the three main ring buffer classes; `ring_buffer_env` ("env"),
`ring_buffer_bytes` ("bytes") and `ring_buffer_bytes_typed` ("typed").
Almost all methods have the same arguments and behaviour, but hopefully
by listing everything together, the differences between implementations
will be a bit more apparent.

- `reset`:

  Reset the state of the buffer. This "zeros" the head and tail pointer
  (and may or may not actually reset the data) so that the buffer can be
  used as if fresh.

  *Usage:* `reset(clear = FALSE)`

  *Arguments:*

  - `clear`: Logical, indicating if the memory should also be cleared.
    Generally this is not necessary, but with environment buffers this
    can let the garbage collector clean up large elements. For the bytes
    buffer this zeros the memory.

  *Return value*: Nothing; called for the side effect only.

- `duplicate`:

  Clone the ring buffer, creating a copy. Copies both the underlying
  data and the position of the head and tail.

  *Usage:* `duplicate()`

  *Return value*: A new ring buffer object

- `grow`:

  Increase the size of the buffer by `n` elements.

  *Usage:*

  - bytes, typed: `grow(n)`

  - env: `grow(n, exact = FALSE)`

  *Arguments:*

  - `n`: The number of additional elements that space should be reserved
    for (scalar non-negative integer).

        \item{\code{exact}:   (For bytes buffer only) Logical scalar indicating if growth should increase the size by \emph{exactly} \code{n} elements (if \code{TRUE}) or so that \emph{at least} \code{n} additional elements will fit (growing the buffer geometrically if needed).
        }

  *Return value*: Nothing; called for the side effect only.

- `size`:

  Return the capacity (maximum size) of the ring buffer

  *Usage:*

  - env: `size()`

  - bytes, typed: `size(bytes = FALSE)`

  *Arguments:*

  - `bytes`: (for `ring_buffer_bytes` only) Logical, indicating if the
    size should be returned in bytes (rather than logical entries, which
    is the default).

  *Return value*: A scalar integer

- `bytes_data`:

  Return the total size of the data storage used in this object.

  *Usage:*

  - env: *(not supported)*

  - bytes, typed: `bytes_data()`

  *Return value*: A scalar integer

- `stride`:

  Length of each element in the ring buffer, in bytes. Only implemented
  (and meaningful) for the bytes buffer; the environment buffer does not
  support this function as it makes no sense there.

  *Usage:*

  - env: *(not supported)*

  - bytes, typed: `stride()`

  *Return value*: A scalar integer

- `used`:

  Return the amount of space used in the ring buffer.

  *Usage:*

  - env: `used()`

  - bytes, typed: `used(bytes = FALSE)`

  *Arguments:*

  - `bytes`: (for `ring_buffer_bytes` only) Logical, indicating if the
    size should be returned in bytes (rather than logical entries, which
    is the default).

  *Return value*: A scalar integer

- `free`:

  Return the amount of space free in the ring buffer.

  *Usage:*

  - env: `free()`

  - bytes, typed: `free(bytes = FALSE)`

  *Arguments:*

  - `bytes`: (for `ring_buffer_bytes` only) Logical, indicating if the
    size should be returned in bytes (rather than logical entries, which
    is the default).

  *Return value*: A scalar integer

- `is_empty`:

  Test if the ring buffer is empty

  *Usage:* `is_empty()`

  *Return value*: A scalar logical

- `is_full`:

  Test if the ring buffer is full

  *Usage:* `is_full()`

  *Return value*: A scalar logical

- `head_pos`:

  Return the number of entries from the "start" of the ring buffer the
  head is. This is mostly useful for debugging.

  *Usage:*

  - env: `head_pos()`

  - bytes, typed: `head_pos(bytes = FALSE)`

  *Arguments:*

  - `bytes`: (for `ring_buffer_bytes` only) Logical, indicating if the
    position should be returned in bytes (rather than logical entries,
    which is the default).

  *Return value*: A scalar integer

- `tail_pos`:

  Return the number of entries from the "start" of the ring buffer the
  tail is. This is mostly useful for debugging.

  *Usage:*

  - env: `tail_pos()`

  - bytes, typed: `tail_pos(bytes = FALSE)`

  *Arguments:*

  - `bytes`: (for `ring_buffer_bytes` only) Logical, indicating if the
    position should be returned in bytes (rather than logical entries,
    which is the default).

  *Return value*: A scalar integer

- `head`:

  Return the contents of the head (the most recently written element in
  the ring buffer).

  *Usage:* [`head()`](https://rdrr.io/r/utils/head.html)

  *Return value*: It depends a little here. For `ring_buffer_env` this
  is a single R object. For `ring_buffer_bytes` it is a raw vector, the
  same length as the stride of the ring buffer. For
  `ring_buffer_bytes_typed`, a single R object that has been translated
  from raw.

- `tail`:

  Return the contents of the tail (the least recently written element in
  the ring buffer).

  *Usage:* [`tail()`](https://rdrr.io/r/utils/head.html)

  *Return value*: As for `head`

- `set`:

  Set a number of ring entries to the same value. The exact behaviour
  here varies depending on the type of ring buffer. This function may
  overflow the ring buffer; in this case the tail will be moved.

  *Usage:* `set(data, n)`

  *Arguments:*

  - `data`: The data to set each ring element to. For an environment
    buffer, this may be any R object. For a bytes buffer it may be
    either a single byte (in which case each ring element will be set to
    that byte, repeated `stride` times), or a raw vector of length
    `stride`.

        \item{\code{n}:   The number of entries to set to \code{data}
        }

  *Return value*: Invisibly returns the number of elements actually
  written (which may be less than `n` if the buffer overflows).
  Primarily called for its side effect.

- `push`:

  Push elements onto the ring buffer head. This may overflow the ring
  buffer, destroying the oldest elements in the buffer (and moving the
  position of the tail).

  *Usage:*

  - env: `push(data, iterate = TRUE)`

  - bytes, typed: `push(data)`

  *Arguments:*

  - `data`: Data to push onto the ring buffer. For `ring_buffer_bytes`,
    this must be a raw vector with a length that is a multiple of the
    buffer stride. For `ring_buffer_bytes_typed` it must be a vector of
    the appropriate type. For `ring_buffer_env` it may be an arbitrary R
    object (but see `iterate` .

        \item{\code{iterate}:   For \code{ring_buffer_env} only, changes the behaviour with vectors and lists.  Because each element of a \code{ring_buffer_env} can b an arbitrary R object, for a list \code{x} it is ambiguous if \code{push(x)} should push one object onto the buffer, or \code{length(x)} objects (i.e. equivalent to \code{push(x[[1]])}, \code{push(x[[2]])}, etc.  The \code{iterate} argument switches between interpretations; if \code{TRUE} (the default) the push will iterate over the object using \code{for (el in x)} (with appropriate S3 dispatch).  If \code{iterate = FALSE}, then the entire object is pushed at once, so always updating only by a single element.
        }

  *Return value*: For `ring_buffer_bytes`, the data invisibly. For
  `ring_buffer_bytes` and `ring_buffer_bytes_typed`, the position of the
  head pointer (relative to the beginning of the storage region).

- `take`:

  Destructively take elements from the ring buffer. This consumes from
  the tail (the least recently added elements). It is not possibly to
  underflow the buffer; if more elements are requested than can be
  supplied then an error will be thrown and the state of the buffer
  unmodified.

  *Usage:* `take(n)`

  *Arguments:*

  - `n`: The number of elements to take.

  *Return value*: For `ring_buffer_env` a `list` of `n` elements. For
  `ring_buffer_bytes`, a raw vector of `n * stride` bytes. For
  `ring_buffer_bytes_typed`, an vector of `n` elements of the storage
  mode of the ring.

- `read`:

  Nondestructively read elements from the ring buffer. This is identical
  to `take` except that the state of the buffer is not modified.

  *Usage:* `read(n)`

  *Arguments:*

  - `n`: The number of elements to read.

  *Return value*: For `ring_buffer_env` a `list` of `n` elements. For
  `ring_buffer_bytes`, a raw vector of `n * stride` bytes. For
  `ring_buffer_bytes_typed`, an vector of `n` elements of the storage
  mode of the ring.

- `copy`:

  Copy from *this* ring buffer into a different ring buffer. This is
  destructive with respect to both ring buffers; the tail pointer will
  be moved in this ring buffer as data are taken, and if the destination
  ring buffer overflows, the tail pointer will be moved too.

  *Usage:* `copy(dest, n)`

  *Arguments:*

  - `dest`: The destination ring buffer - will be modified by this call.

        \item{\code{n}:   The number of elements to copy
        }

- `mirror`:

  Mirror the contents of *this* ring buffer into a different ring
  buffer. This differs from `copy` in that *this* ring buffer is
  unaffected and in that *all* of this ring buffer is copied over
  (including head/tail positions). This provides an alternative way of
  duplicating state to `duplicate` if you already have an appropriately
  sized ring buffer handy. No allocations will be done.

  *Usage:* `mirror(dest)`

  *Arguments:*

  - `dest`: The destination ring buffer - will be modified by this call.

  *Return value*: Nothing; called for the side effect only.

- `head_offset`:

  Nondestructively read the contents of the `head` of the buffer, offset
  by `n` entries.

  *Usage:* `head_offset(n)`

  *Arguments:*

  - `n`: Head offset. This moves away from the most recently added item.
    An offset of 0 reads the most recently added element, 1 reads the
    element added before that.

  *Return value*: As for `head`

- `tail_offset`:

  Nondestructively read the contents of the `tail` of the buffer, offset
  by `n` entries.

  *Usage:* `tail_offset(n)`

  *Arguments:*

  - `n`: Tail offset. This moves away from the oldest item. An offset of
    0 reads the oldest element, 1 reads the element added after that.

  *Return value*: As for `tail` (see `head`)

- `take_head`:

  As for `take`, but operating on the head rather than the tail. This is
  destructive with respect to the head.

  *Usage:* `take_head(n)`

  *Arguments:*

  - `n`: Number of elements to take.

  *Return value*: As for `take`

- `read_head`:

  As for `read`, but operating on the head rather than the tail. This is
  not destructive with respect to the tail.

  *Usage:* `read_head(n)`

  *Arguments:*

  - `n`: Number of elements to read.

  *Return value*: As for `read`

- `head_set`:

  Set data to the head *without advancing*. This is useful in cases
  where the head data will be set and advanced separately (with
  `head_advance`). This is unlikely to be useful for all users. It is
  used extensively in dde (but called from C).

  *Usage:* `head_set(data)`

  *Arguments:*

  - `data`: Data to set into the head. For the bytes buffer this must be
    exactly `stride` bytes long, and for the environment buffer it
    corresponds to a single "element".

  *Return value*: Nothing; called for the side effect only.

- `head_data`:

  Retrieve the current data stored in the head but not advanced. For
  many cases this may be junk - if the byte buffer has looped then it
  will be the bytes that will be overwritten on the next write. However,
  when using `head_set` it will be the data that have been set into the
  buffer but not yet committed with `head_advance`.

  *Usage:* `head_data()`

  *Return value*: As for `head`

- `head_advance`:

  Shift the head around one position. This commits any data written by
  `head_set`.

  *Usage:* `head_advance()`

  *Return value*: Nothing; called for the side effect only.

## Examples

``` r
# Create a ring buffer of 100 bytes
b <- ring_buffer_bytes(100)

# Get the length, number of used and number of free bytes:
b$size()
#> [1] 100
b$used()
#> [1] 0
b$free()
#> [1] 100

# Nothing is used because we're empty:
b$is_empty()
#> [1] TRUE

# To work with a bytes buffer you need to use R's raw vectors;
# here are 30 random bytes:
bytes <- as.raw(as.integer(sample(256, 30, TRUE) - 1L))
bytes
#>  [1] cb 3e e4 65 6e 2e 9e c3 e5 c8 c4 04 fe 17 4e 4c 81 bd 36 aa 3d 2a 04 d4 2b
#> [26] bc 21 45 fe be

# Push these onto the bytes buffer:
b$push(bytes)
b$used()
#> [1] 30

# The head of the buffer points at the most recently added item
b$head()
#> [1] be
bytes[[length(bytes)]]
#> [1] be

# ...and the tail at the oldest (first added in this case)
b$tail()
#> [1] cb
bytes[[1]]
#> [1] cb

# Elements are taken from the tail; these will be the oldest items:
b$take(8)
#> [1] cb 3e e4 65 6e 2e 9e c3
bytes[1:8]
#> [1] cb 3e e4 65 6e 2e 9e c3
b$used()
#> [1] 22

# To read from the buffer without removing elements, use read:
b$read(8)
#> [1] e5 c8 c4 04 fe 17 4e 4c
bytes[9:16]
#> [1] e5 c8 c4 04 fe 17 4e 4c

# It is not possible to take or read more elements than are
# present in the buffer; it will throw an error:
if (FALSE) { # \dontrun{
b$read(50) # error because there are only 22 bytes present
} # }

# More elements can be pushed on:
b$push(as.raw(rep(0, 50)))
b$used()
#> [1] 72
b$read(b$used())
#>  [1] e5 c8 c4 04 fe 17 4e 4c 81 bd 36 aa 3d 2a 04 d4 2b bc 21 45 fe be 00 00 00
#> [26] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
#> [51] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00

# If many new elements are added, they will displace the old elements:
b$push(as.raw(1:75))
b$read(b$used())
#>   [1] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
#>  [26] 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 19
#>  [51] 1a 1b 1c 1d 1e 1f 20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f 30 31 32
#>  [76] 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f 40 41 42 43 44 45 46 47 48 49 4a 4b
```
