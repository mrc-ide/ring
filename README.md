# ring

[![Linux Build Status](https://travis-ci.org/richfitz/ring.svg?branch=master)](https://travis-ci.org/richfitz/ring)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/ring?svg=true)](https://ci.appveyor.com/project/richfitz/ring)
[![codecov.io](https://codecov.io/github/richfitz/ring/coverage.svg?branch=master)](https://codecov.io/github/richfitz/ring?branch=master)

> Ring buffers in R and C

Ring buffers (or circular buffers) are like arrays that seem circular; data is written and read in a first-in-first-out (FIFO) style, but once allocated subsequent writes do not ever cause memory allocations.  Circular buffers are useful for collecting and processing data streams or for queues (with a fixed maximum size).  I use them to implement a solver for delay differential equations in [dde](https://github.com/richfitz/dde).

This package provides two implementations of ring buffers:

1. `ring_buffer_env`: a pure R ring buffer implemented as double linked list (using environments) that is genuinely a ring.
2. `ring_buffer_bytes`: a ring buffer implemented as an array of bytes with a pair of pointers (in C).  Each "element" of the array can be one or more bytes, but each must have a fixed size.  There are two convenience interfaces:
  * `ring_buffer_bytes_translate`: for cases where a raw->R and R->raw translation functions are provided
  * `ring_buffer_bytes_typed` for cases where each element should correspond to a fixed-length vector of one of R's core numeric-ish types (`logical`, `integer`, `numeric`, `complex`)

Both buffer types can be used from R, and `ring_buffer_bytes` can be used in other packages using R's `LinkingTo:` support.

Both buffer types will refuse to underflow (return elements beyond those that have been written) and behaviour on overflow can be controlled:

* overwrite old data: (the default), for a fixed-memory FIFO buffer
* grow buffer: expand the buffer geometrically to fit required elements
* throw error: refuse to overflow

See the documentation for details.

## Usage

See the vignette, either [online](https://richfitz.github.io/ring/vignettes/ring.html) or from the package as `vignette("ring")`.

## Installation

Requires a working C compiler (e.g., rtools on Windows, Xcode on a mac).

```r
devtools::install_github("richfitz/ring", upgrade=FALSE)
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).
