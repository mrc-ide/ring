## ---
## title: "ring"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{ring}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo=FALSE, results="hide"
knitr::opts_chunk$set(error=FALSE)

## This package implements ring buffers.  A ring buffer can be used as
## a first-in-first-out (FIFO) buffer where the maximum size is known
## ahead of time.  Because they do not grow in size, they are useful
## to avoid using more and more memory as a process runs.  Because the
## data reading and writing happens in an (apparently) circular way,
## once data is added to the buffer it is not copied (in contrast if
## you used a vector then every time data is consumed you'd have to
## shuffle the vector around).

## `ring` implements two different ring buffers that will likely suit
## different applications.
##
## * `ring_buffer_bytes` is imlemented as a byte array in C, possibly
##   with a "stride" indicating a set of bytes that go together.  Once
##   the data reaches the end of the array we start writing to the
##   beginning again.
## * `ring_buffer_env` is implemented as a doubly linked list using
##   R's environments.  This buffer can hold arbitrary R objects at
##   each position.

## ## The environment buffer `ring_buffer_env`

## This is the simplest buffer to understand because we don't have to
## deal with raw vectors.

## To create a buffer that can hold up to 100 elements, use the
## `ring_buffer_env` function:
buf <- ring::ring_buffer_env(100)

## This is an [R6](https://github.com/wch/R6) class:
buf

## Operations on the class happen by running methods using `$`.  So
## the size of the buffer:
buf$size()

## ...the number of elements free and used:
buf$free()
buf$used()

## ...whether the buffer is empty or full:
buf$empty()
buf$full()

## To start using the buffer we need to put some data in it.  There
## are two functions for adding data:

## * `buf$set(data, n)` sets `n` elements to be the value `data`
## * `buf$push(data, iterate)` pushes `data` into the buffer, with the
##   `iterate` argument indicating if we should iterate over `data` or
##   treat it as a single element

## So to set the first 5 elements to be "a", "b", ..., "e", use:
buf$push(letters[1:5])

## The buffer is no longer empty
buf$empty()

## ...having 5 elements:
buf$used()

## ...and room for 95 more:
buf$free()

## To read the content of the buffer without modifying it, use
## `read(n)` where `n` is the number of elements to read.  This
## *always* returns a list of length `n`:
buf$read(1)
buf$read(2)

## If you try to read too far, then the buffer will underflow and you
## will get an error:
##+ error=TRUE
buf$read(20)

## If you just want the the first element, use `tail_data()`
buf$tail_data()

## The tail returns the first element in (so the buffer naturally
## operates as a first-in-first-out queue).

## You can also read the most recently added element with `head_data()`
buf$head_data()

## And you can offset these by an integer number of steps.  So moving
## one position into the buffer from the tail gets the second element
## added:
buf$tail_offset_data(1)

## or moving three elements into the buffer from the head (most
## recently added elemnt) gets the same bit of data
buf$head_offset_data(3)

## The above operations are all nondestructive -- they leave the
## buffer unchanged.  To consume elements, use `take(n)` which
## operates the same way as `read` but it also moves the buffer
## `tail`; it consumes elements leaving space for more.
buf$free()
buf$take(1)
buf$free()

## Now we have consumed an element the tail has moved along, so
## `tail_data` contains "b" and "a" is removed from the buffer:
buf$tail_data()

## To reset the buffer, use `reset()`.  This empies the buffer of all data:
buf$reset()
buf$used()
buf$empty()

## ## The bytes buffer `ring_buffer_bytes`

## This is the classical implementation of a ring buffer, and the
## implementation is largely due to
## [@dhess](https://github.com/dhess/c-ringbuf).

## This operates basically the same way as `ring_buffer_env` but with
## a few differences:

## * The contents of the buffer are raw bytes (R's raw vectors).
##   These are a bit fiddly to work with but can be very powerful.
## * The `iterate` distinction of `push` disappears because there is
##   no ambiguity with R objects
## * Reading from the head is not currently implemented.
## * There are a few methods that each implementation has that the
##   other does not but these are not needed for most uses.  These
##   will be documented in the reference documentation.

## To construct a buffer of 1000 bytes:
buf <- ring::ring_buffer_bytes(1000)

## Most of the same methods apply directly:
buf$free()
buf$used()
buf$full()
buf$empty()

## Generate a byte sequence:
bytes <- as.raw(0:255)

## ...and push them into the buffer:
buf$push(bytes)



## ### Striding

## ### The typed bytes buffer `ring_buffer_bytes_typed`

## ## Possible applications

## ### Ring matrix with `ring_matrix`
