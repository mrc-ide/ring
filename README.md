# circle

> Circular Buffers

[![Linux Build Status](https://travis-ci.org/richfitz/circle.svg?branch=master)](https://travis-ci.org/richfitz/circle)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/circle?svg=true)](https://ci.appveyor.com/project/richfitz/circle)

Circular buffers in R and C.

There are two sorts of buffers implemented; a pure R circular buffer implemented as double linked list (using environments) that is genuinely circular, and one implemented in C that operates as an array of bytes.  Both can be used from R and I am looking for a way to make the C buffer available from other C packages without having to deal with dynamically loaded functions (being C rather than C++, being header-only is tricky but we might be able to fake it).

## Installation

```r
devtools::install_github("richfitz/circle")
```

## Usage

```r
library(circle)
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).
