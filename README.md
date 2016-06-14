# ring

> Ring Buffers

[![Linux Build Status](https://travis-ci.org/richfitz/ring.svg?branch=master)](https://travis-ci.org/richfitz/ring)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/ring?svg=true)](https://ci.appveyor.com/project/richfitz/ring)

Ring buffers in R and C.

There are two sorts of buffers implemented; a pure R ring buffer implemented as double linked list (using environments) that is genuinely ring, and one implemented in C that operates as an array of bytes.  Both can be used from R, and the C one can be used in other packages using R's `LinkingTo:` support.

## Installation

```r
devtools::install_github("richfitz/ring")
```

## Usage

See the vignette, either [online](https://richfitz.github.io/ring) or from the package as `vignette("ring")`.

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).
