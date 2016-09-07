context("ring_buffer_bytes_typed (typed)")

test_that("conversion functions are transitive", {
  vec <- list(logical=c(TRUE, FALSE, NA),
              integer=c(NA, as.integer(sample(10))),
              double=c(NA, NaN, runif(10)),
              complex=c(NA, NaN, complex(real=runif(10), imaginary=runif(10))))
  expect_equal(sort(names(vec)), sort(names(sizes)))

  for (nm in names(vec)) {
    x_r <- vec[[nm]]
    x_b <- convert_to[[nm]](x_r)
    expect_is(x_b, "raw")
    expect_equal(length(x_b), sizes[[nm]] * length(x_r))
    expect_identical(convert_from[[nm]](x_b), x_r)
  }
})

test_that("basic", {
  for (type in names(sizes)) {
    size <- 100
    n <- 7
    buf <- ring_buffer_bytes_typed(size, create[[type]](n))

    expect_is(buf, "ring_buffer_bytes_translate")
    expect_equal(buf$type, paste0("typed:", type))

    expect_equal(buf$size(), size)
    expect_equal(buf$stride(), sizes[[type]] * n)

    pool <- switch(type,
                   logical=c(TRUE, FALSE, NA),
                   integer=as.integer(1:50),
                   double=rnorm(50),
                   complex=complex(real=rnorm(20), imaginary=rnorm(20)))
    x1 <- sample(pool, n, TRUE)

    buf$push(x1)
    expect_equal(buf$used(), 1)
    expect_equal(buf$used(TRUE), n * sizes[[type]])

    expect_identical(buf$tail(), x1)
    expect_identical(buf$tail_offset(0), x1)

    expect_equal(buf$head_pos(), 1)
    expect_equal(buf$head_pos(TRUE), sizes[[type]] * n)

    x2 <- matrix(sample(pool, n * 3, TRUE), 3, byrow=FALSE)
    buf$push(x2)

    expect_identical(buf$read(4), c(x1, x2))

    p <- buf$head_pos(TRUE)

    x3 <- sample(pool, 2, TRUE)
    expect_error(buf$push(x3), "Incorrect size data")
    expect_identical(buf$head_pos(TRUE), p)
  }
})

test_that("initialisation", {
  b <- ring_buffer_bytes_typed(5, numeric(), 4L)
  expect_equal(b$size(), 5L)
  expect_equal(b$stride(), sizes[["double"]] * 4L)
  expect_equal(b$type, "typed:double")

  b <- ring_buffer_bytes_typed(5, "numeric", 4L)
  expect_equal(b$size(), 5L)
  expect_equal(b$stride(), sizes[["double"]] * 4L)
  expect_equal(b$type, "typed:double")

  expect_error(ring_buffer_bytes_typed(5, numeric()),
               "'len' must be greater than zero")
  expect_error(ring_buffer_bytes_typed(5, "double"),
               "'len' must be greater than zero")
  expect_error(ring_buffer_bytes_typed(5, 2, 5),
               "Invalid value for 'what'")
  expect_error(ring_buffer_bytes_typed(5, 2, "double"),
               "Invalid value for 'what'")

  expect_error(ring_buffer_bytes_typed(5, "numbers", 10),
               "'what' must be one of")
  expect_error(ring_buffer_bytes_typed(5, "character", 10),
               "'what' must be one of")
  expect_error(ring_buffer_bytes_typed(5, character(10)),
               "storage.mode(what) must be one of", fixed=TRUE)
})

test_that("translate", {
  b <- ring_buffer_bytes_translate(5, 8L, charToRaw, rawToChar)
  b$push("12345678")
  expect_equal(b$used(), 1L)
  expect_equal(b$tail(), "12345678")
})
