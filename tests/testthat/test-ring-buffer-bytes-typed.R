context("ring_buffer_bytes_typed (typed)")

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

    expect_identical(buf$tail_data(), x1)
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
