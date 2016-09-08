context("ring_vector")

test_that("basic use", {
  set.seed(1)

  for (environment in c(TRUE, FALSE)) {
    for (type in names(sizes)) {
      v <- ring_vector(100, type, environment)
      if (!environment) {
        expect_equal(v$buf$stride(), sizes[[type]])
      }
      expect_equal(length(v), 0)

      n <- 3
      pool <- switch(type,
                     logical=c(TRUE, FALSE, NA),
                     integer=as.integer(1:50),
                     double=rnorm(50),
                     complex=complex(real=rnorm(20), imaginary=rnorm(20)))
      dat <- sample(pool, n, TRUE)

      expect_equal(v[], dat[integer(0)])

      expect_equal(head(v), dat[integer(0)])
      expect_equal(tail(v), dat[integer(0)])

      ring_vector_push(v, dat)

      expect_equal(ring_vector_get(v), dat)
      expect_equal(ring_vector_get(v, 1:2), dat[1:2])

      expect_equal(v[], dat[])
      expect_equal(v[1:2], dat[1:2])
      expect_equal(v[1], dat[1])

      idx <- sample(n, n * 3, TRUE)
      expect_equal(v[idx], dat[idx])

      expect_equal(length(v), n)
      expect_null(dim(v))
      expect_equal(head(v), dat)
      expect_equal(head(v, 1), dat[1])
      expect_equal(tail(v), dat)
      expect_equal(tail(v, 1), dat[n])

      v2 <- c(v, dat)
      expect_identical(v, v2) # reference
      expect_equal(length(v), n * 2)
      expect_equal(v[], c(dat, dat))

      v3 <- c(v, dat[n:1], dat[n])
      expect_equal(v[], c(dat, dat, dat[n:1], dat[n]))

      ## This does not work as expected, but that's because 'c' has
      ## different dispatch rules to rbind.
      ##   expect_equal(c(dat, v), c(dat, v[]))
    }
  }
})

test_that("S3", {
  v <- ring_vector(100, "integer")
  push(v, seq_len(4))
  expect_equal(length(v), 4)
})
