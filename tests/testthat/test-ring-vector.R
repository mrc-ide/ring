context("ring_vector")

test_that("basic use", {
  set.seed(1)

  for (environment in c(TRUE, FALSE)) {
    for (type in names(sizes)) {
      m <- ring_vector(100, type, environment)
      if (!environment) {
        expect_equal(m$buf$stride(), sizes[[type]])
      }
      expect_equal(length(m), 0)

      n <- 3
      pool <- switch(type,
                     logical=c(TRUE, FALSE, NA),
                     integer=as.integer(1:50),
                     double=rnorm(50),
                     complex=complex(real=rnorm(20), imaginary=rnorm(20)))
      dat <- sample(pool, n, TRUE)

      expect_equal(m[], dat[integer(0)])

      expect_equal(head(m), dat[integer(0)])
      expect_equal(tail(m), dat[integer(0)])

      ring_vector_push(m, dat)

      expect_equal(ring_vector_get(m), dat)
      expect_equal(ring_vector_get(m, 1:2), dat[1:2])

      expect_equal(m[], dat[])
      expect_equal(m[1:2], dat[1:2])
      expect_equal(m[1], dat[1])

      idx <- sample(n, n * 3, TRUE)
      expect_equal(m[idx], dat[idx])

      expect_equal(length(m), n)
      expect_null(dim(m))
      expect_equal(head(m), dat)
      expect_equal(head(m, 1), dat[1])
      expect_equal(tail(m), dat)
      expect_equal(tail(m, 1), dat[n])

      m2 <- c(m, dat)
      expect_identical(m, m2) # reference
      expect_equal(length(m), n * 2)
      expect_equal(m[], c(dat, dat))

      m3 <- c(m, dat[n:1], dat[n])
      expect_equal(m[], c(dat, dat, dat[n:1], dat[n]))

      ## This does not work as expected, but that's because 'c' has
      ## different dispatch rules to rbind.
      ##   expect_equal(c(dat, m), c(dat, m[]))
    }
  }
})
