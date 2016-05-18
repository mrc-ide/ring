context("ring_matrix")

test_that("basic use", {
  set.seed(1)
  nc <- 5L
  for (environment in c(TRUE, FALSE)) {
    for (type in names(sizes)) {
      m <- ring_matrix(100, nc, type, environment)
      if (!environment) {
        expect_equal(m$buf$stride(), nc * sizes[[type]])
      }

      nr <- 3
      nn <- nr * m$nc
      pool <- switch(type,
                     logical=c(TRUE, FALSE, NA),
                     integer=as.integer(1:50),
                     double=rnorm(50),
                     complex=complex(real=rnorm(20), imaginary=rnorm(20)))

      dat <- matrix(sample(pool, nr * m$nc, TRUE), nr, m$nc)

      ring_matrix_push(m, dat)

      expect_equal(ring_matrix_get(m), dat)
      expect_equal(ring_matrix_get(m, 1:2), dat[1:2, , drop=FALSE])

      expect_equal(m[], dat[])
      expect_equal(m[, ], dat[, ])
      expect_equal(m[1:2, ], dat[1:2, ])
      expect_equal(m[1, ], dat[1, ])
      expect_equal(m[1, , drop=FALSE], dat[1, , drop=FALSE])
      expect_equal(m[, 1], dat[, 1])
      expect_equal(m[, 1, drop=FALSE], dat[, 1, drop=FALSE])

      idx <- cbind(sample(nrow(dat), nc, TRUE),
                   sample(ncol(dat), nc, TRUE))
      expect_equal(m[idx], dat[idx])
    }
  }
})
