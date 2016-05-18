context("ring_matrix")

test_that("basic use", {
  set.seed(1)
  nc <- 5L

  environment <- TRUE
  type <- "integer"

  for (environment in c(TRUE, FALSE)) {
    for (type in names(sizes)) {
      m <- ring_matrix(100, nc, type, environment)
      if (!environment) {
        expect_equal(m$buf$stride(), nc * sizes[[type]])
      }
      expect_equal(dim(m), c(0, 5))

      nr <- 3
      nn <- nr * m$nc
      pool <- switch(type,
                     logical=c(TRUE, FALSE, NA),
                     integer=as.integer(1:50),
                     double=rnorm(50),
                     complex=complex(real=rnorm(20), imaginary=rnorm(20)))

      dat <- matrix(sample(pool, nr * m$nc, TRUE), nr, m$nc)

      expect_equal(head(m), dat[integer(0), ])
      expect_equal(tail(m), dat[integer(0), ])

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

      expect_equal(dim(m), c(3, 5))
      expect_equal(head(m), dat)
      expect_equal(head(m, 1), dat[1,,drop=FALSE])
      expect_equal(tail(m), dat)
      expect_equal(tail(m, 1), dat[3,,drop=FALSE])

      expect_null(dimnames(m))
      expect_null(colnames(m))

      colnames(m) <- letters[1:nc]
      expect_equal(colnames(m), letters[1:nc])
      expect_equal(dimnames(m), list(NULL, letters[1:nc]))
      colnames(m) <- NULL
      expect_null(dimnames(m))
      expect_null(colnames(m))

      expect_error(rownames(m) <- letters[1:3],
                   "Cannot set rownames of a ring matrix")
      expect_error(dimnames(m) <- list(letters[1:3], letters[1:nc]),
                   "Cannot set rownames of a ring matrix")
    }
  }
})
