context("C interface")

test_that("check package", {
  skip_on_cran()
  Sys.setenv("R_TESTS" = "")

  R <- file.path(R.home(), "bin", "R")
  res <- system2(R, c("CMD", "build", "testing"), stdout=FALSE, stderr=FALSE)
  path <- sprintf("testing_%s.tar.gz", "0.0.1")
  res <- system2(R, c("CMD", "check", "--no-manual", path),
                 stdout=TRUE, stderr=TRUE)
  expect_null(attr(res, "status"))
})
