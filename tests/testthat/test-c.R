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
  file.remove(path)
  unlink("testing.Rcheck", recursive=TRUE)
})

test_that("standalone", {
  skip_on_cran()
  gcc <- Sys.which("gcc")
  if (!nzchar(gcc)) {
    skip("No gcc")
  }
  path <- system.file("include", package="ring")
  args <- c("-I", path, "-o", "ring_standalone", "ring_standalone.c")
  code <- system2(gcc, args)
  expect_equal(code, 0)
  code <- system2(normalizePath("ring_standalone"))
  expect_equal(code, 0)
  file.remove("ring_standalone")
})
