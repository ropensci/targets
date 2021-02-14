tar_test("tar_helper_raw() with one line", {
  path <- tempfile()
  tar_helper_raw(path, quote(x <- 1))
  expect_equal(readLines(path), "x <- 1")
})

tar_test("tar_helper_raw() with nonexistent parent dir", {
  path <- file.path(tempfile(), "x", "y", "z")
  tar_helper_raw(path, quote(x <- 1))
  expect_equal(readLines(path), "x <- 1")
})

tar_test("tar_helper_raw() with more than one line", {
  path <- tempfile()
  expr <- quote({
    x <- 1
    y <- 2
  })
  tar_helper_raw(path, expr)
  expect_equal(readLines(path), c("x <- 1", "y <- 2"))
})
