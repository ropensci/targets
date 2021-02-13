tar_test("tar_helper_raw() with one line", {
  path <- tempfile()
  tar_helper_raw(quote(x <- 1), path)
  expect_equal(readLines(path), "x <- 1")
})

tar_test("tar_helper_raw() with more than one line", {
  path <- tempfile()
  expr <- quote({
    x <- 1
    y <- 2
  })
  tar_helper_raw(expr, path)
  expect_equal(readLines(path), c("x <- 1", "y <- 2"))
})
