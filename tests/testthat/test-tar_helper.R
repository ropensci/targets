tar_test("tar_helper() with one line", {
  path <- tempfile()
  tar_helper(path, x <- 1)
  expect_equal(readLines(path), "x <- 1")
})

tar_test("tar_helper() with more than one line", {
  path <- tempfile()
  tar_helper(path, {
    x <- 1
    y <- 2
  })
  expect_equal(readLines(path), c("x <- 1", "y <- 2"))
})

tar_test("tar_helper() with tidy evaluation", {
  path <- tempfile()
  envir <- new.env(parent = baseenv())
  envir$y <- 1
  tar_helper(path, x <- !!y, tidy_eval = TRUE, envir = envir)
  expect_equal(readLines(path), "x <- 1")
})

tar_test("tar_helper() with tidy evaluation", {
  path <- tempfile()
  envir <- new.env(parent = baseenv())
  envir$y <- 1
  tar_helper(path, x <- !!y, tidy_eval = FALSE, envir = envir)
  expect_equal(readLines(path), "x <- !!y")
})
