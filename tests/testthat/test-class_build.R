tar_test("keep track of seconds", {
  build <- build_init(quote(1L + 1L), baseenv())
  out <- build$metrics$seconds
  expect_true(is.numeric(out))
  expect_equal(length(out), 1L)
})

tar_test("run without error", {
  build <- build_init(quote(1L + 1L), baseenv())
  expect_equal(build$object, 2L)
  expect_null(build$metrics$error)
  expect_null(build$metrics$traceback)
})

tar_test("run with error", {
  build <- build_init(quote(stop(12345)), baseenv())
  expect_null(build$object)
  expect_true(any(grepl("12345", build$metrics$error)))
  expect_true(any(grepl("12345", build$metrics$traceback)))
})

tar_test("run with warning", {
  build <- expect_warning(
    build_init(quote(warning(12345)), baseenv()),
    regexp = "12345"
  )
  expect_true(any(grepl("12345", build$metrics$warnings)))
})

tar_test("validate good builds", {
  build <- build_init(quote(1L + 1L), baseenv())
  expect_silent(build_validate(build))
})

tar_test("validate builds with bad metrics", {
  build <- build_init(quote(1L + 1L), baseenv())
  build$metrics$seconds <- NULL
  expect_error(build_validate(build), class = "condition_validate")
})
