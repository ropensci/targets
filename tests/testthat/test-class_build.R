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

tar_test("error with no message", {
  build <- build_init(quote(stop()), baseenv())
  expect_null(build$object)
  expect_equal(build$metrics$error, ".")
  expect_true(is.character(build$metrics$traceback))
  expect_true(length(build$metrics$traceback) > 0L)
  expect_true(any(nzchar(build$metrics$traceback)))
})

tar_test("run with warning", {
  build <- expect_warning(
    build_init(quote(warning(12345)), baseenv()),
    regexp = "12345"
  )
  expect_true(any(grepl("12345", build$metrics$warnings)))
})

tar_test("warning with no message", {
  build <- expect_warning(build_init(quote(warning()), baseenv()))
  expect_equal(build$metrics$warnings, ".")
})

tar_test("load packages", {
  command_good <- command_init(quote(a <- b + c))
  expect_silent(command_validate(command_good))
  expect_silent(
    build_load_packages(command_good$packages, command_good$library)
  )
  command_bad <- command_init(quote(a <- b + c), packages = 123)
  expect_error(
    suppressWarnings(
      build_load_packages(command_bad$packages, command_bad$library)
    ),
    class = "tar_condition_validate"
  )
})

tar_test("validate good builds", {
  build <- build_init(quote(1L + 1L), baseenv())
  expect_silent(build_validate(build))
})

tar_test("validate builds with bad metrics", {
  build <- build_init(quote(1L + 1L), baseenv())
  build$metrics$seconds <- NULL
  expect_error(build_validate(build), class = "tar_condition_validate")
})
