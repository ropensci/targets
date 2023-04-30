tar_test("target", {
  x <- runtime_new()
  x$target <- tar_target(x, 1)
  expect_true(is.environment(x$target))
  expect_silent(runtime_validate(x))
})

tar_test("frames", {
  x <- runtime_new()
  expect_null(x$frames)
  x$frames <- frames_init()
  expect_true(is.environment(x$frames))
  expect_silent(runtime_validate(x))
})

tar_test("interactive", {
  x <- runtime_new()
  expect_null(x$interactive)
  x$interactive <- TRUE
  expect_true(x$interactive)
  expect_silent(runtime_validate(x))
})

tar_test("script", {
  x <- runtime_new()
  expect_null(x$script)
  x$script <- "script"
  expect_equal(x$script, "script")
  expect_silent(runtime_validate(x))
})

tar_test("store", {
  x <- runtime_new()
  expect_null(x$store)
  x$store <- "store"
  expect_equal(x$store, "store")
  expect_silent(runtime_validate(x))
})

tar_test("working_directory", {
  x <- runtime_new()
  expect_null(x$working_directory)
  x$working_directory <- "working_directory"
  expect_equal(x$working_directory, "working_directory")
  expect_silent(runtime_validate(x))
})

tar_test("fun", {
  x <- runtime_new()
  expect_null(x$fun)
  x$fun <- "tar_make"
  expect_equal(x$fun, "tar_make")
  expect_silent(runtime_validate(x))
})

tar_test("gcp_auth", {
  x <- runtime_new()
  expect_null(x$gcp_auth)
  x$gcp_auth <- TRUE
  expect_true(x$gcp_auth)
  expect_silent(runtime_validate(x))
})

tar_test("file_exist", {
  x <- runtime_new()
  expect_null(x$file_exist)
  x$file_exist <- counter_init()
  expect_true(is.environment(x$file_exist))
  expect_silent(runtime_validate(x))
})

tar_test("file_info", {
  x <- runtime_new()
  expect_null(x$file_info)
  tmp <- tempfile()
  file.create(tmp)
  x$file_info <- file.info(tmp, extra_cols = FALSE)
  expect_true(is.data.frame(x$file_info))
  expect_silent(runtime_validate(x))
})

tar_test("file_info_exist", {
  x <- runtime_new()
  expect_null(x$file_info_exist)
  tmp <- tempfile()
  file.create(tmp)
  x$file_info_exist <- counter_init()
  expect_true(is.environment(x$file_info_exist))
  expect_silent(runtime_validate(x))
})

tar_test("validate null fields", {
  x <- runtime_new()
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null runtime", {
  x <- runtime_new(
    target = tar_target(x, 1),
    frames = frames_init(),
    interactive = FALSE
  )
  expect_silent(runtime_validate(x))
})

tar_test("invalidate bad runtime", {
  x <- runtime_new(target = 1, frames = frames_init())
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_new(interactive = letters)
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_new(interactive = letters)
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null script", {
  x <- runtime_new()
  x$script <- "script"
  expect_silent(runtime_validate(x))
})

tar_test("detect bad script", {
  x <- runtime_new()
  x$script <- FALSE
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null store", {
  x <- runtime_new()
  x$store <- "store"
  expect_silent(runtime_validate(x))
})

tar_test("detect bad store", {
  x <- runtime_new()
  x$store <- FALSE
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null fun", {
  x <- runtime_new()
  x$fun <- "tar_make"
  expect_silent(runtime_validate(x))
})

tar_test("detect bad fun", {
  x <- runtime_new()
  x$fun <- ""
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})
