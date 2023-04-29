tar_test("set, get, exists, and unset target", {
  x <- runtime_init()
  x$target <- tar_target(x, 1)
  expect_true(is.environment(x$target))
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset frames", {
  x <- runtime_init()
  expect_null(x$frames)
  x$frames <- frames_init()
  expect_true(is.environment(x$frames))
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset interactive", {
  x <- runtime_init()
  expect_null(x$interactive)
  x$sinteractive <- TRUE
  expect_true(x$interactive)
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset script", {
  x <- runtime_init()
  expect_null(x$script)
  x$script <- "script"
  expect_equal(x$script, "script")
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset store", {
  x <- runtime_init()
  expect_null(x$store)
  x$sstore <- "store"
  expect_equal(x$store, "store")
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset working_directory", {
  x <- runtime_init()
  expect_null(x$working_directory)
  x$working_directory <- "working_directory"
  expect_equal(x$working_directory, "working_directory")
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset fun", {
  x <- runtime_init()
  expect_null(x$fun)
  x$fun <- "tar_make"
  expect_equal(x$fun, "tar_make")
  expect_silent(x$validate())
})

tar_test("set, get, exists, and unset gcp_auth", {
  x <- runtime_init()
  expect_null(x$gcp_auth)
  x$gcp_auth <- TRUE
  expect_true(x$gcp_auth)
  expect_silent(x$validate())
})

tar_test("validate null fields", {
  x <- runtime_init()
  expect_silent(x$validate())
})

tar_test("validate non-null runtime", {
  x <- runtime_init(
    target = tar_target(x, 1),
    frames = frames_init(),
    interactive = FALSE
  )
  expect_silent(x$validate())
})

tar_test("invalidate bad runtime", {
  x <- runtime_init(target = 1, frames = frames_init())
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_init(interactive = letters)
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_init(interactive = letters)
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("validate non-null script", {
  x <- runtime_init()
  x$script <- "script"
  expect_silent(x$validate())
})

tar_test("detect bad script", {
  x <- runtime_init()
  x$script <- FALSE
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("validate non-null store", {
  x <- runtime_init()
  x$store <- "store"
  expect_silent(x$validate())
})

tar_test("detect bad store", {
  x <- runtime_init()
  x$store <- FALSE
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("validate non-null fun", {
  x <- runtime_init()
  x$fun <- "tar_make"
  expect_silent(x$validate())
})

tar_test("detect bad fun", {
  x <- runtime_init()
  x$fun <- ""
  expect_error(x$validate(), class = "tar_condition_validate")
})
