tar_test("NULL name outside target", {
  expect_error(tar_timestamp(), class = "condition_validate")
})

tar_test("empty timestamp", {
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(out, tar_timestamp_default)
})

tar_test("empty file", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  unlink(path_objects("y"))
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(out, tar_timestamp_default)
})

tar_test("empty meta", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  unlink(path_meta())
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(out, tar_timestamp_default)
})

tar_test("good timestamp outside target", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_false(any(out == tar_timestamp_default))
})

tar_test("use timestamp in a target", {
  tar_script(
    list(
      tar_target(
        y, {
          tar_cancel(Sys.time() - tar_timestamp() < 360)
          tar_timestamp()
        },
        cue = tar_cue(mode = "always")
      )
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_read(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(out, tar_timestamp_default)
  progress <- tar_progress()
  expect_equal(progress$progress[progress$name == "y"], "built")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(progress$progress[progress$name == "y"], "canceled")
})
