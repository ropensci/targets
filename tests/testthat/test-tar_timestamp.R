tar_test("NULL name outside target", {
  expect_error(tar_timestamp(), class = "tar_condition_validate")
})

tar_test("empty timestamp", {
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(as.numeric(out), as.numeric(file_time_reference))
})

tar_test("empty file", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  unlink(path_objects("y"))
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(as.numeric(out), as.numeric(file_time_reference))
})

tar_test("empty meta", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  unlink(path_meta())
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(as.numeric(out), as.numeric(file_time_reference))
})

tar_test("good timestamp outside target", {
  tar_script(tar_target(y, 1))
  tar_make(callr_function = NULL)
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_false(any(as.numeric(out) == as.numeric(file_time_reference)))
})

tar_test("one timestamp for two files", {
  tar_script(
    tar_target(
      y, {
        file.create(c("x", "y"))
        c("x", "y")
      }
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_read(y), c("x", "y"))
  expect_true(all(file.exists(c("x", "y"))))
  out <- tar_timestamp(y)
  expect_true(inherits(out, "POSIXct"))
  expect_false(any(as.numeric(out) == as.numeric(file_time_reference)))
  expect_equal(length(out), 1L)
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
  expect_identical(as.numeric(out), as.numeric(file_time_reference))
  progress <- tar_progress()
  expect_equal(progress$progress[progress$name == "y"], "built")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(progress$progress[progress$name == "y"], "canceled")
})
