tar_test("set, get, exists, and unset target", {
  x <- runtime_init()
  expect_false(x$exists_target())
  expect_null(x$get_target())
  x$set_target(tar_target(x, 1))
  expect_true(x$exists_target())
  expect_true(inherits(x$get_target(), "tar_target"))
  x$unset_target()
  expect_false(x$exists_target())
  expect_null(x$get_target())
})

tar_test("set, get, exists, and unset frames", {
  x <- runtime_init()
  expect_false(x$exists_frames())
  expect_null(x$get_frames())
  x$set_frames(frames_init())
  expect_true(x$exists_frames())
  expect_true(is.environment(x$get_frames()))
  x$unset_frames()
  expect_false(x$exists_frames())
  expect_null(x$get_frames())
})

tar_test("validate null runtime", {
  x <- runtime_init()
  expect_silent(x$validate())
})

tar_test("validate non-null runtime", {
  x <- runtime_init(target = tar_target(x, 1), frames = frames_init())
  expect_silent(x$validate())
})

tar_test("invalidate bad runtime", {
  x <- runtime_init(target = 1, frames = frames_init())
  expect_error(x$validate(), class = "tar_condition_validate")
})
