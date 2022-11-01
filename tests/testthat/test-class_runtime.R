tar_test("set, get, exists, and unset target", {
  x <- runtime_init()
  expect_false(x$exists_target())
  expect_null(x$get_target())
  x$set_target(tar_target(x, 1))
  expect_true(x$exists_target())
  expect_true(inherits(x$get_target(), "tar_target"))
  expect_silent(x$validate())
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
  expect_silent(x$validate())
  x$unset_frames()
  expect_false(x$exists_frames())
  expect_null(x$get_frames())
})

tar_test("set, get, exists, and unset interactive", {
  x <- runtime_init()
  expect_false(x$exists_interactive())
  expect_null(x$get_interactive())
  x$set_interactive(TRUE)
  expect_true(x$exists_interactive())
  expect_true(x$get_interactive())
  expect_silent(x$validate())
  x$unset_interactive()
  expect_false(x$exists_interactive())
  expect_null(x$get_interactive())
})

tar_test("set, get, exists, and unset script", {
  x <- runtime_init()
  expect_false(x$exists_script())
  expect_null(x$get_script())
  x$set_script("script")
  expect_true(x$exists_script())
  expect_equal(x$get_script(), "script")
  expect_silent(x$validate())
  x$unset_script()
  expect_false(x$exists_script())
  expect_null(x$get_script())
})

tar_test("set, get, exists, and unset store", {
  x <- runtime_init()
  expect_false(x$exists_store())
  expect_null(x$get_store())
  x$set_store("store")
  expect_true(x$exists_store())
  expect_equal(x$get_store(), "store")
  expect_silent(x$validate())
  x$unset_store()
  expect_false(x$exists_store())
  expect_null(x$get_store())
})

tar_test("set, get, exists, and unset fun", {
  x <- runtime_init()
  expect_false(x$exists_fun())
  expect_null(x$get_fun())
  x$set_fun("tar_make")
  expect_true(x$exists_fun())
  expect_equal(x$get_fun(), "tar_make")
  expect_silent(x$validate())
  x$unset_fun()
  expect_false(x$exists_fun())
  expect_null(x$get_fun())
})

tar_test("set, get, exists, and unset gcp_auth", {
  x <- runtime_init()
  expect_false(x$exists_gcp_auth())
  expect_false(x$get_gcp_auth())
  x$set_gcp_auth(TRUE)
  expect_true(x$exists_gcp_auth())
  expect_true(x$get_gcp_auth())
  expect_silent(x$validate())
  x$unset_gcp_auth()
  expect_false(x$exists_gcp_auth())
  expect_false(x$get_gcp_auth())
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
  x$set_script("script")
  expect_silent(x$validate())
})

tar_test("detect bad script", {
  x <- runtime_init()
  x$set_script(FALSE)
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("validate non-null store", {
  x <- runtime_init()
  x$set_store("store")
  expect_silent(x$validate())
})

tar_test("detect bad store", {
  x <- runtime_init()
  x$set_store(FALSE)
  expect_error(x$validate(), class = "tar_condition_validate")
})

tar_test("validate non-null fun", {
  x <- runtime_init()
  x$set_fun("tar_make")
  expect_silent(x$validate())
})

tar_test("detect bad fun", {
  x <- runtime_init()
  x$set_fun("")
  expect_error(x$validate(), class = "tar_condition_validate")
})
