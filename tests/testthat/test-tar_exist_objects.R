tar_test("tar_exist_objects()", {
  expect_false(any(tar_exist_objects(c("x", "y"))))
  tar_script(
    list(
      tar_target(x, 1),
      tar_target(y, 2),
      tar_target(z, 3)
    )
  )
  tar_make(callr_function = NULL)
  tar_invalidate(x)
  tar_delete(y)
  expect_equal(tar_exist_objects(c("y", "x", "z")), c(FALSE, TRUE, TRUE))
  expect_equal(
    tar_exist_objects(c("y", "x", "z"), cloud = FALSE),
    c(FALSE, TRUE, TRUE)
  )
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, 1), script = "example/script.R")
  expect_false(tar_exist_objects("x", store = "example/store"))
  expect_false(file.exists("example/store"))
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(tar_exist_objects("x", store = "example/store"))
  expect_true(file.exists("example/store"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
