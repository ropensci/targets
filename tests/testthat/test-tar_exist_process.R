tar_test("tar_exist_process()", {
  expect_false(tar_exist_process())
  dir_create(dirname(path_process(path_store_default())))
  file.create(path_process(path_store_default()))
  expect_true(tar_exist_process())
})

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, 1), script = "example/script.R")
  expect_false(tar_exist_process(store = "example/store"))
  expect_false(file.exists("example/store"))
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(tar_exist_process(store = "example/store"))
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
