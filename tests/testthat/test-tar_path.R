tar_test("tar_path() outside a pipeline with no arguments", {
  expect_true(is.character(tar_path()))
  expect_true(is.na(tar_path()))
  expect_equal(length(tar_path()), 1L)
  expect_equal(tar_path(default = "x"), "x")
})

tar_test("tar_path() with a name arg", {
  expect_equal(tar_path(x), file.path("_targets", "objects", "x"))
})

tar_test("tar_path() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_path()))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  path <- file.path("_targets", "objects", "x")
  expect_equal(target_read_value(x, pipeline)$object, path)
})

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists("example/store"))
  out <- tar_path(x, store = "example/store")
  expect_equal(out, "example/store/objects/x")
  expect_false(file.exists("example/store"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists("example/script.R"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
