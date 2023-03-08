tar_test("tar_path_script() outside a pipeline", {
  skip_if_not_installed("bit64")
  expect_equal(tar_path_script(), tar_config_get("script"))
  tar_config_set(script = "example_script")
  expect_equal(tar_path_script(), "example_script")
})

tar_test("tar_path_script() inside a pipeline", {
  skip_if_not_installed("bit64")
  script <- "example_script"
  tar_script(tar_target(x, tar_path_script()), script = script, ask = FALSE)
  tar_make(script = script, callr_function = NULL)
  expect_equal(tar_read(x), script)
})
