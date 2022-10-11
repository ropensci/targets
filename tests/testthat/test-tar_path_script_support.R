tar_test("tar_path_script() outside a pipeline", {
  script <- tar_config_get("script")
  expect_equal(
    tar_path_script_support(),
    paste0(tools::file_path_sans_ext(script), "_r")
  )
  tar_config_set(script = "example_script")
  expect_equal(tar_path_script_support(), "example_script_r")
})

tar_test("tar_path_script() inside a pipeline", {
  skip_cran()
  script <- "example_script"
  tar_script(tar_target(
    x,
    tar_path_script_support()),
    script = script,
    ask = FALSE
  )
  tar_make(script = script, callr_function = NULL)
  expect_equal(tar_read(x), "example_script_r")
})
