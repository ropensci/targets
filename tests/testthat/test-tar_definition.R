tar_test("tar_definition() automatic default", {
  expect_equal(tar_definition()$settings$name, "target_name")
})

tar_test("tar_definition() custom default", {
  x <- tar_target(custom_name, identity())
  expect_equal(tar_definition(default = x)$settings$name, "custom_name")
})

tar_test("tar_definition() in a pipeline", {
  skip_cran()
  tar_script(
    tar_target(
      x,
      tar_definition()$settings$memory,
      memory = "transient"
    )
  )
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_equal(tar_read(x), "transient")
})
