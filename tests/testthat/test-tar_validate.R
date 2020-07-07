tar_test("tar_validate() on a good pipeline", {
  tar_script(tar_pipeline(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("tar_validate() works inside callr", {
  tar_script(tar_pipeline(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_arguments = list(show = FALSE)))
})
