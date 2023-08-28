tar_test("tar_meta_delete() works on a local pipeline", {
  skip_cran()
  tar_script(tar_target(x, 1))
  expect_null(tar_meta_delete())
  tar_make(callr_function = NULL)
  expect_null(tar_meta_delete())
})
