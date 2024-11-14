targets::tar_test("tar_unblock_process()", {
  skip_cran()
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  expect_true(file.exists(path_process(path_store_default())))
  tar_unblock_process()
  expect_false(file.exists(path_process(path_store_default())))
})
