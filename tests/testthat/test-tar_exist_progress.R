tar_test("tar_exist_progress()", {
  expect_false(tar_exist_progress())
  dir_create(dirname(path_progress()))
  file.create(path_progress())
  expect_true(tar_exist_progress())
})
