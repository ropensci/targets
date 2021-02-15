tar_test("tar_exist_process()", {
  expect_false(tar_exist_process())
  dir_create(dirname(path_process()))
  file.create(path_process())
  expect_true(tar_exist_process())
})
