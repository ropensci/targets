tar_test("tar_exist_meta()", {
  expect_false(tar_exist_meta())
  dir_create(dirname(path_meta()))
  file.create(path_meta())
  expect_true(tar_exist_meta())
})
