tar_test("tar_exist_objects()", {
  dir_create(dirname(path_objects("x")))
  file.create(path_objects("x"))
  expect_equal(tar_exist_objects(c("y", "x")), c(FALSE, TRUE))
})
